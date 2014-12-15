/* Copyright 2009-2014 EPFL, Lausanne */

package leon
package repair

import purescala.Common._
import purescala.Definitions._
import purescala.Trees._
import purescala.TreeOps._
import purescala.TypeTrees._
import purescala.DefOps._
import purescala.Constructors._
import purescala.ScalaPrinter
import evaluators._
import solvers._
import utils._
import solvers.z3._
import codegen._
import verification._
import synthesis._
import synthesis.rules._
import synthesis.heuristics._
import graph.DotGenerator
import leon.utils.ASCIIHelpers.title

class Repairman(ctx: LeonContext, initProgram: Program, fd: FunDef, verifTimeoutMs: Option[Long]) {
  val reporter = ctx.reporter

  var program = initProgram

  implicit val debugSection = DebugSectionRepair

  def repair() = {
    reporter.info(ASCIIHelpers.title("1. Discovering tests for "+fd.id))
    val (passingTests, failingTests) = discoverTests

    reporter.info(f" - Passing: ${passingTests.size}%3d")
    reporter.info(f" - Failing: ${failingTests.size}%3d")

    reporter.info(ASCIIHelpers.title("2. Locating/Focusing synthesis problem"))
    val synth = getSynthesizer(passingTests, failingTests)
    val p     = synth.problem

    var solutions = List[Solution]()

    reporter.info(ASCIIHelpers.title("3. Synthesizing"))
    reporter.info(p)

    synth.synthesize() match {
      case (search, sols) =>
        for (sol <- sols) {

          // Validate solution if not trusted
          if (!sol.isTrusted) {
            reporter.info("Found untrusted solution! Verifying...")
            val (npr, fds) = synth.solutionToProgram(sol)

            getVerificationCounterExamples(fds.head, npr) match {
              case Some(ces) =>
                reporter.error("I ended up finding this counter example:\n"+ces.mkString("  |  "))

              case None =>
                solutions ::= sol
                reporter.info("Solution was not trusted but verification passed!")
            }
          } else {
            reporter.info("Found trusted solution!")
            solutions ::= sol
          }
        }

        if (synth.settings.generateDerivationTrees) {
          val dot = new DotGenerator(search.g)
          dot.writeFile("derivation"+DotGenerator.nextId()+".dot")
        }

        if (solutions.isEmpty) {
          reporter.error(ASCIIHelpers.title("Failed to repair!"))
        } else {
          reporter.info(ASCIIHelpers.title("Repair successful:"))
          for ((sol, i) <- solutions.zipWithIndex) {
            reporter.info(ASCIIHelpers.subTitle("Solution "+(i+1)+":"))
            val expr = sol.toSimplifiedExpr(ctx, program)
            reporter.info(ScalaPrinter(expr));
          }
        }
      }
  }

  def getSynthesizer(passingTests: List[Example], failingTests: List[Example]): Synthesizer = {
    // Create a fresh function
    val nid = FreshIdentifier(fd.id.name+"_repair").copiedFrom(fd.id)
    val nfd = new FunDef(nid, fd.tparams, fd.returnType, fd.params, fd.defType)
    nfd.copyContentFrom(fd)
    nfd.copiedFrom(fd)

    nfd.body = nfd.body.map { b => preMap({
      case fi @ FunctionInvocation(TypedFunDef(`fd`, tps), args) =>
        Some(FunctionInvocation(TypedFunDef(nfd, tps), args).copiedFrom(fi))
      case _ => None
    })(b)}

    program = program.addDefinition(nfd, fd)

    val body = nfd.body.get;

    val (newBody, replacedExpr) = focusRepair(program, nfd, passingTests, failingTests)
    nfd.body = Some(newBody)

    val guide = guideOf(replacedExpr)

    // Return synthesizer for this choose
    val soptions0 = SynthesisPhase.processOptions(ctx);

    val soptions = soptions0.copy(
      functionsToIgnore = soptions0.functionsToIgnore + fd + nfd,
      costModel = RepairCostModel(soptions0.costModel),
      rules = (soptions0.rules ++ Seq(
        //GuidedDecomp,
        //GuidedCloser,
        CEGLESS,
        TEGLESS
      )) diff Seq(ADTInduction)
    );

    // extract chooses from nfd
    val Seq(ci) = ChooseInfo.extractFromFunction(ctx, program, nfd, soptions)

    val nci = ci.copy(pc = and(ci.pc, guideOf(replacedExpr)))

    val p = nci.problem

    new Synthesizer(ctx, nfd, program, p, soptions)
  }

  private def guideOf(expr: Expr): Expr = {
    val gfd = program.library.guide.get.typed(Seq(expr.getType))
    FunctionInvocation(gfd, Seq(expr))
  }

  private def focusRepair(program: Program, fd: FunDef, passingTests: List[Example], failingTests: List[Example]): (Expr, Expr) = {

    reporter.ifDebug { printer =>
      printer(title("Tests failing are: "))
      failingTests.foreach { ex =>
        printer(ex.ins.mkString(", "))
      }
    }
    reporter.ifDebug { printer =>
      printer(title("Tests passing are: "))
      passingTests.foreach { ex =>
        printer(ex.ins.mkString(", "))
      }
    }
  

    val pre = fd.precondition.getOrElse(BooleanLiteral(true))
    val args = fd.params.map(_.id)
    val argsWrapped = tupleWrap(args.map(_.toVariable))

    val out = fd.postcondition.map(_._1).getOrElse(FreshIdentifier("res", true).setType(fd.returnType))

    val spec = fd.postcondition.map(_._2).getOrElse(BooleanLiteral(true))

    val body = fd.body.get

    val choose = Choose(List(out), spec)

    val evaluator = new DefaultEvaluator(ctx, program)

    val minimalFailingTests = {
      // We don't want tests whose invocation will call other failing tests.
      // This is because they will appear erroneous, 
      // even though the error comes from the called test
      val testEval : CollectingEvaluator = new CollectingEvaluator(ctx, program){
        def collecting(e : Expr) : Option[Seq[Expr]] = e match {
          case fi@FunctionInvocation(TypedFunDef(`fd`, _), args) => 
            Some(args)
          case _ => None
        }
      }
      
      val passingTs = for (test <- passingTests) yield InExample(test.ins)
      val failingTs = for (test <- failingTests) yield InExample(test.ins)
      
      val test2Tests : Map[InExample, Set[InExample]] = (failingTs ++ passingTs).map{ ts => 
        testEval.eval(body, args.zip(ts.ins).toMap)
        (ts, testEval.collected map (InExample(_)))
      }.toMap
      
      val recursiveTests : Set[InExample] = test2Tests.values.toSet.flatten -- (failingTs ++ passingTs)
      
      val testsTransitive : Map[InExample,Set[InExample]] = 
        leon.utils.GraphOps.transitiveClosure[InExample](
          test2Tests ++ recursiveTests.map ((_,Set[InExample]()))
        )
          
      val knownWithResults : Map[InExample, Boolean] = (failingTs.map((_, false)).toMap) ++ (passingTs.map((_,true)))
      
      val recWithResults : Map[InExample, Boolean] = recursiveTests.map { ex =>
        (ex, evaluator.eval(spec, (args zip ex.ins).toMap + (out -> body)) match {
          case EvaluationResults.Successful(BooleanLiteral(true))  => true
          case _ => false
        })
      }.toMap

      val allWithResults = knownWithResults ++ recWithResults
      
      
      testsTransitive.collect { 
        case (rest, called) if !allWithResults(rest) && (called forall allWithResults) => 
          rest
      }.toSet
    }
    
    reporter.ifDebug { printer =>
      printer(title("MinimalTests are: "))
      minimalFailingTests.foreach { ex =>
        printer(ex.ins.mkString(", "))
      }
    }

    // Check how an expression behaves on tests
    //  - returns Some(true) if for all tests e evaluates to true
    //  - returns Some(false) if for all tests e evaluates to false
    //  - returns None otherwise
    def forAllTests(e: Expr, env: Map[Identifier, Expr]): Option[Boolean] = {
      val results = minimalFailingTests.map { ex =>
        val ins = ex.ins
        evaluator.eval(e, env ++ (args zip ins)) match {
          case EvaluationResults.Successful(BooleanLiteral(true))  => Some(true)
          case EvaluationResults.Successful(BooleanLiteral(false)) => Some(false)
          case _ => None
        }
      }

      if (results.size == 1) {
        results.head
      } else {
        None
      }
    }

    def focus(expr: Expr, env: Map[Identifier, Expr]): (Expr, Expr) = expr match {
      case me @ MatchExpr(scrut, cases) =>
        
        // in case scrut is an non-variable expr, we simplify to a variable + inject in env
        val perCase = for (c <- cases) yield {
          val cond = and(conditionForPattern(scrut, c.pattern, includeBinders = false),
                         c.optGuard.getOrElse(BooleanLiteral(true)))
          val map  = mapForPattern(scrut, c.pattern)


          forAllTests(cond, env ++ map) match {
            case Some(false) =>
              // We know this case is correct
              None
              
            case Some(true) | None =>
              // Either incorrect case or unknown, treat it as incorrect
              val (b, r) = focus(c.rhs, env ++ map)
              Some((c.copy(rhs = b), r))           
          }
        }
        
        perCase count { _.isDefined } match {
          // No wrong cases
          case 0 => ctx.reporter.internalError("No erroneous case found!")
          // 1 wrong case 
          case 1 => 
            val e = perCase.collect{ case Some((b,r)) => r }.head
            val newCases = cases zip perCase map {
              case (cs, None) => 
                cs
              case (_ , Some((b,r))) => 
                b
            }
            (MatchExpr(scrut, newCases), e)
          // More wrong cases, return a choose on the top-level
          case _ => (choose, me)
        }
        
      case Let(id, value, body) =>
        val (b, r) = focus(body, env + (id -> value))
        (Let(id, value, b), r)

      case IfExpr(c, thn, els) =>
        forAllTests(c, env) match {
          case Some(true) =>
            val (b, r) = focus(thn, env)
            (IfExpr(c, b, els), r)
          case Some(false) =>
            val (b, r) = focus(els, env)
            (IfExpr(c, thn, b), r)
          case None =>
            (choose, expr)
        }

      case _ =>
        (choose, expr)
    }

    focus(body, Map())
  }

  def getVerificationCounterExamples(fd: FunDef, prog: Program): Option[Seq[InExample]] = {
    val timeoutMs = verifTimeoutMs.getOrElse(3000L)
    val solver = new TimeoutSolverFactory(SolverFactory.getFromSettings(ctx, prog), timeoutMs)
    val vctx = VerificationContext(ctx, prog, solver, reporter)
    val vcs = AnalysisPhase.generateVerificationConditions(vctx, Some(List(fd.id.name)))

    AnalysisPhase.checkVerificationConditions(vctx, vcs)

    var invalid = false;
    var ces = List[Seq[Expr]]()

    for (vc <- vcs.getOrElse(fd, List())) {
      if (vc.value == Some(false)) {
        invalid = true;

        vc.counterExample match {
          case Some(m) =>
            ces = fd.params.map(vd => m(vd.id)) :: ces;

          case _ =>
        }
      }
    }
    if (invalid) {
      Some(ces.map(InExample(_)))
    } else {
      None
    }
  }


  def discoverTests: (List[Example], List[Example]) = {

    import bonsai._
    import bonsai.enumerators._
    import utils.ExpressionGrammars.ValueGrammar
    import purescala.Extractors.UnwrapTuple

    val maxEnumerated = 1000
    val maxValid      = 100

    val evaluator = new CodeGenEvaluator(ctx, program, CodeGenParams(checkContracts = true))
    val enum      = new MemoizedEnumerator[TypeTree, Expr](ValueGrammar.getProductions _)

    val inputs = enum.iterator(tupleTypeWrap(fd.params map { _.getType})).map{ case UnwrapTuple(is) => is }

    val filtering: Seq[Expr] => Boolean = fd.precondition match {
      case None =>
        _ => true
      case Some(pre) =>
        evaluator.compile(pre, fd.params map { _.id }) match {
          case Some(evalFun) =>
            val sat = EvaluationResults.Successful(BooleanLiteral(true));
            { (e: Seq[Expr]) => evalFun(e) == sat }
          case None =>
            { _ => false }
        }
    }

    val inputsToExample: Seq[Expr] => Example = { ins =>
      evaluator.eval(FunctionInvocation(fd.typed(fd.tparams.map(_.tp)), ins)) match {
        case EvaluationResults.Successful(res) =>
          new InOutExample(ins, List(res))
        case _ =>
          new InExample(ins)
      }
    }

    val generatedTests = inputs
      .take(maxEnumerated)
      .filter(filtering)
      .take(maxValid)
      .map(inputsToExample)
      .toList

    val (generatedPassing, generatedFailing) = generatedTests.partition {
      case _: InOutExample => true
      case _               => false
    }

    // Try to verify, if it fails, we have at least one CE
    val ces = getVerificationCounterExamples(fd, program) getOrElse Nil 

    // Extract passing/failing from the passes in POST
    val ef = new ExamplesFinder(ctx, program)
    val (userPassing, userFailing) = ef.extractTests(fd)

    (generatedPassing ++ userPassing, generatedFailing ++ ces ++ userFailing)
  }


  // ununsed for now, but implementation could be useful later
  private def disambiguate(p: Problem, sol1: Solution, sol2: Solution): Option[(InOutExample, InOutExample)] = {
    val s1 = sol1.toSimplifiedExpr(ctx, program)
    val s2 = sol2.toSimplifiedExpr(ctx, program)

    val e = new DefaultEvaluator(ctx, program)

    def unwrap(e: Expr) = if (p.xs.size > 1) {
      val Tuple(es) = e
      es
    } else {
      Seq(e)
    }

    if (s1 == s2) {
      None
    } else {
      val diff = and(p.pc, not(Equals(s1, s2)))
      val solver = (new FairZ3Solver(ctx, program) with TimeoutSolver).setTimeout(1000)

      solver.assertCnstr(diff)
      solver.check match {
        case Some(true) =>
          val m = solver.getModel
          val inputs = p.as.map(id => m.getOrElse(id, simplestValue(id.getType)))
          val inputsMap = (p.as zip inputs).toMap

          (e.eval(s1, inputsMap), e.eval(s2, inputsMap)) match {
            case (EvaluationResults.Successful(r1), EvaluationResults.Successful(r2)) =>
              Some((InOutExample(inputs, unwrap(r1)), InOutExample(inputs, unwrap(r2))))
            case _ =>
              None
          }
        case Some(false) =>
          None
        case _ =>
          // considered as equivalent
          None
      }
    }
  }
}