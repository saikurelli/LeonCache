package leon.synthesis.search

abstract class AndOrGraphSearch[AT <: AOAndTask[S],
                                OT <: AOOrTask[S],
                                S <: AOSolution](val g: AndOrGraph[AT, OT, S]) {

  var processing = Set[g.Leaf]()

  def nextLeaves(k: Int): List[g.Leaf] = {
    import scala.math.Ordering.Implicits._

    case class WL(t: g.Leaf, costs: List[Int])

    var leaves = List[WL]()

    def collectFromAnd(at: g.AndTree, costs: List[Int]) {
      val newCosts = at.minCost.value :: costs
      at match {
        case l: g.Leaf =>
          collectLeaf(WL(l, newCosts.reverse)) 
        case a: g.AndNode =>
          for (o <- (a.subProblems -- a.subSolutions.keySet).values) {
            collectFromOr(o, newCosts)
          }
      }
    }

    def collectFromOr(ot: g.OrTree, costs: List[Int]) {
      val newCosts = ot.minCost.value :: costs

      ot match {
        case l: g.Leaf =>
          collectLeaf(WL(l, newCosts.reverse))
        case o: g.OrNode =>
          for (a <- o.alternatives.values) {
            collectFromAnd(a, newCosts)
          }
      }
    }

    def collectLeaf(wl: WL) {
      if (!processing(wl.t)) {
        leaves = wl :: leaves
      }
    }

    collectFromOr(g.tree, Nil)

    leaves.sortBy(_.costs).map(_.t)
  }

  def nextLeaf(): Option[g.Leaf] = nextLeaves(1).headOption

  abstract class ExpandResult[T <: AOTask[S]]
  case class Expanded[T <: AOTask[S]](sub: List[T]) extends ExpandResult[T]
  case class ExpandSuccess[T <: AOTask[S]](sol: S) extends ExpandResult[T]
  case class ExpandFailure[T <: AOTask[S]]() extends ExpandResult[T]

  var continue = true

  def stop() {
    continue = false
  }

  def search(): Option[S]

  def onExpansion(al: g.AndLeaf, res: ExpandResult[OT]) {
    res match {
      case Expanded(ls) =>
        al.expandWith(ls)
      case r @ ExpandSuccess(sol) =>
        al.solution = Some(sol)
        al.parent.notifySolution(al, sol)
      case _ =>
        al.isUnsolvable = true
        al.parent.unsolvable(al)
    }
    processing -= al
  }

  def onExpansion(ol: g.OrLeaf, res: ExpandResult[AT]) {
    res match {
      case Expanded(ls) =>
        ol.expandWith(ls)
      case r @ ExpandSuccess(sol) =>
        ol.solution = Some(sol)
        ol.parent.notifySolution(ol, sol)
      case _ =>
        ol.isUnsolvable = true
        ol.parent.unsolvable(ol)
    }
    processing -= ol
  }
}