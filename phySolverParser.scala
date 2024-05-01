
// meant to be run with scala directly (use processBuilder in PhysSolver_Recur.scala)
object phySolverParser extends App{
  // return vars in code
  def codeParser(code: Array[String]): List[String] = {

    var code_indx = 0
    // for a car travelling at _speed_, how long will it take to travel from _initd_ to _endd_

    var total = 0
    var result = List[String]()

    // parsing logic - should this be moved?
    while (code_indx < code.length){
      val line = code(code_indx)
      val rhs = line.split("=")(1).trim
      val lhs = line.split("=")(0).trim
      // val (lhs, rhs) = line.split("=").map(x => x.trim)

      // group together all letters that are not separated by a () or . (. = access length aspect that doesn't impact cache according to design decision)
      val pattern = """[a-zA-Z0-9\ \+\-\*\/\.]+""".r
      val segments = pattern.findAllIn(rhs).filter(x => x.trim() != "").toList

      for(segment <- segments){
        val ContainsOperator: Boolean = segment.contains("+") || segment.contains("-") || segment.contains("*") || segment.contains("/")
        lazy val containsLength = segment.contains(".")
        if (ContainsOperator){
            val vars = segment.split(" ").filter(x => x != "+" && x != "-" && x != "*" && x != "/")

            for(varName <- vars){
              result = varName :: result
            }

        }else if(segment != ".length"){
          println("Error: Unrecognized segment" + segment)
          return result
        }
      }
      code_indx += 1
      }
      result
    }

  // main method to test the solver
  // val speed = 10
  // val initd = Vector(0, 0)
  // val endd = Vector(10, 10)
  // cache:= [[[endd],[initd, dist],[speed]]]
  // = 1 + 14 + 14 + 200
  // cache:= [[[endd],[initd, dist],[]], [[speed],[],[]]]
  // = 1 + 14 + 14 + 2* 1 =
  // val cache = List(List(List("endd"), List("initd", "dist"), List("speed")))
  // println(cacheTimeToHit(cache))
  // val cache2 = List(
  //   List(List("endd"), List("initd", "dist"), List()),
  //   List(List("speed"), List(), List()))
  // println(cacheTimeToHit(cache2))
  // val cache3 = List(
  //   List(List("endd"), List(), List()),
  //   List(List("initd"), List(), List()),
  //   List(List("speed"), List(), List()),
  //   List(List("dist"), List(), List())
  // )
  // println(cacheTimeToHit(cache3))
  // val cache4 = List(
  //   List(List("endd"), List("initd"), List()),
  //   List(List("speed"), List(), List()),
  //   List(List("dist"), List(), List())
  // )
  // println(cacheTimeToHit(cache4))
  val code = Array("val dist = (endd - initd).length", "val result = dist / speed")
  println(codeParser(code))

  // Ignore below - old version of code
  // nanoSec map of cache tvpe to time for fetch (scaled bv 2 for integer) (L1 -> 0, L2 -> 1, RAM -> 2 )
  val cacheTiming = Map(
    0 -> 1,
    1 -> 14,
    2 -> 200
  )

  // represent how many integers can be stored in each cache
  val cacheSize = Map(
    0 -> 1,
    1 -> 2,
    2 -> 10
  )

  val timestep_cost = 3

  // cache structure: [[[L1_0], [L2_0], [RAM_0]], [[L1_1], [L2_1], [RAM_1]], ...] where _i is the cache at timestep i

  // Used for preconditions (checkCacheSizes & minimize time with calcCacheTime)
  // def checkCacheSizes(cache: List[List[List[String]]]) : Boolean = {
  //   for(timestep <- cache){
  //     for(cacheType <- 0 until timestep.length){
  //       val cache = timestep(cacheType)
  //       if(cache.length > cacheSize(cacheType)){
  //         return false
  //       }
  //     }
  //   }
  //   true
  // }

  // def calcCacheTime(cache: List[List[List[String]]]) : Int = {
  //   var time = 0
  //   for(timestep <- cache){
  //     for(cacheType <- 0 until timestep.length){
  //       val cache = timestep(cacheType)
  //       time += cache.length * cacheTiming(cacheType)
  //     }
  //   }
  //   time
  // }

  // case class Vector(h: Int, v: Int) {
  //   def +(newVec: Vector): Vector = Vector(h + newVec.h, v + newVec.v)
  //   def *(c: Int): Vector = Vector(h * c, v * c)
  //   def -(newVec: Vector): Vector = Vector(h - newVec.h, v - newVec.v)
  //   def /(c: Int): Vector = Vector(h / c, v / c)
  //   def length: Double = Math.sqrt(h * h + v * v)
  // }

  // def findValue(cache: List[List[List[String]]], varName: String): Option[Int] = {
  //   for(cacheStepIdx <- 0 until cache.length){
  //     val cacheStep = cache(cacheStepIdx)
  //     for(cacheType <- 0 until cacheStep.length){
  //       val cache: List[String] = cacheStep(cacheType)
  //       for(value <- cache){
  //         if(value == varName){
  //           //TODO:  edit cache to actually return value instead of varName
  //           // return Some(value)
  //           val time = cacheTiming(cacheType)
  //           return Some(Math.max(time, time * Math.pow(timestep_cost, cacheStepIdx).intValue()))
  //         }
  //       }
  //     }
  //   }
  //   None
  // }


  // for a car travelling at _speed_, how long will it take to travel from _initd_ to _endd_
  // def cacheTimeToHit(cache: List[List[List[String]]]): Double = {
  //   var code_indx = 0
  //   val code = Array("val dist = (endd - initd).length", "val result = dist / speed").reverse

  //   var total = 0

  //   // parsing logic - should this be moved?
  //   while (code_indx < code.length){
  //     val line = code(code_indx)
  //     val rhs = line.split("=")(1).trim
  //     val lhs = line.split("=")(0).trim

  //     // group together all letters that are not separated by a () or . (. = access length aspect that doesn't impact cache according to design decision)
  //     val pattern = """[a-zA-Z0-9\ \+\-\*\/\.]+""".r
  //     val segments = pattern.findAllIn(rhs).filter(x => x.trim() != "").toList

  //     for(segment <- segments){
  //       val ContainsOperator: Boolean = segment.contains("+") || segment.contains("-") || segment.contains("*") || segment.contains("/")
  //       lazy val containsLength = segment.contains(".")
  //       if (ContainsOperator){
  //           val vars = segment.split(" ").filter(x => x != "+" && x != "-" && x != "*" && x != "/")

  //           for(varName <- vars){
  //             val value = findValue(cache, varName)
  //             value match {
  //               case Some(value) => total += value
  //               // come back and add early exit to avoid return
  //               case None => {
  //                 println("Error: Unfound variable in segment " + segment + " for "+ varName)
  //                 return Double.PositiveInfinity
  //               }
  //             }
  //           }

  //       }else if(segment != ".length"){
  //         println("Error: Unrecognized segment" + segment)
  //         return Double.PositiveInfinity
  //       }
  //     }
  //     code_indx += 1
  //   }
  //   return total;
  // }
}