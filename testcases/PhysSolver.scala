import leon.lang._
import leon.lang.synthesis._
import leon.collection._ // for List
import scala.collection.JavaConversions._ // to try to fix foreach not a member error



object PhvsicsSolver{
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

  val timestep_cost = 2

  // cache structure: [[[L1_0], [L2_0], [RAM_0]], [[L1_1], [L2_1], [RAM_1]], ...] where _i is the cache at timestep i

  // Used for preconditions (checkCacheSizes & minimize time with calcCacheTime)
  def checkCacheSizes(cache: List[List[List[String]]]) : Boolean = {
    var blength: BigInt = cache.length

    var length = blength.intValue
    for(timestep_indx <- 0 until length){
      val timestep = cache(timestep_indx)
      for(cacheType <- 0 until timestep.length.toInt){
        val cache = timestep(cacheType)
        if(cache.length.toInt > cacheSize(cacheType).toInt){
          return false
        }
      }
    }
    true
  }

  def calcCacheTime(cache: List[List[List[String]]]) : Int = {
    var time = 0
    for(timestep_idx <- 0 until cache.length.toInt){
      val timestep = cache(timestep_idx)
      for(cacheType <- 0 until timestep.length.toInt){
        val cache = timestep(cacheType)
        time += cache.length.toInt * cacheTiming(cacheType)
      }
    }
    time
  }

  case class Vector(h: Int, v: Int) {
    def +(newVec: Vector): Vector = Vector(h + newVec.h, v + newVec.v)
    def *(c: Int): Vector = Vector(h * c, v * c)
    def -(newVec: Vector): Vector = Vector(h - newVec.h, v - newVec.v)
    def /(c: Int): Vector = {
      if (c == 0) {
        Vector(-1, -1)
      }else{
        Vector(h / c, v / c)
      }
    }
    def length: Double = Math.sqrt(h * h + v * v)
  }

  def findValue(cache: List[List[List[String]]], varName: String): Option[Int] = {
    for(cacheStepIdx <- 0 until cache.length.toInt){
      val cacheStep = cache(cacheStepIdx)
      for(cacheType <- 0 until cacheStep.length.toInt){
        val cache: List[String] = cacheStep(cacheType)
        for(valueIdx <- 0 until cache.length.toInt){
          val value = cache(valueIdx)
          if(value == varName){
            //TODO:  edit cache to actually return value instead of varName
            // return Some(value)
            val time = cacheTiming(cacheType)
            return Some(Math.max(time, time * timestep_cost * cacheStepIdx))
          }
        }
      }
    }
    None()
  }


//  def parser(code: Array[String]) := {
//  extract code to here later
//  }

  // for a car travelling at _speed_, how long will it take to travel from _initd_ to _endd_
  def timeToHit(speed: Int, initd: Vector, endd: Vector, cache: List[List[List[String]]]): Int = {
    var code_indx = 0
    val code = Array("val dist = (endd - initd).length", "val result = dist / speed").reverse

    var total = 0

    // parsing logic - should this be moved?
    while (code_indx < code.length){
      val line = code(code_indx)
      val rhs = line.split("=")(1).trim
      val lhs = line.split("=")(0).trim

      // group together all letters that are not separated by a () or . (. = access length aspect that doesn't impact cache according to design decision)
      val pattern = """[a-zA-Z0-9\ \+\-\*\/\.]+""".r
      val segments = pattern.findAllIn(rhs).filter(x => x.trim() != "").toList

      for(segment <- segments){
        val ContainsOperator: Boolean = segment.contains("+") || segment.contains("-") || segment.contains("*") || segment.contains("/")
        lazy val containsLength = segment.contains(".")
        if (ContainsOperator){
            val vars = segment.split(" ").filter(x => x != "+" && x != "-" && x != "*" && x != "/")

            for(varName <- vars){
              val value = findValue(cache, varName)
              value match {
                case Some(value) => total += value
                // come back and add early exit to avoid return
                case None() => {
                  println("Error: Unfound variable in segment " + segment + " for "+ varName)
                  return Int.MaxValue
                }
              }
            }

        }else if(segment != ".length"){
          println("Error: Unrecognized segment" + segment)
          return Int.MaxValue
        }
      }
      code_indx += 1
    }
    return total;
  }
  def genCache(speed: Int, initd: Vector, endd: Vector) : List[List[List[String]]] = {
    choose{ (x: List[List[List[String]]]) =>
       timeToHit(speed, initd, endd, x) == 1 + 14 + 14 + 200}
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
  // println(timeToHit(speed, initd, endd, cache))
  // val cache2 = List(
  //   List(List("endd"), List("initd", "dist"), List()),
  //   List(List("speed"), List(), List()))
  // println(timeToHit(speed, initd, endd, cache2))

}