import leon.lang._
import leon.lang.synthesis._
import leon.collection._ // for List
import scala.collection.JavaConversions._ // to try to fix foreach not a member error



object PhvsicsSolver{
  // nanoSec map of cache tvpe to time for fetch (scaled bv 2 for integer) (L1 -> 0, L2 -> 1, RAM -> 2 )
  val cacheTiming : Map[BigInt, BigInt] = Map(
    BigInt(0) -> BigInt(1),
    BigInt(1) -> BigInt(14),
    BigInt(2) -> BigInt(200)
  )

  // represent how many integers can be stored in each cache
  val cacheSize : Map[BigInt, BigInt] = Map(
    BigInt(0) -> BigInt(1),
    BigInt(1) -> BigInt(2),
    BigInt(2) -> BigInt(10)
  )

  val timestep_cost = BigInt(3)

  // cache structure: [[[L1_0], [L2_0], [RAM_0]], [[L1_1], [L2_1], [RAM_1]], ...] where _i is the cache at timestep i

  def checkCacheSizes(cacheAtStep: List[List[String]], i: BigInt) : Boolean = {
    cacheAtStep match {
      case Nil() => true
      case Cons(cacheType, t) => {
        if( !cacheSize.isDefinedAt(i) || cacheType.length > cacheSize(i)){
          false
        } else {
          checkCacheSizes(t, i + 1)
        }
      }
    }
  }


  // General checkCache Function - recursively checks all timesteps
  def checkCacheSizesOverTimeStep(cache: List[List[List[String]]]) : Boolean = {
    cache match {
      case Nil() => true
      case Cons(timestep, t) => {
        checkCacheSizes(timestep, 0) && checkCacheSizesOverTimeStep(t)
      }
    }
  }
  // represent not being able to find a value
  sealed abstract class OptionInt
  case class Some(v : BigInt) extends OptionInt
  case object None extends OptionInt

  def checkCacheForVar(cacheAtStep: List[List[String]], variable: String, cacheTypeIndex: BigInt) : OptionInt = {
    cacheAtStep match {
      case Nil() => None
      case Cons(cacheType, t) => {
        if(cacheType.contains(variable)){
          if(cacheTiming.contains(cacheTypeIndex)){
            Some(cacheTiming(cacheTypeIndex))
          }else{
            // indicate invalid cache location
            None
          }
        } else {
          checkCacheForVar(t, variable, cacheTypeIndex + 1)
        }
      }
    }
  }
  def findCacheCostOverTimeStep(cache: List[List[List[String]]], variable: String) : OptionInt = {
    cache match {
      case Nil() => None
      case Cons(timestep, t) => {
        checkCacheForVar(timestep, variable, 0) match {
          case Some(v) => Some(v)
          case None => {
            findCacheCostOverTimeStep(t, variable) match {
              case Some(v) => Some(v * timestep_cost)
              case None => None
            }
          }
        }
      }
    }
  }

  def isqrt(x : Int) : Int = {
    choose { (y : Int) =>
      y * y <= x && (y + 1) * (y + 1) >= x
    }
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
    def length: Int = isqrt(h * h + v * v)
  }

  def cacheTimeToHit(speed: Int, initd: Vector, endd: Vector, cache: List[List[List[String]]]) : OptionInt = {
    // variables that would need to be looped over to compute a cache - result: [speed, initd, endd, dist]
    // var variables = "scala phySolverParser.scala".!!.split("\n").toList
    // commenting out to avoid running shell command
    var speed = findCacheCostOverTimeStep(cache, "speed")
    var initd = findCacheCostOverTimeStep(cache, "initd")
    var endd = findCacheCostOverTimeStep(cache, "endd")
    var dist = findCacheCostOverTimeStep(cache, "dist")
    (speed, initd, endd, dist) match {
      case (Some(s), Some(i), Some(e), Some(d)) => {
        Some(s + i + e + d)
      }
      case _ => None
    }
  }


  // check that returned value is smaller than or equal to following variables (cache in order)

  def checkInCache(cache: List[List[List[String]]], variable: String) : Boolean = {
    val cacheCost = findCacheCostOverTimeStep(cache, variable)
    cacheCost match {
      case Some(v) => {
        true
      }
      case None => false
    }
  }
  def pow(base: BigInt, exp: BigInt) : BigInt = {
    if(exp == 0){
      1
    } else {
      base * pow(base, exp - 1)
    }
  }

  def checkUpgrade(cache: List[List[List[String]]], current_timestep: BigInt) : Boolean = {
    if(cache.size > 0){
      val cacheType = cache.head
      if(cacheType.length > 2 && cacheTiming.contains(BigInt(2)) && cacheTiming.contains(BigInt(0))){
        val L3_time = cacheTiming(BigInt(2))
        val L1_time = cacheTiming(BigInt(0))
        // if there are any L3 caches, they should be upgraded to L1 at the end if (timestep_cost ** cache.length - current_timestep) * L3_time > L1_time
        if(pow(timestep_cost, cache.length - current_timestep) * L1_time < L3_time){
          false
        } else {
          true
        }
      } else {
        true
      }
    } else {
      true
    }
  }

  def optimizedCache(cache: List[List[List[String]]]) : Boolean = {
    checkUpgrade(cache, 0)
  }

  // def findOptimal(cacheList: List[List[List[String]]]) : Double = {
  //   var sampleCache = List(List(List("endd"), List("initd", "dist"), List("speed")))
  //   codeParser(sampleCache)
  //   sampleCache
  // }ensuring { res =>
  //   checkCacheSizesOverTimeStep(res) && checkInCache(res, "speed") && checkInCache(res, "initd") && checkInCache(res, "endd") && checkInCache(res, "dist") && optimizedCache(res)
  // }


  // def checkNoUpgrades(cache: List[List[List[String]]], timeToBeat: BigInt, preprend: List[List[List[String]]], speed: Int, initd: Vector, endd: Vector) : Boolean = {
  //   cache match {
  //     case Nil() => true
  //     case Cons(timestep, t) => {
  //       timestep match {
  //         case Nil() => true
  //         case Cons(l1, Cons(l2, Cons(l3, Nil()))) => {
  //           // if there's an elem in l3 or l2, try adding to next timestep's l1 (if not full or next timestep doesn't exist)
  //           l3 match {
  //             case Nil() => true
  //             case
  //           }

  //           if(l3.length > 0){
  //             if(t != Nil() && t.head.head.length < cacheSize(0)){
  //               if(timeToHit(speed, initd, endd, prepend :: Cons(l1, Cons(l2, Cons(l3.tail, Nil()))) :: (t.head.head ++ l3.head) :: t.head.tail:: t.tail) > timeToBeat){
  //                 false
  //               }

  //               ) > timeToBeat){
  //                 return false
  //               }
  //             } else if
  //             }
  //           } else if(l2.length > 0){
  //             if(t.length > 0 && t.head.length < cacheSize(1)){
  //               t = Cons(l2, t.head) :: t.tail
  //             } else {
  //               t = Cons(l2, Nil()) :: t
  //             }
  //           }

  //         }
  //       }
  //     }
  //   }

  // check that returned value is smaller than or equal to following variables (cache in order)

  def genCache(speed: Int, initd: Vector, endd: Vector) : List[List[List[String]]] = {
    var cache = List(List(List("endd"), List("initd", "dist"), List("speed")))
    // codeParser(cache) - moved logic to timeToHit
    if(checkCacheSizesOverTimeStep(cache) && checkInCache(cache, "speed") && checkInCache(cache, "initd") && checkInCache(cache, "endd") && optimizedCache(cache)){
      cache
    } else {
      List(List(List[String]()))
    }
    cache
  }ensuring { res =>
    (res.size == List(List(List[String]())).size) ||
    (checkCacheSizesOverTimeStep(res) && checkInCache(res, "speed") && checkInCache(res, "initd") && checkInCache(res, "endd") && checkInCache(res, "dist") && optimizedCache(res))
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
  // println(cacheTimeToHit(speed, initd, endd, cache))
  // val cache2 = List(
  //   List(List("endd"), List("initd", "dist"), List()),
  //   List(List("speed"), List(), List()))
  // println(cacheTimeToHit(speed, initd, endd, cache2))
}
