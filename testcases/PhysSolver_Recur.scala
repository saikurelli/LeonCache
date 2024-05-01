import leon.lang._
import leon.lang.synthesis._
import leon.collection._ // for List
import scala.collection.JavaConversions._ // to try to fix foreach not a member error
import leon.io.{
  FileInputStream => FIS,
  FileOutputStream => FOS,
  StdOut
}


object PhvsicsSolver_Recur{
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

  val timestep_cost = BigInt(2)

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
            // indicate we
            None

          }
        } else {
          checkCacheForVar(t, variable, cacheTypeIndex + 1)
        }
      }
    }
  }
  // TODO: return answer based on total cache storage (instead of just our variables)
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

  def sqrt(x : Int) : Int = {
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
    def length: Int = sqrt(h * h + v * v)
  }

  def timeToHit(speed: Int, initd: Vector, endd: Vector, cache: List[List[List[String]]]) : OptionInt = {
    val dist = endd - initd
    if (speed != 0) {
      val time = dist.length / speed
      findCacheCostOverTimeStep(cache, "speed") match {
        case Some(v) => Some(v)
        case None => None
      }
    }else{
      None
    }
  }

  // check that returned value is smaller than or equal to following variables (cache in order)

  def optimizedCache(cache: List[List[List[String]]], variable: String) : Boolean = {
    val cacheCost = findCacheCostOverTimeStep(cache, variable)
    cacheCost match {
      case Some(v) => {
        true
      }
      case None => false
    }
  }
  // def genCache(speed: Int, initd: Vector, endd: Vector) : List[List[List[String]]] = {
  //   choose{ (x: List[List[List[String]]]) =>
  //     checkCacheSizesOverTimeStep(x) &&
  //      timeToHit(speed, initd, endd, x) == None ||
  //      timeToHit(speed, initd, endd, x) == Some(1 + 14 + 14 + 200)}
  // }
  def genCache(speed: Int, initd: Vector, endd: Vector) : List[List[List[String]]] = {
    var cache = List(List(List("endd"), List("initd", "dist"), List("speed")))
    cache
  }ensuring { res =>
    checkCacheSizesOverTimeStep(res) && optimizedCache(res, "speed") && optimizedCache(res, "initd") && optimizedCache(res, "endd") && optimizedCache(res, "dist")
  }
}