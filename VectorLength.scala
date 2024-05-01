import leon.lang._
import leon.annotation._
import leon.lang.synthesis._

import java.math.BigInteger;

object VectorLength {

  def syn_sqrt(n: BigInt): BigInt = {
    require(n >= 0)
    choose { (res: BigInt) =>
      res * res <= n && (res + 1) * (res + 1) > n
    }
  }
  // synthesized solution
  def sqrt(n: BigInt): BigInt = {
    require(n >= 0)
    if(n == 0) {
      0
    } else {
      var y = BigInt(1)
      while(y*y <= n) {
        y = y + 1
      }
      y - 1
    }
  }

  case class Vector(h: BigInt, v: BigInt) {
    def +(newVec: Vector): Vector = Vector(h + newVec.h, v + newVec.v)
    def -(newVec: Vector): Vector = Vector(h - newVec.h, v - newVec.v)

    def length: BigInt
       = {
      // writing out individually to reason about overflow
      val h2 = h * h
      val v2 = v * v
      val sum = h2 + v2
      sqrt(sum)
    }
  }

  def timeToHit(speed: BigInt, initd: Vector, endd: Vector) : BigInt = {
    val dist = (endd - initd)
    if (speed == 0) {
      BigInt(-1)
    }else{
      choose { (res: BigInt) =>
        (dist.length <= res * speed) && (dist.length >= (res - 1) * speed)
      }
    }
  }
}