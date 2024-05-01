import leon.lang._
import leon.lang.synthesis._
import leon.collection._ // for List
import scala.collection.JavaConversions._


object test  {
  // def condition(list: List[BigInt], matchList: List[BigInt]) : Boolean = {
  //   list match {
  //     case Nil() => {
  //       matchList match {
  //         case Nil() => true
  //         case Cons(h2, t2) => false
  //       }
  //     }
  //     case Cons(h, t)=> {
  //       matchList match {
  //         case Cons(h2, t2) => {
  //           if(h != h2) {
  //             false
  //           } else {
  //             condition(t, t2)
  //           }
  //         }
  //         case Nil() => false
  //       }
  //     }
  //   }
  // }


  // def main (elem1: BigInt, elem2: BigInt) : List[BigInt] = {
  //   choose { (res: List[BigInt]) =>
  //     condition(res, List(elem1, elem2))
  //   }
  // }

  sealed abstract class VariableObject
  case object speed extends VariableObject
  case object initd extends VariableObject
  case object endd extends VariableObject
  case object dist extends VariableObject

  def testList(list: List[VariableObject], variable: VariableObject) : Boolean = {
    list match {
      case Nil() => false
      case Cons(h, t) => {
        if(h == variable) {
          true
        } else {
          testList(t, variable)
        }
      }
    }
  }
  def mainListTest (elem1: VariableObject, elem2: VariableObject, elem3: VariableObject, elem4: VariableObject) : List[VariableObject] = {
    choose { (res: List[VariableObject]) =>
      (testList(res, elem1) && testList(res, elem2) && testList(res, elem3)) && testList(res, elem4)
    }
  }
}
