val list = List(32,115,23,1,52,28)


def min(ints : List[Int]) : Int = {
     def minAccum(ints: List[Int], theMin: Int) : Int = {
     ints match{
     case Nil => theMin
     case x :: tail =>
     val newMin = if(x > theMin) theMin else x
     minAccum(tail,newMin)
     }
     }
     minAccum(ints,ints.head)
     }


  def max(ints: List[Int]): Int = { 
    def maxAccum(ints: List[Int], theMax: Int): Int = {
      ints match {
        case Nil => theMax
        case x :: tail =>
          val newMax = if (x > theMax) x else theMax
          maxAccum(tail, newMax)
      }
    }
    maxAccum(ints, ints.head)
  }

def distBetweenMaxAndmin(ints: List[Int]) : Int = {max(ints)-min(ints)}
