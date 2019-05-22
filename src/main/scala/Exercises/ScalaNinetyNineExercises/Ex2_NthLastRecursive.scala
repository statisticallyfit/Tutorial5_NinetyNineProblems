package ScalaNinetyNineExercises

//import org.scalatest.Assertions._
/**
  *
  */
object Ex2_NthLastRecursive {

     def lastNthRecursive[A](list: List[A], n: Int): Option[A] = {

          def innerLastNthR(count: Int,
                            resultList: List[A],
                            currentList: List[A]): Option[A] =

               currentList match {
                    case Nil if count > 0 => None
                    case Nil              => Some(resultList.head)
                    case _ :: more => {
                         //debugging
                         println(s"count = $count, resultlist = $resultList, more = $more")

                         innerLastNthR(count - 1,
                              if (count > 0) resultList else resultList.tail,
                              more)
                    }
               }

          ///
          if(n <= 0) return None
          innerLastNthR(n, list, list)
          /*else innerLastNthR(n, list, list) match {
               case Some(elem) => Some(elem)
               case None => None
          }*/

     }



     def myLastNthRecursive[A](list: List[A], n:Int): Option[A] = {

          def innerLastNth(target: Int, currentList: List[A]): Option[A] = {
               currentList match {
                    case Nil => None
                    case _ if (target == 1) => Some(currentList.head)
                    case _ :: tail => {
                         //debug
                         println(s"target = $target, currentlist = $currentList")

                         innerLastNth(target - 1, tail)
                    }
               }
          }

          if(n <= 0) None
          else innerLastNth(list.length - n + 1, list)
     }




     def main(args: Array[String]) {
          assert(lastNthRecursive(List(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1), 4) == Some(4), "Test 1")
          assert(lastNthRecursive(List(12, 11, 10, 9), 2) == Some(10), "Test 1.5")
          assert(lastNthRecursive(List(1,2,3,4,5,6,7,8), 2) == Some(7), "Test 2")
          assert(lastNthRecursive(List(1,2,3,4,5,6,7,8), 1) == Some(8), "Test 3")
          assert(lastNthRecursive(List(12), 1) == Some(12), "Test 4")
          assert(lastNthRecursive(List(), 1) == None, "Test 5")
          assert(lastNthRecursive(List(), 0) == None, "Test 6")
          assert(myLastNthRecursive(List(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1), 4) == Some(4), "Test 7")
          assert(myLastNthRecursive(List(1,2,3,4,5,6,7,8), 2) == Some(7), "Test 8")
          assert(myLastNthRecursive(List(1,2,3,4,5,6,7,8), 1) == Some(8), "Test 9")
          assert(myLastNthRecursive(List(12), 1) == Some(12), "Test 10")
          assert(myLastNthRecursive(List(), 10) == None, "Test 11")
          assert(myLastNthRecursive(List(), 0) == None, "Test 12")
     }

}
