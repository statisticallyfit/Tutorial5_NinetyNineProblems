package Exercises.ScalaNinetyNineExercises

import org.scalatest.Assertions._

/**
  *
  */
object Ex16_DropEveryNth {


	def dropNthTailRec[A](n: Int, list: List[A]): List[A] = {

		def innerDrop(n: Int, nCounter: Int, acc: List[A], given: List[A]): List[A] = given match {
			case Nil => acc
				//if at the right spot, drop the element and restart counter
			case x :: rest if nCounter == n => innerDrop(n, 1, acc, rest)
				//if not at right spot, keep the element and increment counter
			case x :: rest => innerDrop(n, nCounter + 1, acc :+ x, rest)
		}

		innerDrop(n, 1, List.empty[A], list)
	}

	def dropNthTailRec2[A](n: Int, list: List[A]): List[A] = {

		def innerDrop(counter: Int, acc:List[A], given: List[A]): List[A] = {
			(counter, given) match {
				case (_, Nil) => acc
				case (1, _ :: rest) => innerDrop(n, acc, rest)
				case (_, x :: rest) => innerDrop(counter - 1, acc :+ x, rest)
			}
		}
		innerDrop(n, Nil, list)
	}


	def dropNthRec[A](n: Int, list: List[A]): List[A] = {

		def innerDrop(counter: Int, given: List[A]): List[A] = {
			(counter, given) match {
				case (_, Nil) => Nil
				case (1, _ :: rest) => innerDrop(n, rest)
				case (_, x :: rest) => x :: innerDrop(counter - 1, rest)
			}
		}
		innerDrop(n, list)
	}



	def dropNthFilter[A](n: Int, list: List[A]): List[A] = {
		//shifting the index with a map and then filtering, then map to get the element, not the elem-index tuple.
		list.zipWithIndex.map{case (e, i) => (e, i+1)}.filter{tup => (tup._2 % n) != 0}.map(_._1)
	}

	def main(args: Array[String]) {
		assert(dropNthTailRec(3, List(1, 3, 8, 5, 4, 2, 6)) === List(1,3, 5,4, 6), "Test 1")
		assert(dropNthTailRec(1, List(1,3,4,5)) === List(), "Test 2")
		assert(dropNthTailRec(1, List()) === List(), "Test 3")

		assert(dropNthTailRec2(3, List(1, 3, 8, 5, 4, 2, 6)) === List(1,3, 5,4, 6), "Test 4")
		assert(dropNthTailRec2(1, List(1,3,4,5)) === List(), "Test 5")
		assert(dropNthTailRec2(1, List()) === List(), "Test 6")

		assert(dropNthRec(3, List(1, 3, 8, 5, 4, 2, 6)) === List(1,3, 5,4, 6), "Test 7")
		assert(dropNthRec(1, List(1,3,4,5)) === List(), "Test 8")
		assert(dropNthRec(1, List()) === List(), "Test 9")

		assert(dropNthFilter(3, List(1, 3, 8, 5, 4, 2, 6)) === List(1,3, 5,4, 6), "Test 10")
		assert(dropNthFilter(1, List(1,3,4,5)) === List(), "Test 11")
		assert(dropNthFilter(1, List()) === List(), "Test 12")
	}

}
