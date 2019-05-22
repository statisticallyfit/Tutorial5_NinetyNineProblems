package Exercises.ScalaNinetyNineExercises

import org.scalatest.Assertions._


/**
  *
  */
object Ex15_MakeDuplicateN {


	def duplicateNMap[A](numTimes: Int, list: List[A]): List[A] = {
		list.flatMap(x => List.fill[A](numTimes)(x))
	}

	def duplicateNRec[A](numTimes: Int, list: List[A]): List[A] = list match {
		case Nil => Nil
		case x :: rest => List.fill[A](numTimes)(x) ++ duplicateNRec(numTimes, rest)
	}

	def main(args: Array[String]) {
		assert(duplicateNMap(5, List(1,2,3,4)) === List(1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3, 4,4,4,4,4), "Test 1")
		assert(duplicateNRec(5, List(1,2,3,4)) === List(1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3, 4,4,4,4,4), "Test 2")

		assert(duplicateNMap(0, List(1,2,3,4)) === Nil, "Test 3")
		assert(duplicateNMap(1, List(1,2,3,4)) === List(1,2,3,4), "Test 4")

	}
}
