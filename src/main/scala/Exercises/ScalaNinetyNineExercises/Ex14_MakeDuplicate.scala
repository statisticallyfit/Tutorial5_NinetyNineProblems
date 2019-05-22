package Exercises.ScalaNinetyNineExercises

import org.scalatest.Assertions._
/**
  *
  */
object Ex14_MakeDuplicate {


	def duplicateMap[A](list: List[A]): List[A] = {
		list.flatMap(x => List(x, x))
	}

	def duplicateRec[A](list: List[A]): List[A] = list match {
		case Nil => Nil
		case x :: rest => x :: x :: duplicateRec(rest)
	}

	def main(args: Array[String]) {
		assert(duplicateMap(List(1,2,3,4)) === List(1,1, 2,2, 3,3, 4,4))
		assert(duplicateRec(List(1,2,3,4)) === List(1,1, 2,2, 3,3, 4,4))
	}
}
