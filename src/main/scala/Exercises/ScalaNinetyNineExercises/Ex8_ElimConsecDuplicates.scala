package Exercises.ScalaNinetyNineExercises

import cats.Eq
import cats.implicits._
import org.scalatest.Assertions._
/**
  *
  */
object Ex8_ElimConsecDuplicates {

	def squashRec[A](list: List[A]): List[A] =   list match {

		case Nil => Nil
		case h :: tail => h :: squashRec(tail.dropWhile(_ == h))
	}

	def squashTailRec[A](list: List[A]): List[A] = {

		def inner(acc: List[A], given: List[A]): List[A] = given match {
			case Nil => acc
			case h :: tail => inner(acc :+ h, tail.dropWhile(_ == h))
		}
		inner(Nil, list)
	}


	def squashFoldLeft[A](list: List[A]): List[A] = {
		list.foldLeft(List[A]()){(accList, x) =>
			accList match {
				case Nil => x :: Nil
				case _ if x != accList.last => accList :+ x
				case _ => accList
			}
		}
	}

	def squashFoldRight[A](list: List[A]): List[A] = {
		list.foldRight(List.empty[A]){(x, accList) =>
			accList match {
				case Nil => x :: Nil
				case _ if x != accList.head => x :: accList
				case _ => accList
			}
		}
	}

	def main(args: Array[String]) {

		val neighboringDuplicates: List[Int] = List(1,1,1, 2, 3, 4, 3, 3, 3, 3, 5, 5, 6, 7, 7, 7, 0)
		//println(squashRec(neighboringDuplicates))

		assert(squashRec(neighboringDuplicates) == List(1, 2, 3, 4, 3, 5, 6, 7, 0), "Test 1")
		assert(squashTailRec(neighboringDuplicates) == List(1, 2, 3, 4, 3, 5, 6, 7, 0), "Test 2")
		assert(squashFoldLeft(neighboringDuplicates) == List(1, 2, 3, 4, 3, 5, 6, 7, 0), "Test 3")
		assert(squashFoldRight(neighboringDuplicates) == List(1, 2, 3, 4, 3, 5, 6, 7, 0), "Test 4")
	}

}
