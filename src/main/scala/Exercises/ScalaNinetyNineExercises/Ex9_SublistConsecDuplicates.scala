package Exercises.ScalaNinetyNineExercises

import scala.annotation.tailrec
import org.scalatest.Assertions._
/**
  *
  */
object Ex9_SublistConsecDuplicates {

	def sublistRec[A](list: List[A]): List[List[A]] = {

		@tailrec
		def innerSublistRec[A](accNested: List[List[A]], given: List[A]): List[List[A]] =  accNested match {

				//when acclist is NIL
			case Nil => given match {
				case Nil => Nil
				case a :: rest => innerSublistRec(List(List(a)), rest)
			}

				//when acclist is not nil (can call acc.last.last)
			case _ => given match {

				case Nil => accNested
				//if the first two are equal then continue, adding them to accumulator list
				case a :: rest if a == accNested.last.last => {
					//updating the last element
					val newLast: List[A] = accNested.last :+ a

					//new acc = all bust last lists in acc groubed with the new last
					innerSublistRec(accNested.init :+ newLast, rest)
				}
				//if a != acc.last.last then separate: create new list containing the element as last thing in acc
				case a :: rest => innerSublistRec(accNested :+ List(a), rest)
			}
		}

		innerSublistRec(List.empty[List[A]], list)
	}


	def sublistSpan[A](list: List[A]): List[List[A]] = list match {
		case Nil => List(List())
		case _ => {
			// get the values equalying same value, at the start of the list
			val (firstRun, rest): (List[A], List[A]) = list.span(_ == list.head)

			//if we don't have this check here, we end up with [.... List() ] if rest is empty
			if(rest.isEmpty){
				List(firstRun) // wrap it up, done, last value
			} else {
				firstRun :: sublistSpan(rest)
			}
		}
	}


	def main(args: Array[String]) {
		val xs: List[Int] = List(1,1,1, 2, 2, 3, 4, 5, 5, 4, 4, 1)

		println(sublistRec(xs))

		assert(sublistRec(xs) === List(List(1,1,1), List(2,2), List(3), List(4), List(5,5), List(4,4), List(1)),
			"Test 1")
		assert(sublistRec(List(1,1,1,2,2)) === List(List(1,1,1), List(2,2)), "Test 2")


		assert(sublistSpan(xs) === List(List(1,1,1), List(2,2), List(3), List(4), List(5,5), List(4,4), List(1)),
			"Test 3")
		assert(sublistSpan(List(1,1,1,2,2)) === List(List(1,1,1), List(2,2)), "Test 4")
	}
}
