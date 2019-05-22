package Exercises.ScalaNinetyNineExercises

import org.scalatest.Assertions._
/**
  *
  */
object Ex10_12_13_RunLength {


	def runLengthSpan[A](list: List[A]): List[(Int, A)] = list match {
		case Nil => Nil
		case _ => {
			val (firstRun, rest): (List[A], List[A]) = list.span(_ == list.head)

			(firstRun.length, firstRun.head) :: runLengthSpan(rest)
		}
	}

	def runLengthRec[A](list: List[A]): List[(Int, A)] = {

		def innerRunRec(accTuples: List[(Int, A)], given: List[A]): List[(Int, A)] =  accTuples  match {

			case Nil => given match {
				case Nil => Nil
				case a :: rest => {
					innerRunRec( List((1, a)), rest) //start a brand-new list
				}
			}
			case _ => given match {

				case Nil => accTuples

				case a :: rest  => {
					//{if a == accTuples.last._2 =>  innerRunRec((n+1, x))
					val (n, x): (Int, A) = accTuples.last

					if(x == a) {
						innerRunRec(accTuples.init :+ (n+1, x), rest)
					} else { //if x != a then start a new tuple
						innerRunRec(accTuples :+ (1, a), rest)
					}
				}
			}
		}

		innerRunRec(List.empty[(Int, A)], list)
	}


	def decodeRunLengthRec[A](tuples: List[(Int, A)]): List[A] = tuples match {
		case Nil => Nil
		case (count, x) :: rest => List.fill[A](count)(x) ++ decodeRunLengthRec(rest)
	}


	def main(args: Array[String]) {
		val xs = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'd', 'd', 'e')
		val xs2 = List('a', 'a', 'b', 'b', 'b', 'c', 'c', 'c')

		assert(runLengthRec(xs) === List( (4, 'a'), (1, 'b'), (2, 'c'), (2, 'd'), (1, 'e')), "Test 1")
		assert(runLengthRec(xs2) === List( (2, 'a'), (3, 'b'), (3, 'c')), "Test 2")
		assert(runLengthSpan(xs) === List( (4, 'a'), (1, 'b'), (2, 'c'), (2, 'd'), (1, 'e')), "Test 1.5")
		assert(runLengthSpan(xs2) === List( (2, 'a'), (3, 'b'), (3, 'c')), "Test 2.5")


		val tups = List( (4, 'a'), (1, 'b'), (2, 'c'), (2, 'd'), (1, 'e'))
		val tups2 = List( (2, 'a'), (3, 'b'), (3, 'c'))


		assert(decodeRunLengthRec(tups) === List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'd', 'd', 'e'), "Test 3")
		assert(decodeRunLengthRec(tups2) === List('a', 'a', 'b', 'b', 'b', 'c', 'c', 'c'), "Test 4")

		//they should be inverses
		assert(runLengthRec(decodeRunLengthRec(tups)) === tups, "Test 5")
		assert(runLengthRec(decodeRunLengthRec(tups2)) === tups2, "Test 6")
		assert(decodeRunLengthRec(runLengthRec(xs)) === xs, "Test 7")
		assert(decodeRunLengthRec(runLengthRec(xs2)) === xs2, "Test 8")
	}
}
