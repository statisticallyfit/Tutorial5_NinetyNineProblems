package Exericises.ScalaNinetyNineExercises

import org.scalatest.Assertions._

import scala.annotation.tailrec
/**
  *
  */
object Ex7_FlattenRecursive {


     //help sas implicits type mismatch
     /*def flattenWithFoldLeft[A](list: List[A])(implicit m: Monoid[A]): A = {
          list.foldLeft(m.empty)((acc, xElem) => m.combine(acc, xElem))
     }*/
	def flattenFoldLeft[A](nested: List[List[A]]): List[A] = {
		nested.foldLeft(List.empty[A])((accList, newList) => accList ++ newList)
	}

	def flattenRecursive[A](nested: List[List[A]]): List[A]  = {

		@tailrec
		def innerFlattenRec(acc: List[A], nested: List[List[A]]): List[A] = nested match {
			case Nil => acc
			//case lastList :: Nil =>
			case firstList :: rest => innerFlattenRec(acc ++ firstList, rest)
		}

		innerFlattenRec(List.empty[A], nested)
     }

     def main(args: Array[String]) {
          val nested: List[List[Int]] = List(List(1,2,3), List(9,4,3,3), List(5,5,9))
          assert(flattenFoldLeft(nested) === nested.flatten)


		assert(flattenRecursive(nested) === nested.flatten)
     }
}

