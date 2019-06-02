package Exercises.ScalaNinetyNineExercises

/**
  *
  */
object Ex26_Combinations {

	def flatMapSublists[A, B](as: List[A])(f: List[A] => List[B]): List[B] = as match {
		case Nil => Nil
		case sublist@(_ :: tail) => f(sublist) ++ flatMapSublists(tail)(f)
	}

	//take n at a time from list
	def makeCombinations[A](n: Int, list: List[A]): List[List[A]] = {
		if(n == 0) List(Nil) else {
			flatMapSublists(list) ({
				sublist => makeCombinations(n-1, sublist.tail)
					.map(sublistCombos => sublist.head :: sublistCombos)
			})
		}
	}

	def main(args: Array[String]) {
		assert(makeCombinations(3, List(1,2,3,4,5)) == (1 to 5).toList.combinations(3).toList)
	}
}
