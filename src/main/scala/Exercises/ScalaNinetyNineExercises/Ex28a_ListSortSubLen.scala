package Exercises.ScalaNinetyNineExercises

/**
  *
  */
object Ex28a_ListSortSubLen {

	def listSortSublen[A](list: List[List[A]]): List[List[A]] = {
		//list.map(xs => (xs, xs.length) )
		/*list sort { _.length < _.length}*/
		??? //TODO
	}

	def main(args: Array[String]) {
		val list = List(List('a', 'b', 'c'), List('d', 'e'), List('f', 'g', 'h'),
			List('d', 'e'), List('i', 'j', 'k', 'l'), List('m', 'n'), List('o'))

		val result = List(List('o'), List('d', 'e'), List('d', 'e'), List('m', 'n'), List('a', 'b', 'c'),
			List('f', 'g', 'h'), List('i', 'j', 'k', 'l'))

		assert(listSortSublen(list) == result)
	}
}
