package Exercises.ScalaNinetyNineExercises

/**
  *
  */
object Ex20_23_25_RemoveAt_RandomSelectPermute {

	def removeAt[A](i: Int, list: List[A]): (List[A], A) = {
		list.splitAt(i) match {
			case (Nil, _) if i < 0 => throw new NoSuchElementException
			case (pre, e :: post) => (pre ::: post, e)
			case (pre, Nil) => throw new NoSuchElementException
		}
	}

	def randomSelect[A](n: Int, list: List[A]): List[A] = {
		if(n <= 0) Nil
		else {
			val iRand: Int = new util.Random().nextInt(list.length)
			val (rest, e) = removeAt(iRand, list)
			e :: randomSelect(n-1, rest)
		}
	}

	def randomPermute[A](list: List[A]): List[A] = {
		randomSelect(list.length, list)
	}
}
