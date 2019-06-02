package Exercises.ScalaNinetyNineExercises

/**
  *
  */
object Ex22_Range {

	def rangeRecursive(start: Int, end: Int): List[Int] = {
		if(end < start) Nil else {
			start :: rangeRecursive(start + 1, end)
		}
	}

	def main(args: Array[String]) {
		assert(rangeRecursive(4, 9) == (4 to 9).toList)
	}
}
