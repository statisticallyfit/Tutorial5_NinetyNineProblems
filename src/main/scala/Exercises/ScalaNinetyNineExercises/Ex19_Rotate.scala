package Exercises.ScalaNinetyNineExercises

/**
  *
  */
object Ex19_Rotate {

	//Rotate a list N places to the left
	def rotate[A](numPlaces: Int, list: List[A]): List[A] = {
		list match {
			case Nil => Nil
			case x :: xs if numPlaces == 0 => x :: xs
			case x :: xs => rotate(numPlaces - 1, xs :+ x)
		}
	}

	def main(args: Array[String]) {
		assert(rotate(3, List(1,2,3,4,5,6)) == List(4,5,6,1,2,3))
	}
}
