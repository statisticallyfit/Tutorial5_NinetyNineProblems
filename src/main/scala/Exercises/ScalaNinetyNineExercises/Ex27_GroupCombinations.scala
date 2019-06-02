package Exercises.ScalaNinetyNineExercises

/**
  *
  */
object Ex27_GroupCombinations {

	// P27 (**) Group the elements of a set into disjoint subsets.
	//     a) In how many ways can a group of 9 people work in 3 disjoint subgroups
	//        of 2, 3 and 4 persons?  Write a function that generates all the
	//        possibilities.
	//
	//        Example:
	//        scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
	//        res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
	//
	//     b) Generalize the above predicate in a way that we can specify a list
	//        of group sizes and the predicate will return a list of groups.
	//
	//        Example:
	//        scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
	//        res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
	//
	//     Note that we do not want permutations of the group members;
	//     i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...).
	//     However, we make a difference between ((Aldo, Beat), (Carla, David), ...)
	//     and ((Carla, David), (Aldo, Beat), ...).
	//
	//     You may find more about this combinatorial problem in a good book on
	//     discrete mathematics under the term "multinomial coefficients".

	import Ex26_Combinations._

	def group3[A](ls: List[A]): List[List[List[A]]] =
		for {
			a <- makeCombinations(2, ls)
			noA = ls.filterNot(e => e == a) //ls -- a
			b <- makeCombinations(3, noA)
		} yield List(a, b, noA.filterNot(e => e == b) ) ///-- b)

	/*def group[A](ns: List[Int], ls: List[A]): List[List[List[A]]] = ns match {
		case Nil     => List(Nil)
		case n :: ns => makeCombinations(n, ls) flatMap { c =>
			group(ns, ls.filter(e => e != c)) map {c :: _}
		}
	}*/
	def group[A](ns: List[Int], elements: List[A]): List[List[List[A]]] = {
		ns match {
			case Nil => List(Nil)
			case n :: ns => makeCombinations(n, elements)
				.flatMap{
					comboElem => group(ns, elements.filter(e => e != comboElem))
						.map { res => comboElem :: res }
				}
		}
	}

	def main(args: Array[String]) {
		println(group(List(2,3,5), List(333, 444, 111, 222, 888, 999, 555, 777)).mkString("\n"))
	}
}
