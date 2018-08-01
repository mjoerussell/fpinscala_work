package fpinscala.testing
import fpinscala.state._
import fpinscala.testing.Prop._

trait Prop {
	def check: Either[(FailedCase, SuccessCount), SuccessCount]
	def forAll[A](a: Gen[A])(f: A => Boolean): Prop
}

object Prop {
	type FailedCase = String
	type SuccessCount = Int
}

case class Gen[+A](sample: State[RNG, A]) {

}

object Gen {
	def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
	def boolean: Gen[Boolean] = Gen(State(RNG.boolean))
	def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(RNG.nonNegativeInt).map(i => start + i % (stopExclusive - start)))
	//def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State())
}

