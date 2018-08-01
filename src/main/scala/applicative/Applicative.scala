package applicative
import applicative.Applicative.Const
import fpinscala.monads.Functor
import fpinscala.state.State
import fpinscala.mondoids.{Foldable, Monoid}

//Functions can be defined in terms of each other, as long as at least one of them is
//overwritten in a derived class.  This means that the derived class can choose which one to
//overwrite depending on which is simpler given the class' implementation, and then both
//behaviors will become available.
trait Applicative[F[_]] extends Functor[F] {
	def unit[A](a: => A): F[A]
	
	def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))
	
	def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fa, fab)((a, f) => f(a))
	
	def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
		as.foldRight(unit( List[B]() ))( (a, fbs) => map2(f(a), fbs)(_::_) )
	
	def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(a => a)
	
	def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = ???
	
	def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
	
	//def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
	
	//def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)( f.curried ))(fb)
	def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(apply(unit(f.curried))(fa))(fb)
	
	def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
		apply(map2(fa, fb)( (a, b) => f.curried(a)(b) ))(fc)
	
	def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
		apply(map3(fa, fb, fc)( (a, b, c) => f.curried(a)(b)(c) ))(fd)
	
	def product[G[_]](G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] = {
		val self = this
		new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
			override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
			override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])): (F[B], G[B]) =
				(self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
		}
	}
	
	def compose[G[_]](G: Applicative[G]): Applicative[({ type f[x] = F[G[x]] })#f] = {
		val self = this
		new Applicative[({ type f[x] = F[G[x]] })#f] {
			override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
			override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
				self.map2(fga, fgb)(G.map2(_, _)(f))
		}
	}
	
}

//All monads are applicatives, but not all applicatives are monads
//Applicatives are less powerful than monads, but that means applicative is more flexible
//in its implementation
trait Monad[F[_]] extends Applicative[F] {
	def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)
	def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
	def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
	
	override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))
	override def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fa, fab)((a, f) => f(a))
	override def map[A, B](m: F[A])(f: A => B): F[B] = flatMap(m)(a => unit(f(a)))
	
}

object Applicative {
	val streamApplicative = new Applicative[Stream] {
		def unit[A](a: => A): Stream[A] = Stream.continually(a)
		override def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] = fa zip fb map f.tupled
	}
	def eitherMonad[E] = new Monad[({ type f[x] = Either[E, x] })#f] {
		override def unit[A](a: => A): Either[E, A] = Right(a)
		
		override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
			case Left(e) => Left(e)
			case Right(a) => f(a)
		}
	}
	def validationApplicative[E] = new Applicative[({ type StringError[x] = Validation[E, x] })#StringError] {
		def unit[A](a: => A): Validation[E, A] = Success(a)
		override def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] = fab match {
			case Success(f) => fa match {
				case Success(a) => Success(f(a))
				case Failure(eh, et) => Failure(eh, et)
			}
			case Failure(ffh, fft) => Failure(ffh, fft)
		}
	}
	
	type Const[M, B] = M
	
	implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({ type f[x] = Const[M, x] })#f] {
		def unit[A](a: => A): M = M.zero
		override def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
	}

}

object Monad {
	def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
		override def unit[A](a: A): State[S, A] = State(s => (a, s))
		override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa flatMap f
	}
	def getState[S]: State[S, S] = State(s => (s, s))
	def setState[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

trait Traverse[F[_]] extends Functor[F] with Foldable[F]{
	def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] = sequence(map(fa)(f))
	def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] = traverse(fga)(ga => ga)
	
	type Id[A] = A
	
	val idMonad = new Monad[Id] {
		def unit[A](a: => A) = a
		override def flatMap[A, B](a: A)(f: A => B): B = f(a)
	}
	
	override def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idMonad)
	
	override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
		traverse[({ type f[x] = Const[M, x] })#f, A, Nothing](as)(f)(Applicative.monoidApplicative(mb))

	def traverseState[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
		traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)
	
	def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = traverseState(fa)((a: A) => for {
		s1 <- Monad.getState[S]
		(b, s2) = f(a, s1)
		_ <- Monad.setState(s2)
	} yield b).run(s)
	
	def zipWithIndex[A](ta: F[A]): F[(A, Int)] = mapAccum(ta, 0)((a, s) => ((a, s), s + 1))._1
	
	def toList[A](fa: F[A]): List[A] = mapAccum(fa, List[A]())((a, s) => ((), a::s))._2.reverse
	
	def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1
	
}

object Traverse {
	def treeTraverse[A]: Traverse[Tree] = new Traverse[Tree] {
		override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
			case Leaf(v) => Leaf(f(v))
			case Branch(v, l, r) => Branch(f(v), map(l)(f), map(r)(f))
		}
		
		override def sequence[G[_], A](fga: Tree[G[A]])(implicit G: Applicative[G]): G[Tree[A]] = 
	}
}





