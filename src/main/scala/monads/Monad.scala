package fpinscala.monads
import fpinscala.state._

trait Functor[F[_]] {
	def map[A, B](fa: F[A])(f: A => B): F[B]
	//Turn a Functor of tuples into a tuple of Functors
	def unzip[A, B](fab: F[(A, B)]): (F[A], F[B]) =
		(map(fab)(_._1), map(fab)(_._2))
	//Get a Functor of either A or B depending on e, which is either a Functor of A or a Functor of B
	def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
		case Left(fa) => map(fa)(Left(_))
		case Right(fb) => map(fb)(Right(_))
	}
}

trait Monad[F[_]] extends Functor[F]{
	def unit[A](a: A): F[A]
	def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
	
	def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))
	def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))
	
	def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = sequenceViaFoldRight(la map f)
	
	//My solution
	def sequence[A](lma: List[F[A]]): F[List[A]] = lma match {
		case h::t => map2(h, sequence(t))(_::_)
		case _ => unit(Nil)
	}
	
	//Book's solution
	def sequenceViaFoldRight[A, B](lma: List[F[A]]): F[List[A]] = lma.foldRight(unit(List[A]()))((ma, mlb) => map2(ma, mlb)(_::_))
	
	def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))
	
	def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))
	
	def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ???
	
	def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
	
	def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
	
}

object Monad {
	def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
		override def unit[A](a: A): State[S, A] = State(s => (a, s))
		override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa flatMap f
	}
	
	def getState[S]: State[S, S] = State(s => (s, s))
	def setState[S](s: S): State[S, Unit] = State(_ => ((), s))
	
//	def zipWithIndex[A](as: List[A]): List[(Int, A)] = {
//		val intState = stateMonad[Int]
//		as foldLeft (intState.unit( List[(Int, A)] ))((acc, a) => for {
//			xs <- acc
//			n  <- getState
//			_  <- setState(n + 1)
//		} yield (n, a)::xs ).run(0)._1.reverse
//	}

	
}

case class Id[A](value: A) {
	def map[B](f: A => B): Id[B] = Id(f(value))
	def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

//case class Reader[R, A](run: R => A)
//
//object Reader {
//	def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
//		def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
//		override def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(fa.run(r)).run(r))
//	}
//}





