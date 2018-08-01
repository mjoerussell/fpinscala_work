package fpinscala.mondoids
import fpinscala.parallelism.Par
import fpinscala.parallelism.Par._
import annotation.tailrec

trait Monoid[A] {
	def op(a1: A, a2: A): A
	def zero: A
}

trait Foldable[F[_]] {
	import Monoid._
	
	def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = foldMap(as)(f.curried)(endoMonoid[B])(z)
	def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)
	def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = foldLeft(as)(mb.zero)((b, a) => mb.op(f(a), b))
	def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
}

object Monoid {
	val StringMonoid = new Monoid[String] {
		def op(s1: String, s2: String): String = s1 + s2
		def zero: String = ""
	}
	val intAddition: Monoid[Int] = new Monoid[Int] {
		override def op(a1: Int, a2: Int): Int = a1 + a2
		override def zero: Int = 0
	}
	val intMultiplication: Monoid[Int] = new Monoid[Int] {
		override def op(a1: Int, a2: Int): Int = a1 * a2
		override def zero: Int = 1
	}
	val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
		override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
		override def zero: Boolean = false
	}
	val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
		override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
		override def zero: Boolean = true
	}
	def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
		override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
		override def zero: Option[A] = None
	}
	def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
		override def op(a1: A => A, a2: A => A): A => A = a => a2(a1(a))
		override def zero: A => A = a => a
	}
	def ListMonoid[A] = new Monoid[List[A]] {
		def op(l1: List[A], l2: List[A]): List[A] = l1 ++ l2
		def zero: List[A] = Nil
	}
	
	def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.map(f).foldRight(m.zero)(m.op)
	
	def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.length match {
		case 0 | 1 => v.map(f).headOption getOrElse m.zero
		case _ =>
			val (first, second) = v.splitAt(v.length / 2)
			m.op(foldMapV(first, m)(f), foldMapV(second, m)(f))
	}
	
	def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
		override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
		override def zero: Par[A] = Par.unit(m.zero)
	}
	
	def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = foldMapV(v, par(m))(Par.asyncF(f))
	
	def isSorted(v: IndexedSeq[Int]): Boolean = ???
	
	def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
		override def op(a1: A, a2: A) = m.op(a2, a1)
		
		override def zero = m.zero
	}
	
	def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
		override def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = (a.keySet ++ b.keySet).foldLeft(zero) {
			(acc, k) =>
				acc.updated(k, V.op(a getOrElse (k, V.zero), b getOrElse (k, V.zero)))
		}
		
		override def zero: Map[K, V] = Map[K, V]()
	}
	
}

object Foldable {
	val FoldableList: Foldable[List] = new Foldable[List] {
		override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldLeft(as.reverse)(z)((a, b) => f(b, a))
		
		@tailrec override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as match {
			case h::t => foldLeft(t)(f(z, h))(f)
			case _ => z
		}
		
		//override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = Monoid.foldMap(as, mb)(f)
	}
	val FoldableIndexedSeq: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
		override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = foldLeft(as.reverse)(z)((a, b) => f(b, a))
		
		@tailrec override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.length match {
			case 0 | 1 => as.map(a => f(z, a)).headOption getOrElse z
			case _ => foldLeft(as.tail)(f(z, as.head))(f)
		}
		
		//override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = Monoid.foldMapV(as, mb)(f)
	}
	
	val FoldableOption: Foldable[Option] = new Foldable[Option] {
		override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = foldLeft(as)(z)((b, a) => f(a, b))
		override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
			case Some(a) => f(z, a)
			case None => z
		}
	}
	
}