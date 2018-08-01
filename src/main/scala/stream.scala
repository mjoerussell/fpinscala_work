package fpinscala

import fpinscala.Stream._

sealed trait Stream[+A] {

    def headOption: Option[A] = this match {
        case Cons(h, _) => Some(h())
        case _ => None
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    @annotation.tailrec
    final def foldLeft[B](z: => B)(f: (A, => B) => B): B = this match {
        case Cons(h, t) => t().foldLeft(f(h(), z))(f)
        case _ => z
    }

    def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

    @annotation.tailrec
    final def find(f: A => Boolean): Option[A] = this match {
        case Empty => None
        case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
    }

    def toList: List[A] = foldRight(List[A]())(_ :: _)

    def take(n: Int): Stream[A] = this match {
        case Cons(h, t) => if (n > 0) cons(h(), t().take(n - 1)) else Empty
        case _ => Empty
    }

    def drop(n: Int): Stream[A] = this match {
        case Cons(h, t) => if (n > 0) t().drop(n - 1) else Cons(h, t)
        case _ => Empty
    }

    def takeWhile(f: A => Boolean): Stream[A] = ???

    def forAll(f: A => Boolean): Boolean = foldRight(true)((a, b) => f(a) && b)

    def append[B >: A](b: Stream[B]): Stream[B] = foldRight(b)((a, b) => cons(a, b))

    def startsWith[B](s: Stream[B]): Boolean = s match {
        case Cons(h, t) => headOption.map(ho => (ho == h()) && drop(1).startsWith(t())) getOrElse false
        case Empty => true
    }

}
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
case object Empty extends Stream[Nothing]

object Stream {
    def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
        lazy val head = h 
        lazy val tail = t 
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = 
        if (as.isEmpty) empty
        else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    val ones: Stream[Int] = constant(1)

    def from(n: Int): Stream[Int] = cons(n, from(n+1))

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
        case Some((a, s)) => cons(a, unfold(s)(f))
        case _ => Empty
    }

}