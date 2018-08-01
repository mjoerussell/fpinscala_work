package fpinscala


import scala.{Either => _, Left => _, Option => _, Right => _}

sealed trait Either[+E, +A] {
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }
  
  def getOrElse[EE >: E, B >: A](op: => B): B = this match {
    case Right(a) => a
    case Left(_) => op
  }
  
  def map[B](f: A => B): Either[E, B] = this flatMap (a => Right(f(a)))
  
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this map (a => Right(a)) getOrElse b
  
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap(aa => b map(bb => f(aa, bb)))
  
  //Equivalent to map2, just with for comprehension syntax
  def map22[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    aa <- this
    bb <- b
  } yield f(aa, bb)

  

}
case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h::t => f(h).map2(traverse(t)(f))(_::_)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

}