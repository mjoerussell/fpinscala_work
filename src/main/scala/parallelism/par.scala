package fpinscala.parallelism

import java.util.concurrent.{Callable, ExecutorService, Future}

import scala.concurrent.duration.TimeUnit
import scala.language.implicitConversions

object Par {
	type Par[A] = ExecutorService => Future[A]
	
	def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
	
	def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
	
	def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
	
	def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
	
	//This is potentially running in another logical thread.
	//When `get` is called it forces the Future to evaluate its argument
	private case class UnitFuture[A](get: A) extends Future[A] {
		def isDone = true
		def get(timeout: Long, units: TimeUnit) = get
		def isCancelled = false
		def cancel(evenIfRunning: Boolean): Boolean = false
	}
	
	def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
		val af = a(es)
		val bf = b(es)
		UnitFuture(f(af.get, bf.get))	//Runs `f` in a separate logical thread, so af.get && bf.get aren't called immediately
	}
	
	//Transform a list of parallel computations into one parallel computation that produces a list of values
	def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
		case h::t => map2(h, sequence(t))(_::_)
		case _ => unit(List[A]())
	}
	
	//Map a function `f` on a List in parallel
	def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
		val fbs: List[Par[B]] = ps.map(asyncF(f))
		sequence(fbs)
	}
	
	//My solution
	def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = as match {
		case h::t => map2(unit(h), parFilter(t)(f))((a, fas) => if (f(a)) a::fas else fas)
		case _ => unit(List[A]())
	}
	
	//Book's solution
	def parFilter2[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
		val pars: List[Par[List[A]]] = as map asyncF(a => if (f(a)) List(a) else List())
		map(sequence(pars))(_.flatten)
	}
	
	//Marks some value `a` for evaluation in a separate thread (created by es.submit, evaluated in the `call` function
	def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
		override def call(): A = a(es).get
	})
	
	def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))
	
	def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)
	
	def delay[A](fa: => Par[A]): Par[A] = es => fa(es)
	
	def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es => {
		if (run(es)(cond).get) t(es)
		else f(es)
	}
	
	def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
		val i = run(es)(n).get
		run(es)(choices(i))
	}
	
	//Choice via choiceN
	def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = choiceN(map(cond)(c => if (c) 1 else 0))(List(f, t))
	
	def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => {
		val k = run(es)(key).get
		run(es)(choices(k))
	}
	
	def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = es => {
		val ar = run(es)(a).get
		run(es)(f(ar))
	}
	
	def choiceN2[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = flatMap(n)(choices(_))
	
	def choice3[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = flatMap(cond)(if (_) t else f)
	
	def join[A](a: Par[Par[A]]): Par[A] = es => run(es)(run(es)(a).get())
	
	def flatMapViaJoin[A, B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))
	
	def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(x => x)
	
	implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)
	
	class ParOps[A](p: Par[A]) {
	
	}
	
}
