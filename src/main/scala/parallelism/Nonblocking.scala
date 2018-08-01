package fpinscala.parallelism
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference
import language.implicitConversions

object Nonblocking {
	trait Future[+A] {
		private[parallelism] def apply(k: A => Unit): Unit
	}
	
	type Par[+A] = ExecutorService => Future[A]
	
	object Par {
		def run[A](es: ExecutorService)(p: Par[A]): A = {
			val ref = new AtomicReference[A]				//A mutable && threadsafe reference for storing the result
			val latch = new CountDownLatch(1)		//A latch that, when decremented, implies that `ref` has the result
			p(es) { a => ref.set(a); latch.countDown() }	//Asynchronously set the result & decrement the latch
			latch.await 									//Block until `latch.countDown` is called asynchronously
			ref.get
		}
		
		def unit[A](a: A): Par[A] = es => new Future[A] {
			def apply(cb: A => Unit): Unit = cb(a)
		}
		
		def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
		
		def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
			def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
		}
		
		//Helper function for construting Pars out of calls to nonblocking continuation-style APIs
		def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
			def apply(cb: A => Unit): Unit = f(cb)
		}
		
		//Helper function for evaluating an action async
		//Example: eval(es)(cb(a)) means "submit some callback with value `a`
		def eval(service: ExecutorService)(r: => Unit): Unit = service.submit(new Callable[Unit] {
			def call = r
		})
		
		def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => new Future[C] {
			def apply(cb: C => Unit): Unit = {
				var ar: Option[A] = None
				var br: Option[B] = None
				//An actor that awaits both results, combines them with `f`, and passes them to the callback
				val combiner = Actor[Either[A, B]](es) {
					//If A came first, store in `ar` and wait for B.  Else, call `f` with both results and pass resulting C to callback
					case Left(a) => br match {
						case None => ar = Some(a)
						case Some(b) => eval(es)(cb(f(a, b)))
					}
					//Same as above but if B comes first
					case Right(b) => ar match {
						case None => br = Some(b)
						case Some(a) => eval(es)(cb(f(a, b)))
					}
				}
				//Pass the actor as a continuation to both sides.
				a(es)(pa => combiner ! Left(pa))
				b(es)(pb => combiner ! Right(pb))
			}
		}
		
		def map[A, B](p: Par[A])(f: A => B): Par[B] = es => new Future[B] {
			def apply(cb: B => Unit): Unit = p(es)(a => eval(es) { cb(f(a)) })
		}
		
		def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
		
		def sequenceRight[A](as: List[Par[A]]): Par[List[A]] = as match {
			case h::t => map2(h, fork(sequenceRight(t)))(_::_)
			case _ => unit(Nil)
		}
		
//		def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
//			if (as.isEmpty) unit(Vector())
//			else if (as.length == 1) map(as.head)(a => Vector(a))
//			else {
//				val (l, r) = as.splitAt(as.length / 2)
//				val map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
//			}
//		}
		
	}
	
}
