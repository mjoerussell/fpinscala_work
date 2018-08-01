package fpinscala.parallelism
import java.util.concurrent.atomic.{AtomicReference, AtomicInteger}
import java.util.concurrent.{Callable, ExecutorService}
import annotation.tailrec

//Code taken from book's github (not expected to implement myself)
final class Actor[A](strategy: Strategy)(handler: A => Unit, onError: Throwable => Unit = throw(_)) {
	self =>
	
	private val tail = new AtomicReference(new Node[A]())
	private val suspended = new AtomicInteger(1)
	private val head = new AtomicReference(tail.get)
	
	/** Alias for `apply` */
	def !(a: A) {
		val n = new Node(a)
		head.getAndSet(n).lazySet(n)
		trySchedule()
	}
	
	/** Pass the message `a` to the mailbox of this actor */
	def apply(a: A) {
		this ! a
	}
	
	def contramap[B](f: B => A): Actor[B] =
		new Actor[B](strategy)((b: B) => (this ! f(b)), onError)
	
	private def trySchedule() {
		if (suspended.compareAndSet(1, 0)) schedule()
	}
	
	private def schedule() {
		strategy(act())
	}
	
	private def act() {
		val t = tail.get
		val n = batchHandle(t, 1024)
		if (n ne t) {
			n.a = null.asInstanceOf[A]
			tail.lazySet(n)
			schedule()
		} else {
			suspended.set(1)
			if (n.get ne null) trySchedule()
		}
	}
	
	@tailrec
	private def batchHandle(t: Node[A], i: Int): Node[A] = {
		val n = t.get
		if (n ne null) {
			try {
				handler(n.a)
			} catch {
				case ex: Throwable => onError(ex)
			}
			if (i > 0) batchHandle(n, i - 1) else n
		} else t
	}
}

private class Node[A](var a: A = null.asInstanceOf[A]) extends AtomicReference[Node[A]]

object Actor {
	
	/** Create an `Actor` backed by the given `ExecutorService`. */
	def apply[A](es: ExecutorService)(handler: A => Unit, onError: Throwable => Unit = throw(_)): Actor[A] =
		new Actor(Strategy.fromExecutorService(es))(handler, onError)
}

/**
  * Provides a function for evaluating expressions, possibly asynchronously.
  * The `apply` function should typically begin evaluating its argument
  * immediately. The returned thunk can be used to block until the resulting `A`
  * is available.
  */
trait Strategy {
	def apply[A](a: => A): () => A
}

object Strategy {
	
	/**
	  * We can create a `Strategy` from any `ExecutorService`. It's a little more
	  * convenient than submitting `Callable` objects directly.
	  */
	def fromExecutorService(es: ExecutorService): Strategy = new Strategy {
		def apply[A](a: => A): () => A = {
			val f = es.submit { new Callable[A] { def call = a} }
			() => f.get
		}
	}
	
	/**
	  * A `Strategy` which begins executing its argument immediately in the calling thread.
	  */
	def sequential: Strategy = new Strategy {
		def apply[A](a: => A): () => A = {
			val r = a
			() => r
		}
	}
}
