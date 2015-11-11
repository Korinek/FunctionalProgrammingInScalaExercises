package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(l: Stream[A], acc: List[A]): List[A] = l match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case _ if n <= 0 => empty
    case Cons(h, t) => cons(h(), t().take(n-1))
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
     case Cons(_, t) if n > 0 => t().drop(n-1)
     case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](s: Stream[B]): Stream[B] = 
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 0) => Some(h(), (empty, 0))
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n-1))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWithViaUnfold[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this,s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some(f(h1(),h2()), (t1(),t2()))
      case _ => None
    }

  def zipAllViaUnfold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this,s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some((Some(h1()),Some(h2())), (t1(),t2()))
      case (Cons(h1,t1), _) => Some((Some(h1()),None), (t1(), Empty))
      case (_, Cons(h2,t2)) => Some((None,Some(h2())), (Empty, t2()))
      case _ => None
    }

  def startsWith[B](s: Stream[B]): Boolean = {
    zipAllViaUnfold(s).takeWhileViaUnfold(!_._2.isEmpty) forAll {
      case (h1,h2) => h1 == h2
    }
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = cons(a, tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] = {
      cons(n1, go(n2, n1+n2))
    }

    go(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Empty
      case Some((a,s)) => cons(a, unfold(s)(f))
    }
  }

  def onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1,1)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a,a)))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(x => Some((x,x+1)))

  def fibsViaUnfold: Stream[Int] = unfold((0,1)) { case (f0,f1) => Some((f0, (f1, f0+f1))) }
}
