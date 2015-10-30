package fpinscala.laziness

import Stream._
trait Stream[+A] {

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }
   
    def toListRecursive: List[A] = this match {
        case Cons(h, t) => h()::t().toListRecursive
        case _ => List()
    }

    def toList: List[A] = {
        @annotation.tailrec
        def go(s: Stream[A], acc:List[A]): List[A] = s match {
            case Cons(h, t) => go(t(), h()::acc)
            case _ => acc
        }

        go(this, List()).reverse
    }

    def take(n: Int): Stream[A] = this match {
        case Cons(h, t) if n > 1  => cons(h(), t().take(n-1)) 
        case Cons(h, _) if n == 1 => cons(h(), empty)
        case _ => empty
    }

    @annotation.tailrec
    final def drop(n: Int): Stream[A] = this match {
        case Cons(_, t) if n > 0 => t().drop(n-1)
        case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
        case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
        case _ => empty
    }

    def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
        foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)
    }

    def exists(p: A => Boolean): Boolean = {
        foldRight(false)((a, b) => p(a) || b)
    }

    def forAll(p: A => Boolean): Boolean = {
        foldRight(true)((a, b) => p(a) && b)
    }

    def headOption: Option[A] = {
        foldRight(None: Option[A])((h, _) => Some(h))
    }

    def map[B](f: A => B): Stream[B] = {
        foldRight(empty[B])((h,t) => cons(f(h), t))
    }

    def filter(p: A => Boolean): Stream[A] = {
        foldRight(empty[A])((h,t) => if (p(h)) cons(h,t) else t)
    }

    def append[B>:A](s: => Stream[B]): Stream[B] = {
        foldRight(s)((h,t) => cons(h,t))
    }

    def flatMap[B](f: A => Stream[B]): Stream[B] = {
        foldRight(empty[B])((h,t) => f(h) append t)
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

    def apply[A](as: A*): Stream[A] = {
        if (as.isEmpty) empty
        else cons(as.head, apply(as.tail: _*))
    }

    def ones: Stream[Int] = cons(1, ones)

    def constant[A](x: A): Stream[A] = {
        lazy val tail: Stream[A] = Cons(() => x, () => tail)
        tail
    }

    def from(n: Int): Stream[Int] = {
        cons(n, from(n+1))
    }

    def fibs: Stream[Int] = {
        def go(f0: Int, f1: Int): Stream[Int] = {
            cons(f0, go(f1, f0+f1)) 
        }
        go(0, 1) 
    }

    def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = f(z) match {
        case Some((h,s)) => cons(h, unfold(s)(f))
        case None => empty
    }

    def onesViaUnfold: Stream[Int] = {
        unfold(1)(_ => Some(1,1))
    }

    def constantViaUnfold[A](a: A): Stream[A] = {
        unfold(a)(_ => Some(a,a)) 
    }

    def fromViaUnfold(n: Int): Stream[Int] = {
        unfold(n)(n => Some(n, n+1))
    }

    def fibsViaUnfold: Stream[Int] = {
        unfold((0,1)) { case (f0,f1) => (Some(f0, (f1, f0+f1))) }
    }
}
