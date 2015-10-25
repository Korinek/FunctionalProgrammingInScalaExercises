sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]) : Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }
    
    def product(ds: List[Double]) : Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }

    def tail[A](l: List[A]) : List[A] = l match {
        case Nil => sys.error("tail of empty list")
        case Cons(_, xs) => xs
    }

    def drop[A](l: List[A], n: Int) : List[A] = {
        if (n <= 0) l
        else l match {
            case Nil => Nil
            case Cons(_, xs) => drop(xs, n-1)
        }
    }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case Cons(h, t) if f(h) => dropWhile(t, f)
        case _ => l
    }

    def setHead[A](newHead: A, as: List[A]) = as match {
        case Nil => sys.error("setHead of empty list")
        case Cons(_, xs) => Cons(newHead, xs)
    }

    def init[A](l: List[A]) : List[A] = l match {
        case Nil => sys.error("init on empty list")
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))  
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B) : B = as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }


    def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B) : B = {
        foldLeft(reverse(as), z)((b,a) => f(a,b))
    }

    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B) : B = as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B) : B = {
        foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
    }

    def append[A](a: List[A], l: List[A]) : List[A] = {
        foldRight(l, a)(Cons(_,_))
    }

    def flatten[A](ll: List[List[A]]) : List[A] = {
        foldRight(ll, Nil:List[A])(append)
    }

    def add1(l: List[Int]) : List[Int] = {
        foldRight(l, Nil:List[Int])((h, t) => Cons(h+1, t))
    }

    def doubleToString(l: List[Double]) : List[String] = {
        foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))
    }

    def map[A,B](l: List[A])(f: A => B): List[B] = {
        foldRight(l, Nil:List[B])((h, t) => Cons(f(h), t))
    }

    def filter[A](l: List[A])(p: A => Boolean): List[A] = {
        foldRight(l, Nil:List[A])((h, t) => if (p(h)) Cons(h, t) else t) 
    }

    def sum2(ns: List[Int]) =
        foldRight(ns, 0)((x,y) => x + y)

    def product2(ns: List[Int]) =
        foldRight(ns, 1.0)((x,y) => x * y)

    def length[A](as: List[A]): Int = {
        foldRight(as, 0)((_, acc) => 1 + acc)
    }

    def sum3(l: List[Int]) =
        foldLeft(l, 0)(_ + _)

    def product3(l: List[Int]) =
        foldLeft(l, 1.0)(_ * _)

    def length2[A](l: List[A]) : Int = {
        foldLeft(l, 0)((acc, _) => 1 + acc)
    }

    def reverse[A](l: List[A]) : List[A] = {
        foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
    }

    def apply[A](as: A*) : List[A] = {
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    }
}
