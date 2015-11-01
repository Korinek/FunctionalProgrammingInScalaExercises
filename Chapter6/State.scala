package fpinscala.state

trait RNG {
    def nextInt: (Int, RNG)
}

object RNG {
    
    case class Simple(seed: Long) extends RNG {
        def nextInt: (Int, RNG) = {
            val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
            val nextRNG = Simple(newSeed)
            val n = (newSeed >>> 16).toInt
            (n, nextRNG)
        }
    }

    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] =
        rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
        rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (i, r) = rng.nextInt
        (if (i<0) -(i+1) else i, r)
    }

    def double(rng: RNG): (Double, RNG) = {
        val (i, r) = nonNegativeInt(rng)
        (i / (Int.MaxValue.toDouble + 1), r)
    }

    def _double(rng: RNG): Rand[Double] = {
      map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (i, r2) = rng.nextInt
        val (d, r3) = double(rng)
        ((i, d), r3)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val ((i, d), r) = intDouble(rng)
        ((d, i), r)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val (d1, r2) = double(rng)
        val (d2, r3) = double(rng)
        val (d3, r4) = double(rng)
        ((d1, d2, d3), r4)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        if (count <= 0) (List(), rng)
        else {
            val (x, r1) = rng.nextInt
            val (xs, r2) = ints(count - 1)(r1)
            (x::xs, r2)
        }
    }

    def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
        def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = {
            if (count <= 0) (xs, r)
            else {
                val (x, r2) = r.nextInt
                go(count - 1, r2, x :: xs)
            }
        }

        go(count, rng, List())
    }

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
        rng => {
            val (a, r1) = ra(rng)
            val (b, r2) = rb(r1)
            (f(a,b), r2)
        }
    }

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
        map2(ra, rb)((_, _))

    def sequence[A](fa: List[Rand[A]]): Rand[List[A]] = {
        fa.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
    }

    def _ints(count: Int): Rand[List[Int]] = {
        sequence(List.fill(count)(int))
    }

    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
        rng => {
            val (a, r1) = f(rng)
            g(a)(r1)
        }
    }

    def nonNegativeLessThan(n: Int): Rand[Int] = {
        flatMap(nonNegativeInt) { i => 
            val mod = i % n
            if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(mod)
        }
    }
}
