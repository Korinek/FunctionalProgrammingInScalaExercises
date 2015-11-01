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
      val (i, r) = rng.nextInt
        (i.toDouble / (Int.MaxValue + 1), r)
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
}
