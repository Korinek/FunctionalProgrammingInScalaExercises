
object MyModule {
	
	def isSorted[A](as: Array[A], ordered: (A, A) => Boolean) : Boolean = {
		def loop(n: Int): Boolean = {
			if (n <= 0) true
			else ordered(as(n), as(n-1)) && loop(n-1)
		}

		loop(as.length - 1)
	}
}
