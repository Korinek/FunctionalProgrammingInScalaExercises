
object MyModule {

	def fib(n: Int): Int = {
		def go(n: Int): Int = {
			if (n <= 1) 0
			else if (n == 2) 1
			else go(n-1) + go(n-2)
		}
	
		go(n)
	}
}
