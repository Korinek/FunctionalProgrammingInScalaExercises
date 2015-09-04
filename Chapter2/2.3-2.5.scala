object MyModule {

	def partial[A,B,C](a: A, f: (A,B) => C): B => C = {
		(b: B) => f(a, b)
	}	

	def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
		(a: A) => partial(a, f)

	}

	def uncurry[A,B,C](f: A => B => C) : (A, B) => C = {
		(a:A, b:B) => f(a)(b)
	}
}
