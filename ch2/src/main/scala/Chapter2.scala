object Chapter2 { 
	// 2.1 피보나치수열 작성
	def fib( n: Int ) : Int = {
		def go( n: Int, x: Int, y: Int) : Int = {
			if( n == 0 ) 0
			else if( n == 1 ) x
			else go( n-1, y, x+y )
		}

		go( n, 0, 1 )
	}

	// 2.2 정렬함수 구현 
	def isSorted[A]( as: Array[A], ordered: (A,A) => Boolean ) : Boolean = {

		def loop( n: Int ) : Boolean = {
			if ( n >= as.length-1 ) true
			else if ( ordered( as(n), as(n+1) ) == false ) false 
			else loop( n + 1 )
		}

		loop( 0 )
	}

	// 2.3 커링 ( 어렵다 ㅜㅜ )
	def curry[A,B,C]( f: (A, B) => C ): A => (B => C ) = {
		(a: A) => (b: B) => f(a, b)
	}

	// 2.4 언커링
	def uncurry[A,B,C]( f: A => B => C ): (A, B) => C = 
		(a : A, b : B) => f(a)(b)

    // 2.5 함수합성하는 고차원 함수 ( 커링이용 )
	def compose[A,B,C](f: B => C, g: A => B): A => C = {
		(a: A) => f( g(a) )
	}

	def main(args: Array[String]) = {
		println( fib(5) )

		println( "== 2.2 Solve ==" )
		println( isSorted( Array(1,2,3,4), (x:Int,y:Int) => x<y ) )
	}
}
