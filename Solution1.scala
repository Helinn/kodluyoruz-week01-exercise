val < = (a:Int, b: Int) => a < b
val > = (a:Int, b: Int) => a > b

def calculate(ints : List[Int], temp: Int, operator: (Int, Int) => Boolean) : Int = {
	ints match{
		case Nil => temp
		case x :: tail =>
		val newValue = if(operator(x,temp)) temp else x
		calculate(tail,newValue,operator)
	}
}

def distBetweenMaxAndmin(ints: List[Int]) : Int = {calculate(ints,ints.head,<)-calculate(ints,ints.head,>)}
