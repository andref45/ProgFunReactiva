//Funciones
val f = (x : Double) => -Math.pow(x,2)+(8*x)-12
val h = (x : Double) => 3*Math.pow(x,2)
val i = (x : Double) => x+2*Math.pow(x,2)- Math.pow(x,3)+5*Math.pow(x,4)
val j = (x : Double) => ((2*x+1)/(Math.pow(x,2)+x))
val k = (x : Double) => Math.pow(Math.E,x)
val l = (x : Double) => (1/Math.sqrt(x-1))
val m = (x : Double) => (1/(1+Math.pow(x,2)))

//Simpson 1/3 compuesta
def integralCompuesta(a : Int, b : Int, n :  Int, f: Double => Double): Double = {
  val h = (b-a)/n
  val xj = (j : Double) => a + (j * h)
  val fun = (j : Double) => f(xj(2 * j - 2))+ 4 * f(xj(2 * j - 1))+ f(xj(2 * j))
  //Rango n/2
  (1 to 2).map(fun(_))(h/3)

}




