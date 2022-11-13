//Simpson 1/3 compuesta
def integralCompuesta(a : Int, b : Int, n :  Int, f: Double => Double): Double = {
  val h = (b-a)/n
  val xj = (j : Double) => a + (j*h)
  val fun = (j : Double) => f(xj(2*j-2))+4*f(xj(2*j-1))+f(xj(2*j))
  //Rango n/2
  (1 to 2).map(fun(_))(h/3)
}