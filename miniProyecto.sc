//Funciones
val f = (x : Double) => -Math.pow(x,2)+(8*x)-12
val h = (x : Double) => 3*Math.pow(x,2)
val i = (x : Double) => x+2*Math.pow(x,2)- Math.pow(x,3)+5*Math.pow(x,4)
val j = (x : Double) => (2*x+1)/Math.pow(x,2)+x
val k = (x : Double) => Math.pow(Math.E,x)
val l = (x : Double) => 1/Math.sqrt(x-1)
val m = (x : Double) => 1/(1+Math.pow(x,2))

//Simpson 1/3 compuesta
def integralCompuesta(a : Int, b : Int, n :  Int, f : Double => Double): Double = {
  val h = (b-a)/n
  val xj = (j : Double) => a + (j * h)
  val fun = (j : Double) => f(xj(2 * j - 2))+ 4 * f(xj(2 * j - 1))+ f(xj(2 * j))
  //Rango n/2
  (h/3) * (1 to (n/2)).map(fun(_)).sum
}

integralCompuesta(3,5,f)
integralCompuesta(0,2,h)
integralCompuesta(-1,1,i)
integralCompuesta(1,2,j)
integralCompuesta(0,1,k)
integralCompuesta(2,3,l)
integralCompuesta(0,1,m)

//Simpson 1/3 extendida
def integralCompExtendida(a : Int, b : Int, f : Double => Double) : (Double, Double) => Double = {
  val n = 2 * (b - a)
  val h = (b-a)/n
  val i = n - 1
  val j = n - 2
  val fun = (i : Double, j : Double) => f(a) + 4 * f(a+i*h)+ 2 * f(a + j*h) + f(b)
  return fun
}
integralCompExtendida(3,5,f)
integralCompExtendida(0,2,h)
integralCompExtendida(-1,1,i)
integralCompExtendida(1,2,j)
integralCompExtendida(0,1,k)
integralCompExtendida(2,3,l)
integralCompExtendida(0,1,m)

def err(a : Double, b : Double) : Double = (a - b).abs
