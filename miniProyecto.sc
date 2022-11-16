//Funciones
val f = (x : Double) => -Math.pow(x,2)+(8*x)-12
val h = (x : Double) => 3*Math.pow(x,2)
val i = (x : Double) => x+2*Math.pow(x,2)- Math.pow(x,3)+5*Math.pow(x,4)
val j = (x : Double) => (2*x+1)/Math.pow(x,2)+x
val k = (x : Double) => Math.pow(Math.E,x)
val l = (x : Double) => 1/Math.sqrt(x-1)
val m = (x : Double) => 1/(1+Math.pow(x,2))

//Resultados de las integrales
val v01 = 7.33
val v02 = 8
val v03 = 3.333
val v04 = 1.09861
val v05 = 1.71828
val v06 = 0.828427
val v07 = 0.785398

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
def integralCompExtendida(a:Double, b:Double, f:Double=>Double) : Double = {
  val n = (2 * (b - a)).toInt
  val h = (b-a)/n
  val i = (1 to n-1 by 2).toList
  val j = (2 to n-2 by 2).toList
  val sumI = i.map(x => f(a + x * h)).sum
  val sumJ = j.map(x => f(a + x * h)).sum
  (h/3) * (  f(a) +  (4 * sumI) + (2 * sumJ) +  f(b)  )
}
val ie1 = integralCompExtendida(3,5,f)
val ie2 = integralCompExtendida(0,2,h)
val ie3 = integralCompExtendida(-1,1,i)
val ie4 = integralCompExtendida(1,2,j)
val ie5 = integralCompExtendida(0,1,k)
val ie6 = integralCompExtendida(2,3,l)
val ie7 = integralCompExtendida(0,1,m)

def err(a : Double, b : Double) : Double = (a - b).abs
