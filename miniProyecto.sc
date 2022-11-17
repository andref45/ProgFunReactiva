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
//Simpson compuesta
def integralCompuesta(a: Int, b: Int, n: Int, f: Double => Double): Double = {
  val h = (b - a) / n
  val xj = (j: Double) => a + (j * h)
  val fun = (j: Double) => f(xj(2 * j - 2)) + 4 * f(xj(2 * j - 1)) + f(xj(2 * j))
  (h/3) * (1 to (n/2)).map(fun(_)).sum
}
val ic1 = integralCompuesta(3,5,2,f)
val ic2 = integralCompuesta(0,2,6,h)
val ic3 = integralCompuesta(-1,1,4,i)
val ic4 = integralCompuesta(1,2,8,j)
val ic5 = integralCompuesta(0,1,10,k)
val ic6 = integralCompuesta(2,3,12,l)
val ic7 = integralCompuesta(0,1,2,m)


//Simpson 1/3 extendida
def integralCompExtendida(a : Double, b : Double, f: Double => Double) : Double = {
  val n = (2 * (b - a)).toInt
  val h = (b-a)/n
  val i = (1 until n by 2).toList
  val j = (2 to n-2 by 2).toList
  val sumI = i.map(x => f(a + x * h)).sum
  val sumJ = j.map(x => f(a + x * h)).sum
  (h/3) * (f(a) + (4 * sumI) + (2 * sumJ) + f(b))
}
val ie1 = integralCompExtendida(3,5,f)
val ie2 = integralCompExtendida(0,2,h)
val ie3 = integralCompExtendida(-1,1,i)
val ie4 = integralCompExtendida(1,2,j)
val ie5 = integralCompExtendida(0,1,k)
val ie6 = integralCompExtendida(2,3,l)
val ie7 = integralCompExtendida(0,1,m)

def err(a : Double, b : Double) : Double = (a - b).abs
//Error Simpson 1/3 compuesta
err(v01, ic1)
err(v02, ic2)
err(v03, ic3)
err(v04, ic4)
err(v05, ic5)
err(v06, ic6)
err(v07, ic7)
//Error Simpson 1/3 extendida
err(v01, ie1)
err(v02, ie2)
err(v03, ie3)
err(v04, ie4)
err(v05, ie5)
err(v06, ie6)
err(v07, ie7)

