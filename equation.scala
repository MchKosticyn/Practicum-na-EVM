import java.lang.Math._

/**
  * Created by Asus on 24.11.2016.
  */

object equation {
  def Solution(coef : Array[Double]) : List[Double] = {
    val a = coef(0)
    val b = coef(1)
    val c = coef(2)
    var lst : List[Double] = List()
    if (coef.length == 4) {
      val d = coef(3)
      var p = (3 * a * c - b*b) / 3 * a * a
      val q = (2*b*b*b - 9*a*b*c + 27*a*a*d)/27*a*a*a
      var Q = pow(p/3,3) + pow(q/2,2)
      if ((Q > - 1.0E-12)&&(Q < 1.0E-12))
        Q=0
      val vs1 = -q/2 + pow(Q,1.0/2)
      var alfa : Double = 0.0
      if (vs1 < 0)
        alfa = - pow(-vs1,1.0/3)
      else alfa = pow(vs1,1.0/3)
      val vs2 = -q/2 - pow(Q,1.0/2)
      var beta : Double = 0.0
      if (vs2 < 0)
         beta = -pow(-vs2,1.0/3)
      else beta = pow(vs2,1.0/3)
      if (Q>0)
        lst = alfa + beta :: lst
      if (Q==0){
        if ((p==q)&&(p==0))
          lst = - b/3*a :: lst
        else {
          if (-q / 2 < 0)
            lst = -2 * pow(q / 2, 1.0 / 3) - b / 3 * a :: lst
          else lst = 2 * pow(-q / 2, 1.0 / 3) - b / 3 * a :: lst
          if (-q / 2 < 0)
            lst = pow(q / 2 , 1.0 / 3) - b / 3 * a :: lst
          else lst = -1 * pow(-q / 2 , 1.0 / 3) - b / 3 * a :: lst
        }
      }
      if (Q<0){
        var fi : Double = 0
        if (q>0)
          fi = atan(sqrt(-Q)/(-q/2))
        if (q<0)
          fi =atan(sqrt(-Q)/(-q/2)) + PI
        if (q==0)
          fi = PI/2
        lst = 2*sqrt(-p/3)*cos(fi/3)-b/3*a :: lst
        lst = 2*sqrt(-p/3)*cos(fi/3+(2*PI)/3)-b/3*a :: lst
        lst = 2*sqrt(-p/3)*cos(fi/3+(4*PI)/3)-b/3*a :: lst
      }
      lst
    }
    else {
      val D = pow(b,2) - 4*a*c
      if (D == 0)
        lst = -b/2*a :: lst
      if (D>0){
        lst = (-b-sqrt(D))/2*a :: lst
        lst = (-b+sqrt(D))/2*a :: lst
      }
      lst
    }
  }
  def pars(s: String) : Array[Double] = {
    readLine("Write coefs: ").split(' ').map(_.toDouble)
  }
  def main(args: Array[String]): Unit = {
    //val S = readLine("Write an equation: ")
    print(Solution(pars("")).map(_.toString))
  }
}
