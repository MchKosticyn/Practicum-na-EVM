/**
  * Created by Asus on 21.11.2016.
  */
object pasc {

  def fac(n : Int) : Int ={
    if (n<=1)
      1
    else n * fac(n-1)
  }

  def sochet(i : Int,j : Int) = fac(i)/(fac(j)*fac(i-j))

  def main(args: Array[String]): Unit = {
    print(sochet(4,2))
  }
}
