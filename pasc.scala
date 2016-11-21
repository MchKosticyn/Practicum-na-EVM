/**
  * Created by Asus on 21.11.2016.
  */
object pasc {

  def fac(n : Int) ={
    var r = 1
    for (i <- 1 to n)
      r = r * i
    r
  }

  def sochet(i : Int,j : Int) = fac(i)/(fac(j)*fac(i-j))

  def main(args: Array[String]): Unit = {
    print(sochet(4,2))
  }
}
