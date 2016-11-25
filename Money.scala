/**
  * Created by Asus on 23.11.2016.
  */
object Money {
  def  Count(sum : Int, coins: List[Int]) : Int = {
    if (sum == 0) 1
    else {
      if ((sum > 0)&&coins.nonEmpty)
        Count(sum - coins.head, coins) + Count(sum, coins.tail)
      else 0
    }
  }
  def main(args: Array[String]): Unit = {
    val S = readLine("Write an acount of money: ").toInt
    val c = readLine("Write a list ").split(' ').map(_.toInt).toList
    print(Count(S, c))
  }
}
