/**
  * Created by Asus on 23.11.2016.
  */
object Money {
  def  Count(sum : Int, coins: List[Int]) : Int = {
    var S = 0
    if (sum == 0)
      S = 1
    if (sum > 0)
        for (i <- coins)
          S+=Count(sum - i, coins.filter(_>=i))
    S
  }

  def main(args: Array[String]): Unit = {
    val S = readLine("Write an acount of money: ").toInt
    val c : List[Int] = readLine("Write a list ").split(' ').map(_.toInt).toList
    print(Count(S, c))
  }
}
