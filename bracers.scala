/**
  * Created by Asus on 21.11.2016.
  */
object bracers {
  def correct(s:List[Char], n : Int = 0):Boolean ={
    //var f : Boolean = false
    if (s.isEmpty)
      {
        if (n==0)true
        else false
      }
    else {
      if (n>=0) {
        if (s.head == '(')
          correct(s.drop(1), n + 1)
        else correct(s.drop(1), n - 1)
      }
        else false
    }
  }
  def main(args: Array[String]): Unit = {
   if (correct(readLine("Write a string: ").toList,0))
     print("""Correct!""")
   else  print("""Not correct!""")
  }
}
