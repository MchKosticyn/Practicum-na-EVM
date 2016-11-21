/**
  * Created by Asus on 21.11.2016.
  */
object bracers {
  def correct(s:List[Char]) ={
    var l = 0
    for (e <- s) {
      if (e == '(')
        l += 1
      if (e == ')')
        l -= 1
    }
    if (l==0)
      true
    else
      false
  }
  def main(args: Array[String]): Unit = {
   if (correct(List('(',')')))
     print("""Correct!""")
   else  print("""Not correct!""")
  }
}
