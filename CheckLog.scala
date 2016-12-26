/**
  * Created by Asus on 26.12.2016.
  */
package BarberShopP

import scala.collection.mutable
import scala.io.Source

object CheckLog {

  var AtSofaNow = 0
  var Barbers: Array[Int] = Array()
  val getInfo: List[String] = Source.fromFile("log.txt").getLines().toList

  def parseStr(str: String, visitors: Int, barbers: Int, size: Int): (Boolean, String) = {
    val A = str.split(' ')
    var l = 0
    if (A.contains("Enq")) l = 1
    else if (A.contains("deq")) l = 2
    else if (A.contains("done")) l = 3
    else if (A.contains("back")) l = 4
    l match {
      case 0 =>
        (false, "Wrong log")
      case 1 => {
        val ID = A(1)
        AtSofaNow += 1
        if (AtSofaNow > size)
          (false, "Sofa overflow!")
        else (true, "")
      }
      case 2 => {
        val ID = A(0).toInt
        val VID = A(2).toInt
        AtSofaNow -= 1
        Barbers(ID - 1) += 1
        if (Barbers(ID - 1) > 1)
          (false, "Chair overflow!")
        else (true, "")
      }
      case 3 => {
        val ID = A(0).toInt
        val VID = A(2).toInt
        Barbers(ID - 1) -= 1
        if (Barbers(ID - 1) < 0)
          (false, "Not done yet!")
        else (true, "")
      }
      case 4 =>
        if (AtSofaNow < size)
          (false, "Wrong direction!")
        else (true, "")
    }
  }

  def StartCheck(visitors: Int, barbers: Int, SizeOfSofa: Int): Unit = {
    Barbers = new Array(barbers)
    Barbers = Barbers.map(x => 0)
    println("Log checker started...")
    val BList = getInfo.map(parseStr(_, visitors, barbers, SizeOfSofa)).zipWithIndex
    BList.foreach(x =>
      if (x._1._1 == false) println("Error in line " + (x._2+1) + " ( " + x._1._2 + " )" )
    )
    if (!BList.map(_._1._1).contains(false))
      println("Clear!")
  }

}
