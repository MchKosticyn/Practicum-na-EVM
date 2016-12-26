/**
  * Created by Asus on 26.12.2016.
  */
package BarberShopP

import scala.collection.mutable
import scala.io.Source

class CheckLog(visitors : Int, barbers : Int, sizeOfSofa : Int) {

  var barbersState = mutable.Map[Long, Long]()
  var atSofaNow = 0

  def getInfo() : List[String] = {
    Source.fromFile("log.txt").getLines().toList
  }

  def parseStr(str : String) : Unit = {
    val info = str.split(' ')
    val actor = info(0)
    val id = info(1).toLong

    actor match {
      case "Visitor" =>
        if (info(2) == "at")
          atSofaNow += 1

      case "Barber"  =>
        if (info(2) == "take") {
          if (barbersState.contains(id)) {
            if (barbersState.apply(id) == -1) {
              barbersState(id) = info(3).toLong
              atSofaNow -= 1
            }
            else {
              println("Double visitors")
              System.exit(0)
            }
          }
          else {
            barbersState update (id, info(3).toLong)
            atSofaNow -= 1
          }
        }
        else {
          if (barbersState.apply(id) != -1) {
            barbersState(id) = -1
          }
          else {
            println("Free barber done work")
            System.exit(0)
          }
        }
      case _ => {
        println("Unknown log")
        System.exit(0)
      }
    }

    if (atSofaNow > sizeOfSofa) {
      println("Sofa overflow")
      System.exit(0)
    }

    if (atSofaNow < 0) {
      println("Taking from empty sofa")
      System.exit(0)
    }

  }

  def main(): Unit = {
    getInfo().map(x => parseStr(x))
    println("Errors is log:")
  }

}
