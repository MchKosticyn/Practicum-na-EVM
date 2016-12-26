/**
  * Created by Asus on 25.12.2016.
  */
package BarberShopP

import java.io.{File, PrintWriter}

import scala.util.Random

object BarberShop {

  var lock: AnyRef = new Object()

  class Logger(file_name: String) {
    private val file = new PrintWriter(new File(file_name))

    def close(): Unit = {
      file.close()
    }

    def write(msg: String): Unit = {
      file.write(msg + '\n')
    }
  }


  class Sofa(max: Int) {

    var Divan: Array[Int] = new Array(max)

    @volatile var Tail = 0
    @volatile var Head = 0
    @volatile var Count = 0


    def IsEmpty = (Head == Tail) && (Count == 0)

    def IsFull = (Head == Tail) && (Count > 0)

    val EmptySofa: Array[Int] = Array(-1)

    def Add(Id: Int) = {
      lock.synchronized {
        if (IsFull) EmptySofa
        else {
          Tail += 1
          if (Tail >= max) Tail = 0
          Divan(Tail) = Id
          Count += 1
        }
      }
    }

    def Recieve: Int =
      lock.synchronized {
        if (IsEmpty)
          -1
        else {
          val t = Divan(Head)
          Head += 1
          if (Head >= max) Head = 0
          Count -= 1
          t
        }
      }

  }

  @volatile var VisitorCount = 0

  def WaitTillTheEnd() =
    while (VisitorCount > 0) {}

  def GenVisit(M: Int, SofaS: Sofa, log: Logger) = {
    VisitorCount = M
    var S = 0
    val r = new Random
    for (i <- 0 until M) {
      val Vstr = new Visitor(i, S, SofaS, log)
      Vstr.me.start()
      S += math.abs(r.nextInt() % 100)
    }
  }

  def SetVol(x: Int): Int = lock.synchronized {
    x
  }

  class Barber(Id: Int, D: Sofa, log: Logger) {
    var VID = -1
    val me = new Thread {
      override def run(): Unit =
        while (VisitorCount > 0) {
          while (TakeVisitor(D, log)) {}
          Thread.sleep(300)
          Work(D, log)
        }
    }

    def TakeVisitor(D: Sofa, log: Logger): Boolean = lock.synchronized {
      if (!D.IsEmpty) {
        VisitorCount = SetVol(VisitorCount - 1)
        VID = D.Recieve
        log.write("Barber " + Id + " take " + VID)
        false
      }
      else true
    }

    def Work(D: Sofa, log: Logger) = lock.synchronized {
      log.write("Barber " + Id + " done " + VID)
    }
  }

  class Visitor(Id: Int, SleepTime: Int, D: Sofa, log: Logger) {
    val me = new Thread {
      override def run(): Unit = {
        Thread.sleep(SleepTime)
        lock.synchronized {
          if (D.IsFull) {
            log.write("Visitor " + Id + " go back")
            VisitorCount -= 1
          }
          else {
            D.Add(Id)
            log.write("Visitor " + Id + " at sofa ")
          }
        }
      }
    }
  }

  val visitors: Int = {
    println("Enter visitors number")
    scala.io.StdIn.readInt()
  }

  val barbers: Int = {
    println("Enter barbers number")
    scala.io.StdIn.readInt()
  }

  val size: Int = {
    println("Enter sofa size")
    scala.io.StdIn.readInt()
  }

  def main(args: Array[String]): Unit = {

    val log = new Logger("log.txt")
    val sf = new Sofa(size)

    for (i <- 1 to barbers) {
      val Brbr = new Barber(i, sf, log)
      Brbr.me.start()
    }
    GenVisit(visitors, sf, log)


    WaitTillTheEnd()

    log.close()

    new CheckLog(visitors, barbers, size).main()
  }

}