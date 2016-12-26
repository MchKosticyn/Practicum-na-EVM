/**
  * Created by Asus on 25.12.2016.
  */
package BarberShopP

import java.io.{File, PrintWriter}
import java.util.concurrent.CountDownLatch
import scala.util.Random

object BarberShop {

  var lock: AnyRef = new Object()

  class Logger(file_name: String) {
    private val file = new PrintWriter(new File(file_name))

    def close(): Unit = {
      file.close()
    }

    def write(msg: String): Unit = lock.synchronized {
      file.write(msg + '\n')
    }
  }


  class Sofa(max: Int) {

    var Divan: Array[Int] = new Array(max)

    def init_() = (0 until Divan.length) foreach (Divan(_) = -2)


    @volatile var Tail = 0
    @volatile var Head = 0
    @volatile var Count = 0

    @volatile var IsEmpty = true
    //def IsEmpty = lock.synchronized((Head == Tail) && (Count == 0))

    def IsFull = lock.synchronized((Head == Tail) && (Count > 0))

    val EmptySofa: Array[Int] = Array(-1)

    def Add(Id: Int) = lock.synchronized {
      if (IsFull) EmptySofa
      else {
        if (Tail + 1 >= max)
          Divan(0) = Id
        else Divan(Tail + 1) = Id
        Tail += 1
        if (Tail >= max) Tail = 0
        Count += 1
        IsEmpty = (Head == Tail) && (Count == 0)
      }
    }

    def Recieve: Int = lock.synchronized {
      if (IsEmpty)
        -1
      else {
        Count -= 1
        Head += 1
        if (Head >= max) Head = 0
        val t = Divan(Head)
        IsEmpty = (Head == Tail) && (Count == 0)
        t
      }
    }

  }
  @volatile var WasWorked = 0
  @volatile var VisitorCount = 0
  @volatile var Flag = 0

  def WaitTillTheEnd() =
    while (TakeVol(WasWorked) < visitors) {}

  def GenVisit(M: Int, SofaS: Sofa, log: Logger) = lock.synchronized {
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

  def TakeVol(x: Int): Int = lock.synchronized {
    x
  }

  class Barber(Id: Int, D: Sofa, log: Logger) {
    var VID = -1
    val me = new Thread {
      override def run(): Unit =
        while (TakeVol(VisitorCount) > 0) {
          while (TakeVisitor(D, log)) {}
          Thread.sleep(300)
          Work(D, log)
          /*if (TakeVol(VisitorCount) == 0)
            Flag = SetVol(1)*/
        }
    }

    def TakeVisitor(D: Sofa, log: Logger): Boolean = lock.synchronized {
      if (!D.IsEmpty) {
        VisitorCount = SetVol(VisitorCount - 1)
        VID = D.Recieve
        log.write(Id + " deq " + VID)
        false
      }
      else true
    }

    def Work(D: Sofa, log: Logger) = lock.synchronized {
      log.write(Id + " done " + VID)
      WasWorked = SetVol(WasWorked + 1)
    }
  }

  class Visitor(Id: Int, SleepTime: Int, D: Sofa, log: Logger) {
    val me = new Thread {
      override def run(): Unit = {
        Thread.sleep(SleepTime)
        lock.synchronized {
          if (D.IsFull) {
            log.write(Id + " back")
            VisitorCount = SetVol(VisitorCount - 1)
            WasWorked = SetVol(WasWorked + 1)
          }
          else {
            D.Add(Id)
            log.write("Enq " + Id)
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
    sf.init_()

    VisitorCount = SetVol(visitors)

    for (i <- 1 to barbers) {
      val Brbr = new Barber(i, sf, log)
      Brbr.me.start()
    }
    GenVisit(visitors, sf, log)

    WaitTillTheEnd()

    log.close()

    CheckLog.StartCheck(visitors, barbers, size)

    //new CheckLog(visitors, barbers, size).main()
  }

}