import scala.reflect.ClassTag
import org.scalameter._

/**
  * Created by Asus on 21.12.2016.
  */
object PScan {

  private val standardConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 15,
    Key.exec.benchRuns -> 10
  ) withWarmer new Warmer.Default

  type BigInt = Array[Int]

  def parallel(a: => Unit, b: => Unit) = {
    val left = new Thread {
      override def run(): Unit = {
        b
      }
    }
    left.start()
    a
    left.join()
  }

  @volatile var thread: Int = 1

  def oneThread(A: BigInt, B: BigInt): BigInt = {
    var carry = 0
    val Res: BigInt = new Array(A.length)
    for (i <- A.indices) {
      Res(i) = (A(i) + B(i) + carry) % 10
      carry = (A(i) + B(i) + carry) / 10
    }
    if (carry > 0) {
      Res(Res.length - 1) = 1
      Res
    } else Res.init
  }

  def SumWithOneThread(A: BigInt, B: BigInt): BigInt = oneThread(A.reverse, B.reverse).reverse

  def operation(arg1: Char, arg2: Char): Char = (arg1, arg2) match {
    case ('N', 'N') => 'N'
    case ('N', 'M') => 'N'
    case ('N', 'C') => 'C'
    case ('M', 'N') => 'N'
    case ('M', 'M') => 'M'
    case ('M', 'C') => 'C'
    case ('C', 'N') => 'N'
    case ('C', 'M') => 'C'
    case ('C', 'C') => 'C'
  }

  def Swap[T](A: Array[T], i: Int, j: Int) = {
    val c = A(i)
    A(i) = A(j)
    A(j) = c
  }

  def SingleThreadC[T](A: Array[T], f: (T, T) => T, from: Int, count: Int): Unit =
    if (count == 1) A(from) = A(from)
    else {
      SingleThreadC(A, f, from, count - count / 2)
      SingleThreadC(A, f, from + count - count / 2, count / 2)
      A(from + count - 1) = f(A(from + count - 1 - count / 2), A(from + count - 1))
    }

  def scanC[T](A: Array[T], f: (T, T) => T, from: Int, count: Int, threads: Int): Unit =
    if ((thread >= threads) || (count == 1)) SingleThreadC(A, f, from, count)
    else {
      thread += 1
      parallel(scanC(A, f, from, count - count / 2, threads),
        scanC(A, f, from + count - count / 2, count / 2, threads))
      A(from + count - 1) = f(A(from + count - 1 - count / 2), A(from + count - 1))
      thread -= 1
    }

  def SingleThreadD[T](A: Array[T], f: (T, T) => T, from: Int, count: Int): Unit =
    if (count == 1) A(from) = A(from)
    else {
      Swap(A, from - 1 + count - count / 2, from + count - 1)
      A(from + count - 1) = f(A(from - 1 + count - count / 2), A(from + count - 1))
      SingleThreadD(A, f, from, count - count / 2)
      SingleThreadD(A, f, from + count - count / 2, count / 2)
    }

  def scanD[T](A: Array[T], f: (T, T) => T, from: Int, count: Int, threads: Int): Unit = {
    if ((thread >= threads) || (count == 1)) SingleThreadD(A, f, from, count)
    else {
      Swap(A, from - 1 + count - count / 2, from + count - 1)
      A(from + count - 1) = f(A(from - 1 + count - count / 2), A(from + count - 1))
      thread += 1
      parallel(scanD(A, f, from, count - count / 2, threads),
        scanD(A, f, from + count - count / 2, count / 2, threads))
      thread -= 1
    }
  }

  def PrefixScan[T: ClassTag](A: Array[T], Zero: T, f: (T, T) => T, threads: Int): Array[T] = {
    scanC(A, f, 0, A.length, threads)
    val Addit: T = A(A.length - 1)
    A(A.length - 1) = Zero
    scanD(A, f, 0, A.length, threads)
    A :+ Addit
  }


  def OneThreadCompute(A: BigInt, B: BigInt, C: Array[Char], from: Int, count: Int) =
    for (i <- from until from + count)
      if (A(i) + B(i) >= 10) C(i) = 'C'
      else if (A(i) + B(i) == 9) C(i) = 'M'
      else C(i) = 'N'

  def ComputeExtraArray(A: BigInt, B: BigInt, C: Array[Char], from: Int, count: Int, threads: Int): Unit = {
    if ((thread >= threads) || (count == 1)) OneThreadCompute(A, B, C, from, count)
    else {
      thread += 1
      parallel(ComputeExtraArray(A, B, C, from, count / 2, threads),
        ComputeExtraArray(A, B, C, from + count / 2, count - count / 2, threads))
      thread -= 1
    }
  }

  def CExtraArray(A: BigInt, B: BigInt, threads: Int): Array[Char] = {
    val Extr: Array[Char] = new Array(A.length)
    ComputeExtraArray(A, B, Extr, 0, A.length, threads)
    Extr
  }

  def ToArray(arg1: Char) = arg1 match {
    case 'C' => 1
    case 'N' => 0
  }

  def Shr[T](A: Array[T], z: T): Array[T] = {
    for (i <- A.length - 1 to 1 by -1) A(i) = A(i - 1)
    A(0) = z
    A
  }

  def Shl[T](A: Array[T]): Array[T] = {
    for (i <- 0 until A.length - 1) A(i) = A(i + 1)
    A
  }


  def OneThreadSum(A: BigInt, B: BigInt, C: BigInt, Res: BigInt, from: Int, count: Int) =
    for (i <- from until from + count)
      Res(i) = (A(i) + B(i) + C(i)) % 10


  def Sum(A: BigInt, B: BigInt, C: BigInt, Res: BigInt, from: Int, count: Int, threads: Int): Unit = {
    if ((thread >= threads) || (count == 1)) OneThreadSum(A, B, C, Res, from, count)
    else {
      thread += 1
      parallel(Sum(A, B, C, Res, from, count / 2, threads),
        Sum(A, B, C, Res, from + count / 2, count - count / 2, threads))
      thread -= 1
    }
  }

  val l = List(1, 2, 4, 8, 16, 32, 64) //threads

  // Big Integers
  def BigSum(A: BigInt, B: BigInt, threads: Int): BigInt = {
    //FirstArray = 0 +: scala.io.StdIn.readLine().toCharArray.map(_.toInt.-(48))
    //SecondArray = 0 +: scala.io.StdIn.readLine().toCharArray.map(_.toInt.-(48))
    var ExtraArray = CExtraArray(A, B, threads)
    ExtraArray = PrefixScan(ExtraArray, 'M', operation, 1)
    ExtraArray = ExtraArray.tail
    val FinalA: BigInt = new Array(ExtraArray.length)
    Sum(A, B, Shr(ExtraArray.map(ToArray), 0), FinalA, 0, FinalA.length, threads)
    if (FinalA(FinalA.length - 1) == 0) FinalA.init
    else FinalA
  }

  def BigIntTime() = {
    println("{--------------Big integers---------------}")
    val FstAr = 0 +: Array(9, 1, 3, 2, 6)
    val SndAr = 0 +: Array(1, 7, 4, 5, 4)

    val seqtime = standardConfig measure {
      SumWithOneThread(FstAr.reverse, SndAr.reverse)
    }
    println("Sequential time: " + seqtime.value)

    for (threads <- l) {
      val partime = standardConfig measure {
        BigSum(FstAr.reverse, SndAr.reverse, threads)
      }
      println("Parallel time with " + threads + " threads: " + partime.value)
    }
  }

  // N-th term sequence
  type Pair = (Double, Double)

  def operationS(a: Pair, b: Pair) =
    (a._1 * b._1, b._1 * a._2 + b._2)

  def NthTerm(A: Array[Pair], threads: Int): Double = {
    scanC[Pair](A, operationS, 0, A.length, threads)
    A.last._2
  }

  def OneThreadTerm(A: Array[Pair]): Double = {
    var x: Double = A(0)._2
    for (i <- 1 until A.length)
      x = A(i)._1 * x + A(i)._2
    x
  }

  def NthTermTime() = {
    println("{--------------N-th term of the sequence---------------}")
    val A: Array[Pair] = Array((1.0, 2.0), (3.0, 1.0), (2.0, 3.0), (6.0, 4.0), (2.0, 7.0))
    val seqtime = standardConfig measure {
      OneThreadTerm(A)
    }
    println("Sequential time: " + seqtime.value)

    for (threads <- l) {
      val partime = standardConfig measure {
        NthTerm(A, threads)
      }
      println("Parallel time with " + threads + " threads: " + partime.value)
    }


  }

  // Bracers

  def OneThreadBrac(b: Array[Char]): Boolean = {
    def correct(s: List[Char], n: Int = 0): Boolean = {
      //var f : Boolean = false
      if (s.isEmpty) {
        if (n == 0) true
        else false
      }
      else {
        if (n >= 0) {
          if (s.head == '(')
            correct(s.drop(1), n + 1)
          else correct(s.drop(1), n - 1)
        }
        else false
      }
    }
    correct(b.toList, 0)
  }

  def BracersTime() = {
    println("{--------------Brackets--------------}")
    val A = "(()()(()))".toCharArray
    val seqtime = standardConfig measure {
      OneThreadBrac(A)
    }
    println("Sequential time: " + seqtime.value)

    for (threads <- l) {
      val partime = standardConfig measure {
        Brac(A, threads)
      }
      println("Parallel time with " + threads + " threads: " + partime.value)
    }
  }

  def toTuple(c: Char): (Int, Int) = c match {
    case '(' => (0, 1)
    case ')' => (1, 0)
  }

  def max(a: Int, b: Int): Int = if (a > b) a else b

  def operationB(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    (math.max(a._1 + b._1 - a._2, 0), math.max(a._2 - b._1 + b._2, 0))
  }

  def Brac(A: Array[Char], threads: Int): Boolean = {
    val B = A.map(toTuple)
    scanC(B, operationB, 0, A.length, threads)
    (B.last._1 == 0) && (B.last._2 == 0)
  }


  // Turtle

  def asin(x: Double): Double =
    180 * Math.asin(x) / math.Pi

  def cos(x: Double): Double =
    Math.cos(x * math.Pi / 180)

  def sin(x: Double): Double =
    Math.sin(x * math.Pi / 180)

  def oneThreadTurtle(moves: Array[(Double, Double)]): (Double, Double) = {
    var (x, y) = (0.0, 0.0)
    var directionAngle = 0.0 // east

    for ((angle, move) <- moves) {
      directionAngle = (directionAngle + angle) % 360
      x += move * cos(directionAngle)
      y += move * sin(directionAngle)
    }

    (x, y)
  }

  def parTurtle(moves: Array[(Double, Double)], threads: Int): (Double, Double) = {

    def operator(p: (Double, Double, Double), q: (Double, Double, Double)): (Double, Double, Double) = {
      val (a, realAlpha, alpha) = p
      val (b, realBeta, beta) = q

      val M = math.sqrt(a * a + b * b + 2 * a * b * cos(realBeta + alpha - realAlpha))
      if (M == 0) {
        (M, realAlpha, (alpha + beta) % 360)
      } else {
        (M,
          (realAlpha + asin(b * sin(realBeta + alpha - realAlpha) / M)) % 360,
          (alpha + beta) % 360)
      }
    }

    def applyMove(move: (Double, Double, Double)): (Double, Double) = move match {
      case (m, a, _) => (m * cos(a), m * sin(a))
    }

    val C = moves.map({ case (x, y) => (y, x, x) })
    scanC(C, operator, 0, moves.length, threads)
    applyMove(C.last)
  }

  def TurtleTime() = {
    println("{--------------Turtle--------------}")
    val n = 1 << 18
    val r = new scala.util.Random
    val moves = new Array[(Double, Double)](n)

    for {
      i <- 0 until n
      x = r.nextDouble() % 360
      y = r.nextDouble()
    } moves(i) = (x, y)

    val seqtime = standardConfig measure {
      oneThreadTurtle(moves)
    }
    println("Sequential time: " + seqtime.value)
    for (threads <- l) {
      val partime = standardConfig measure {
        parTurtle(moves, threads)
      }

      println("Parallel time with " + threads + " threads: " + partime.value)
    }
  }


  def main(args: Array[String]): Unit = {
    BigIntTime()
    NthTermTime()
    BracersTime()
    TurtleTime()
  }
}