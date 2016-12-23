/**
  * Created by Asus on 21.12.2016.
  */
object PScan {

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

  def oneThread(A:BigInt, B:BigInt): BigInt = {
    var carry = 0
    val Res: BigInt = new Array(A.length)
    for (i <- A.indices) {
      Res(i) = (A(i) + B(i) + carry) % 10
      carry = (A(i) + B(i) + carry) / 10
    }
    if (carry>0) {
      Res(Res.length - 1) = 1
      Res
    } else Res.init
  }

  def SumWithOneThread(A:BigInt, B:BigInt): BigInt = oneThread(A.reverse, B.reverse).reverse

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

  def PrefixScan[T](A: Array[T], Zero: T, f: (T, T) => T, threads: Int): Unit = {
    scanC(A, f, 0, A.length, threads)
    A(A.length - 1) = Zero
    scanD(A, f, 0, A.length, threads)
  }

  val FirstArray = 0 +: scala.io.StdIn.readLine().toCharArray.map(_.toInt.-(48))
  val SecondArray = 0 +: scala.io.StdIn.readLine().toCharArray.map(_.toInt.-(48))
  var ExtraArray:  Array[Char] = new Array(FirstArray.length)

  def OneThreadCompute(A: BigInt, B: BigInt, from: Int, count: Int) =
    for (i <- from until from + count)
      if (A(i) + B(i) >= 10) ExtraArray(i) = 'C'
      else if (A(i) + B(i) == 9) ExtraArray(i) = 'M'
      else ExtraArray(i) = 'N'

  def ComputeExtraArray(A: BigInt, B: BigInt, from: Int, count: Int, threads: Int): Unit = {
    if ((thread >= threads) || (count == 1)) OneThreadCompute(A, B, from, count)
    else {
      thread += 1
      parallel(ComputeExtraArray(A, B, from, count / 2, threads),
               ComputeExtraArray(A, B, from + count / 2, count - count / 2, threads))
      thread -= 1
    }
  }

  def ToArray(arg1: Char) = arg1 match {
    case 'C' => 1
    case 'N' => 0
  }

  def Shr[T](A: Array[T], z: T): Array[T] = {
    for (i <- A.length - 1 to 1 by -1) A(i) = A(i-1)
    A(0) = z
    A
  }

  def Shl[T](A: Array[T]): Array[T] = {
    for (i <- 0 until A.length - 1) A(i) = A(i+1)
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

  def BigSum(A: BigInt, B: BigInt, threads: Int): BigInt = {
    ComputeExtraArray(A, B, 0, A.length, threads)
    PrefixScan(ExtraArray, 'M', operation, 1)
    ExtraArray = Shl[Char](ExtraArray)
    ExtraArray(ExtraArray.length - 1) = 'N'
    val FinalA: BigInt = new Array(ExtraArray.length)
    Sum(A, B, Shr(ExtraArray.map(ToArray), 0), FinalA, 0, FinalA.length, threads)
    if (FinalA(FinalA.length - 1) == 0) FinalA.init
    else FinalA
  }

  def main(args: Array[String]): Unit = {
    for (i <- SumWithOneThread(FirstArray, SecondArray))
      print(i + "  ")
    //for (i <- BigSum(FirstArray.reverse, SecondArray.reverse, 8).reverse)
  }
}