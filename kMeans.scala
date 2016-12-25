/**
  * Created by Asus on 25.12.2016.
  */

import scala.collection.GenSeq
import scala.util._
import math._
import scala.annotation.tailrec
import org.scalameter._

object Kmeans {

  private val standardConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 15,
    Key.exec.benchRuns -> 10
  ) withWarmer new Warmer.Default

  type Point = (Int, Int)

  def generatePoints(k: Int, num: Int): Seq[Point] = {
    // - генерирует множество точек,
    val r = new Random
    for {
      i <- 0 until num
      x = r.nextInt() % k
      y = r.nextInt() % k
    } yield (x, y)
  }

  def initializeMeans(k: Int, points: Seq[Point]): Seq[Point] = {
    // - выбирает k точек случайным образом для использования в качестве начальных центров кластеров,
    val r = new Random
    for {i <- 0 until k
         j = abs(r.nextInt() % points.length)
    } yield points.apply(j)
  }

  def getDistance(x: Point, y: Point): Double = sqrt((y._1 - x._1) ^ 2 + (y._2 - x._2) ^ 2)

  def findClosest(p: Point, means: GenSeq[Point]): Point = {
    // - для точки находит ближайший из центров кластеров,
    var min = getDistance(p, means.head)
    var t = means.head
    for (i <- means) {
      val n = getDistance(p, i)
      if (n < min) {
        min = n
        t = i
      }
    }
    t
  }

  def classify(points: GenSeq[Point], means: GenSeq[Point]): GenSeq[(Point, GenSeq[Point])] = {
    // - возвращает последовательность из элементов (центр кластера, <точки, для которых этот центр ближайший>),
    val A: Array[GenSeq[Point]] = new Array(means.length)
    for (i <- 0 until means.length)
      A(i) = GenSeq()
    for (i <- points)
      A(means.indexOf(findClosest(i, means))) :+ i
    val B: GenSeq[(Point, GenSeq[Point])] = GenSeq()
    for (i <- means)
      B :+ (i, A(means.indexOf(i)))
    B
  }

  def Sr(seq: GenSeq[Point]): Point =
    (seq.map(_._1).sum / seq.length,
      seq.map(_._2).sum / seq.length)

  def findAverage(oldMean: Point, points: GenSeq[Point]): Point =
  // - вычисляет новый центр кластера; если после classify в кластере нет ни одной, возвращается старое значение.
    if (points.isEmpty)
      oldMean
    else {
      Sr(points)
    }

  def update(classified: GenSeq[(Point, GenSeq[Point])]): GenSeq[Point] = {
    // - вычисляет новые центры кластеров,
    classified.map({ case (x, ys) => findAverage(x, ys) })
  }

  def converged(eta: Double)(oldMeans: GenSeq[Point], newMeans: GenSeq[Point]): Boolean = {
    // - определяет, сошлись ли итерации (абсолютное изменение положения для каждого меньше eta -> true),
    val G = oldMeans.zip(newMeans).map({case (x, y) => getDistance(x,y)})
    var f = true
    for (i <- G){
      if (i >= eta) f = false
    }
    f
  }

  @tailrec
  final def kMeans(points: GenSeq[Point], means: GenSeq[Point], eta: Double): GenSeq[Point] = {
    // - вычисляет центры кластеров алгоритмом k-means.
    val NewMeans = update(classify(points,means))
    if (converged(eta)(means, NewMeans))
      NewMeans
    else {
      kMeans(points, NewMeans, eta)
    }
  }

  def kMeansTime() = {
    val k = 1 << 3
    val numOfPoints = 1 << 11
    val eta = 1.0 / (1 << 10)

    println("{------------k-Means------------}")
    val points = generatePoints(k, numOfPoints)
    val startingMeans = initializeMeans(k, points)

    val l = kMeans(points, startingMeans, eta)

    val seqtime = standardConfig measure {
      kMeans(points, startingMeans, eta)
    }

    val partime = standardConfig measure {
      kMeans(points.par, startingMeans.par, eta)
    }

    println("seqential time: " + seqtime)
    println("parallel time: " + partime)
    println("Speedup: " + seqtime.value / partime.value)
  }

  def main(args: Array[String]): Unit = {
    kMeansTime()
  }
}
