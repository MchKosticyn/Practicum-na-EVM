/**
  * Created by Asus on 01.12.2016.
  */
object Set {
  type Set = Int => Boolean

  def contains(s: Set, elem: Int): Boolean = s(elem)

  def singletonSet(elem: Int): Set =
    st => st==elem

  def emptyBuilder:Set =
    st => false

  def union(s: Set, t: Set): Set =
    st => contains(s,st)||t(st)

  def intersect(s: Set, t: Set): Set =
    st => contains(s,st)&&t(st)

  def diff(s: Set, t: Set): Set =
    st => contains(s,st)&&(!t(st))

  def filter(s: Set, p: Int => Boolean): Set =
    st => contains(s,st)&&p(st)

  val bound = 1000
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }
    //Проверка, что все элементы множества s удовлетворяют условию p
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (s(a)&&(!p(a))) false
      else if (a== -1000) true
      else iter(a-1)
    }
    iter(bound)
  }

  def exists(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a== -1001) false
      else if (s(a)&&p(a)) true
      else iter(a-1)
    }
    iter(bound)
  }

  def map(s: Set, f: Int => Int): Set = {
    def iter(a: Int): Set = {
      if (a< -bound) emptyBuilder
      else if (contains(s,a))
        union(singletonSet(f(a)),iter(a-1))
           else iter(a-1)
    }
    iter(bound)
  }

  def main(args: Array[String]): Unit = {
    val Stp = union(singletonSet(1),union(singletonSet(2),union(singletonSet(3),singletonSet(4))))
    println(contains(Stp, 1))
    println(toString(Stp))
    println(toString(filter(Stp,x=>x%2==0)))
    println(forall(Stp,x=>x>=3))
    println(exists(Stp,x=>x>3))
    println(toString(map(Stp,x=>x*x)))
  }
}