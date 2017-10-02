package ru.spbau.jvm.scala
import scala.math.min

class MultiSet[A](private var map: Map[A, Int] = Map.empty[A, Int]) {
  def +(elem: A): Unit = this.map = this.map + (elem -> (this.map.getOrElse(elem, 0) + 1))

  def -(elem: A): Unit = this.map.get(elem) match {
    case None => ()
    case Some(1) => this.map = this.map - elem
    case Some(n) => this.map = this.map + (elem -> (n - 1))
  }

  def contains(elem: A): Boolean = this.map.contains(elem)

  def find(elem: A): Option[A] = this.map.get(elem) match {
    case None => None
    case _ => Some(elem)
  }

  def filter(predicate: A => Boolean): MultiSet[A] = new MultiSet(this.map.filterKeys(predicate))

  def map[B](fun: A => B): MultiSet[B] = new MultiSet(this.map.map(x => (fun(x._1) -> x._2)))

  def flatMap[B](fun: A => MultiSet[B]): MultiSet[B] = {
    val multisets = this.map.map(x => fun(x._1))
    multisets.foldLeft(new MultiSet[B]())((agg, next) => { agg.|(next); agg})
  }

  def |(other: MultiSet[A]): Unit = {
    var sumMap = other.map.map(x => (x._1, x._2 + this.map.getOrElse(x._1, 0)))
    this.map.filter(x => !sumMap.contains(x._1)).foreach(x => sumMap = sumMap + x)
    this.map = sumMap
  }

  def &(other: MultiSet[A]): Unit = {
    val diffMap = other.map.map(x => this.map.get(x._1) match {
      case None => (x._1, -1)
      case Some(n) => (x._1, min(x._2, n))
    })
    this.map = diffMap.filter(x => x._2 > 0)
  }

  def apply(elem: A): Int = this.map.getOrElse(elem, 0)
}

object MultiSet {
  def apply[A](map: Map[A, Int] = Map.empty): MultiSet[A] = new MultiSet(map)

  def unapplySeq[A](arg: MultiSet[A]): Option[Seq[A]] = {
    if (arg.map.isEmpty) None else
      Some(arg.map.foldLeft(Seq.empty[A])((arg, next) => arg ++ Seq.fill(next._2)(next._1)))
  }
}