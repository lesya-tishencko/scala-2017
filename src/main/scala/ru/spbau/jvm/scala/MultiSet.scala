package ru.spbau.jvm.scala

import scala.collection.mutable

class MultiSet[A](private var elements: mutable.Map[A, Int] = mutable.Map.empty[A, Int]) {
  def +(element: A): Unit = {
    elements += element -> (this(element) + 1)
  }

  def -(element: A): Unit = elements.get(element) match {
    case Some(1) => elements -= element
    case Some(n) => elements += (element -> (n - 1))
    case None =>
  }

  def contains(element: A): Boolean = elements.contains(element)

  def filter(predicate: A => Boolean): MultiSet[A] = new MultiSet(elements.filter(x => predicate(x._1)))

  def map[B](fun: A => B): MultiSet[B] = {
    var newElements = mutable.Map.empty[B, Int];
    elements.foreach {
      case (element, count) => newElements += fun(element) -> {
        newElements.getOrElse(fun(element), 0) + count
      }
    }
    new MultiSet(newElements)
  }

  def flatMap[B](fun: A => MultiSet[B]): MultiSet[B] = {
    val multisets = elements.map(x => fun(x._1))
    multisets.foldLeft(new MultiSet[B]())((agg, next) => { agg.|(next); agg})
  }

  def |(other: MultiSet[A]): Unit = {
    var sumMap = other.elements.map {
      case (element, count) => element -> (count + this(element))
    }
    elements.filter {
      case (element, count) => !sumMap.contains(element)
    }.foreach(x => sumMap += x)
    elements = sumMap
  }

  def &(other: MultiSet[A]): Unit = {
    val diffMap = other.elements.map{
      case (element, count) => element -> count.min(elements.getOrElse(element, -1))
    }
    elements = diffMap.filter(x => x._2 > 0)
  }

  def apply(element: A): Int = elements.getOrElse(element, 0)

  override def toString: String = elements.toString
  override def equals(obj: scala.Any): Boolean = elements.equals(obj)
  override def hashCode(): Int = elements.hashCode()
}

object MultiSet {
  def apply[A](seq: A*): MultiSet[A] = {
    var newElements = mutable.Map.empty[A, Int]
    seq.foreach(x => newElements += x -> (newElements.getOrElse(x, 0) + 1))
    new MultiSet(newElements)
  }

  def unapplySeq[A](set: MultiSet[A]): Option[Seq[A]] = {
    val elements = set.elements
    val r = elements.foldLeft(Seq.empty[A]) {
      case (arg, (element, count)) => arg ++ Seq.fill(count)(element)
    }
    if (elements.isEmpty) None else
      Some(elements.foldLeft(Seq.empty[A]) {
        case (arg, (element, count)) => arg ++ Seq.fill(count)(element)
      })
  }
}