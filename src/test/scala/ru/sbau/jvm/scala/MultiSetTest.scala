package ru.sbau.jvm.scala

import org.scalatest.FunSuite
import ru.spbau.jvm.scala.MultiSet

/**
  * Created by lesya on 02.10.2017.
  */
class MultiSetTest extends FunSuite {

  test("testUnapply") {
    val mySet = new MultiSet[String]
    mySet + "one"
    mySet + "two"
    mySet + "three"

    mySet match {
      case MultiSet("one", "two", "three") => assert(true)
      case _ => assert(false)
    }
  }

  test("testIntersect") {
    val mySet = new MultiSet[Int]
    val mySet2 = new MultiSet[Int]

    mySet + 1
    mySet2 + 1
    mySet + 2
    mySet2 + 3

    mySet & mySet2
    assert(mySet.apply(1) == 1)
    assert(!mySet.contains(2))
    assert(!mySet.contains(3))
  }

  test("testUnion") {
    val mySet = new MultiSet[Int]
    val mySet2 = new MultiSet[Int]

    mySet + 1
    mySet2 + 1
    mySet + 2
    mySet2 + 3

    mySet | mySet2
    assert(mySet.apply(1) == 2)
    assert(mySet.contains(2))
    assert(mySet.contains(3))
  }

  test("testMap") {
    val mySet = new MultiSet[Int]
    mySet + 2
    mySet + 4
    mySet + 6

    def fun(a: Int)(b: Int): Int = a * b

    val mapped = mySet.map(x => fun(x)(_))
    val mapped2 = mapped.map(x => x(10))
    for (res <- mapped2.find(20)) {
      if (res == Some(20))
        assert(true)
      if (res == None)
        assert(false)
    }

    mapped2.find(40) match {
      case Some(40) => assert(true)
      case None => assert(false)
    }

    assert(mapped2.contains(60))
  }

  test("testFilter") {
    val mySet = new MultiSet[Int]
    mySet + 1
    mySet + 1
    mySet + 2
    mySet + 2
    mySet + 3
    mySet + 4
    val filtred = mySet.filter(x => x < 3)
    filtred.apply(2) match {
      case 2 => assert(true)
      case _ => assert(false)
    }
    filtred.apply(3) match {
      case 0 => assert(true)
      case _ => assert(false)
    }
  }

  test("testApply") {
    val mySet = new MultiSet[Int];
    mySet + 1;
    mySet + 1;
    mySet + 2;
    assert(mySet.apply(1) == 2)
    assert(mySet.apply(2) == 1)
    assert(mySet.apply(0) == 0)
  }

  test("testFind") {
    val mySet = new MultiSet[String];
    mySet + "1_one";
    mySet + "2_two";
    mySet + "3_three";
    assert(mySet.find("1_one") == Some("1_one"))
    assert(mySet.find("2_two") == Some("2_two"))
    assert(mySet.find("") == None)
  }

  test("testMinus") {
    val mySet = new MultiSet[Int];
    mySet + 1;
    mySet + 1;
    mySet + 2;
    assert(mySet.contains(1));
    assert(mySet.contains(2));
    mySet - 1;
    assert(mySet.contains(1));
    mySet - 2;
    assert(!mySet.contains(2));
    mySet - 1;
    assert(!mySet.contains(1));
  }

  test("testAdd") {
    val mySet = new MultiSet[Int];
    mySet + 1;
    mySet + 1;
    mySet + 2;
    mySet + 3;
    assert(mySet.contains(1));
    assert(mySet.contains(2));
    assert(mySet.contains(3))
    assert(!mySet.contains(4));
  }

}
