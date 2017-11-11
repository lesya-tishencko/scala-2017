package ru.spbau.jvm.scala

import org.scalatest.FunSuite

/**
  * Created by lesya on 02.10.2017.
  */
class MultiSetTest extends FunSuite {

  test("testUnapply") {
    val mySet = MultiSet("one", "three", "five")
    mySet match {
      case MultiSet("one", "three", "five") => assert(true)
      case _ => fail()
    }
  }

  test("testIntersect") {
    val mySet = MultiSet(1, 2)
    val mySet2 = MultiSet(1, 3)

    mySet & mySet2
    assert(1 == mySet(1))
    assert(!mySet.contains(2))
    assert(!mySet.contains(3))
  }

  test("testUnion") {
    val mySet = MultiSet(1, 2)
    val mySet2 = MultiSet(1, 3)

    mySet | mySet2
    assert(2 == mySet(1))
    assert(mySet.contains(2))
    assert(mySet.contains(3))
  }

  test("testMap") {
    val mySet = MultiSet(2, 4, 6)

    def fun(a: Int)(b: Int): Int = a * b

    val mapped = mySet.map(x => fun(x)(_))
    val mapped2 = mapped.map(x => x(10))
    assert(mapped2.contains(20))
    assert(mapped2.contains(40))
    assert(mapped2.contains(60))

    val mapped3 = mySet.map(_ => 0)
    assert(3 == mapped3(0))
  }

  test("testFilter") {
    val mySet = MultiSet(1, 1, 2, 2, 3, 4)
    mySet + 4
    val filtered = mySet.filter(x => x < 3)
    assert(2 == filtered(2))
    assert(0 == filtered(3))
  }

  test("testApply") {
    val mySet = MultiSet(1, 1, 3)
    assert(2 == mySet(1))
    assert(1 == mySet(3))
    assert(0 == mySet(0))
  }

  test("testContains") {
    val mySet = MultiSet[String]("1_one", "2_two", "3_three")
    assert(mySet.contains("1_one"))
    assert(mySet.contains("2_two"))
    assert(!mySet.contains(""))
  }

  test("testMinus") {
    val mySet = MultiSet(1, 1, 2);
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
    val mySet = MultiSet(1, 1, 2, 3);
    assert(mySet.contains(1));
    assert(mySet.contains(2));
    assert(mySet.contains(3))
    assert(!mySet.contains(4));
  }

}
