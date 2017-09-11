package ru.spbau.jvm.scala

import org.scalatest._

/**
  * Created by lesya on 11.09.2017.
  */

class ParserTest extends FunSuite {
  test("Expression 5 + 3 * 10 is equal 35") {
    var expression = Parser.parse("5 + 3 * 10")
    assert(35.0 - expression.eval() <= 1e-10)
  }

  test("Expression (5 + 3) * 10 is equal 80") {
    var expression = Parser.parse("(5 + 3) * 10")
    assert(80.0 - expression.eval() <= 1e-10)
  }

  test("Expression -5 * (5 - 10) is equal 25") {
    var expression = Parser.parse("-5 * (5 - 10)")
    assert(25.0 - expression.eval() <= 1e-10)
  }

  test("Expression -0 * (10 / 4 + 6) + 6 * (-1 - 3) is equal -24") {
    var expression = Parser.parse("-0 * (10 / 4 + 6) + 6 * (-1 - 3)")
    assert(-24.0 - expression.eval() <= 1e-10)
  }

  test("Expression ((5 + 3) * (10 / 2)) + 1 is equal 76") {
    var expression = Parser.parse("((5 + 3) * (10 / 2)) + 1")
    var x = expression.eval()
    assert(41.0 - expression.eval() <= 1e-10)
  }
}
