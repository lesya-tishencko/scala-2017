package ru.spbau.jvm.scala

import java.util
/**
  * Created by lesya on 10.09.2017.
  */
object Parser {
  def parse(input: String): Expression = {
    opstack.clear()
    result.clear()
    prev = ""

    var str = preprocess(input)
    var tokens = str.split(' ')
    buildPostfixExpr(tokens, tokens.length)
    while (!opstack.empty()) {
      handleToken(opstack.peek()._1, isUnary(opstack.pop()._2))
    }
    result.pop()
  }

  private var opstack = new util.Stack[(String, Int)]()
  private var result = new util.Stack[Expression]()
  private var prev = ""

  private def preprocess(input: String): String = {
    var str = input.replace("-", " - ");
    str = str.replace("+", " + ");
    str = str.replace("*", " * ");
    str = str.replace("/", " / ");
    str = str.replace("(", " ( ");
    str.replace(")", " ) ");
  }

  private def priority(literal: String): Int = {
    literal match {
      case "+" => 2
      case "*" => 3
      case "/" => 3
      case "(" => 1
      case "-" => if (prev == "" || isLiteral(prev)) 4 else 2
    }
  }

  private def buildPostfixExpr(tokens: Array[String], pos: Int): Unit = {
    pos match {
      case 0 => return ()
      case _ => {
        process(tokens(tokens.length - pos))
        buildPostfixExpr(tokens, pos - 1)
      }
    }
  }

  private def handleToken(token: String, isUnary: Boolean): Unit = {
    token match {
      case "+" => result.push(new BinOp(new Function2[Double, Double, Double] {
        override def apply(v1: Double, v2: Double) = v1 + v2
      }, result.pop(), result.pop()))
      case "*" => result.push(new BinOp(new Function2[Double, Double, Double] {
        override def apply(v1: Double, v2: Double) = v1 * v2
      }, result.pop(), result.pop()))
      case "/" => result.push(new BinOp(new Function2[Double, Double, Double] {
        override def apply(v1: Double, v2: Double) = v2 / v1
      }, result.pop(), result.pop()))
      case "-" => if (isUnary) {
        result.push(new UnOp(new Function1[Double, Double] {
          override def apply(v1: Double) = -v1
        }, result.pop()))
      } else {
        result.push(new BinOp(new Function2[Double, Double, Double] {
          override def apply(v1: Double, v2: Double) = v2 - v1
        }, result.pop(), result.pop()))
      }
      case _ => result.push(new Number(token.toDouble))
    }
  }

  private def isLiteral(char: String): Boolean =
    char == ")" || char == "(" || char == "+" || char == "-" || char == "*" || char == "/"

  private def isUnary(priority: Int): Boolean = priority == 4

  private def process(symbol: String): Unit = {
    if (symbol == " " || symbol == "") return ()
    if (symbol == "(")
      opstack.push((symbol, priority(symbol)))
    else if (symbol == ")") {
      while (opstack.peek()._1 != "(")
        handleToken(opstack.peek()._1, isUnary(opstack.pop()._2))
      opstack.pop()
    } else if (isLiteral(symbol)) {
      while (!opstack.empty() && opstack.peek()._2 >= priority(symbol))
        handleToken(opstack.peek()._1, isUnary(opstack.pop()._2))
      opstack.push((symbol, priority(symbol)))
    } else
      handleToken(symbol, false)
    prev = symbol
  }
}
