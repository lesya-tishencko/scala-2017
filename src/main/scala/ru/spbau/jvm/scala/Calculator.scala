package ru.spbau.jvm.scala

/**
  * Created by lesya on 11.09.2017.
  */
object Calculator {
  def main(args: Array[String]): Unit = {
    for (line <- io.Source.stdin.getLines) {
      if (line.equals("")) System.exit(0)
      val expression = Parser.parse(line)
      println(expression.eval())
    }
  }
}
