package ru.spbau.jvm.scala

/**
  * Created by lesya on 10.09.2017.
  */
abstract class Expression {
  def eval(): Double;
}

class Number(value: Double = 0.0) extends Expression {
  override def eval(): Double = value;
}

class UnOp(function: Function1[Double, Double], arg: Expression) extends Expression {
  override def eval(): Double = function(arg.eval());
}

class BinOp(function: Function2[Double, Double, Double], argl: Expression, argr: Expression) extends Expression {
  override def eval(): Double = function(argl.eval(), argr.eval());
}