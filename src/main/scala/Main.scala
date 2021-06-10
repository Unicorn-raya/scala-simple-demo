import scala.collection.mutable.Stack

trait BinaryOp{
  val op: String
  def apply(expr1: String, expr2: String) = expr1 + op + expr2

  def unapply(str: String): Option[(String,String)] = {
    val index = str.indexOf(op)
    if (index > 0) 
      Some(str.substring(0,index),str.substring(index + 1))
    else
      None
  }
}

object Add extends {val op = "+"} with BinaryOp
object Substract extends {val op = "-"} with BinaryOp
object Mulitiply extends {val op = "*"} with BinaryOp
object Divide extends {val op = "/"} with BinaryOp

object Main extends App {

  def eval(str: String): Int = str match {
    case Add(expr1,expr2) => eval(expr1) + eval(expr2)
    case Substract(expr1,expr2) => eval(expr1) + eval(expr2)
    case Mulitiply(expr1,expr2) => eval(expr1) + eval(expr2)
    case Divide(expr1,expr2) => eval(expr1) + eval(expr2)
    case _ => str.toInt
  }
   
  def matchBracket(str: String): Option[(Int,Int)] = {
    val left = str.indexOf("(")
    if (left > 0) {
      val stack = Stack[Char]()
      val remaining = str.substring(left + 1)
      var index = 0
      var right = 0
      for (c <- remaining if right == 0) {
        index += 1
        c match {
          case '(' => stack.push(c)
          case ')' => if (stack.isEmpty) right = left + index else stack.pop()
          case _ => 
        }
      }
      Some(left,right)
    }
    else
      None
  }

  println(eval("4*3-2/2"))
  println(matchBracket("1+2+(3*5)+3+3*(3+(3+5))"))
}