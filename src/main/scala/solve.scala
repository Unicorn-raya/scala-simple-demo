// Another methods

import scala.collection.mutable.Stack

trait BinaryOp{
  val op:String
  def apply(expr1:String,expr2:String) = expr1 + op + expr2
  def unapply(str:String) :Option[(String,String)] ={
    val index=str indexOf (op)
    if(index>0)
      Some(str substring(0,index),str substring(index+1))
    else None
  }
}

class Rational (n:Int, d:Int) {
  require(d!=0)
  private val g =gcd (n.abs,d.abs)
  val numer =n/g
  val denom =d/g
  override def toString = numer + "\\" +denom
  def +(that:Rational)  =
    new Rational(
      numer * that.denom + that.numer* denom,
      denom * that.denom
    )

  def -(that:Rational)  =
    new Rational(
      numer * that.denom - that.numer* denom,
      denom * that.denom
    )

  def * (that:Rational) =
    new Rational( numer * that.numer, denom * that.denom)

  def / (that:Rational) =
    new Rational( numer * that.denom, denom * that.numer)

  def this(n:Int) = this(n,1)
  private def gcd(a:Int,b:Int):Int =
    if(b==0) a else gcd(b, a % b)
}


object Bracket{
  def matchBracket(str:String):Option[(Int,Int)] ={
    val left = str.indexOf('(')
    if(left >=0) {
      val stack = Stack[Char]()
      val remaining = str substring (left+1)
      var index=0
      var right=0
      for(c <-remaining if right==0){
        index=index + 1
        c match{
          case '(' => stack push c
          case ')'  => if (stack isEmpty)  right= left+index else stack pop
          case _ =>
        }

      }

      Some(left,right)
    }else  None
  }

  def apply(part1:String,expr:String,part2:String) =part1+ "(" + expr + ")"+ part2
  def unapply(str:String) :Option[(String,String,String)] ={
     Bracket.matchBracket(str) match{
      case Some((left:Int,right:Int)) =>{
        val part1 = if (left == 0) "" else str substring(0, left )
        val expr = str substring(left + 1, right)
        val part2 = if (right == (str length)-1) "" else str substring (right+1)
        Some(part1, expr, part2)
      }
      case _ => None
    }
  }
}

object Multiply  extends {val op="*"} with BinaryOp
object Divide  extends {val op="/"} with BinaryOp
object Add  extends {val op="+"} with BinaryOp
object Subtract  extends {val op="-"} with BinaryOp
object Rational  extends {val op="\\"} with BinaryOp


object Main extends App{
  val templates=List(
    "N*N-N+N",
    "(N-N)*N*N",
    "N*N+N*N",
    "(N+N)*N*N",
    "N*N*N*N",
    "(N+N*N)*N",
    "(N*N-N)*N",
    "N*N+N+N",
    "(N/N-N)*N",
    "(N-(N-N))*N",
    "N-(N-N-N)",
    "N+N-(N-N)",
    "N*(N/N-N)",
    "(N-N*N)*N",
    "N*(N-N)+N",
    "N+N+N/N",
    "(N-N)*(N-N)",
    "N+N*N/N",
    "N*N/(N-N)",
    "(N+N)*(N+N)",
    "(N-N)*N/N",
    "N+(N+N)/N",
    "N*N/(N+N)",
    "(N+N)*N/N",
    "(N*N+N)*N",
    "(N*N-N)/N",
    "(N/N+N)*N",
    "N*N/N/N",
    "N+N+N-N",
    "N-(N-N)+N",
    "N/(N-N/N)",
    "N+(N-N)*N",
    "(N+N+N)*N",
    "N+N*N-N",
    "N*N-N/N",
    "(N+N)*N-N",
    "(N+N)*(N-N)",
    "(N-N/N)*N",
    "N*(N+N)+N",
    "N*N+N/N",
    "N*N/N-N",
    "(N+N/N)*N",
    "N*N*N/N",
    "(N+N*N)/N",
    "N+N*N+N",
    "N-(N-N)*N",
    "(N-(N+N))*N",
    "N*N-N-N",
    "N+N/N+N",
    "(N-N)*N-N",
    "(N+N)/N+N",
    "N*N+N-N",
    "N/N+N+N",
    "N*N*N-N",
    "(N*N+N)/N",
    "N+N+N*N",
    "N*(N-N)/N",
    "N/N*N+N",
    "N+N*N*N",
    "N+N+N+N",
    "N*N/(N*N)",
    "N+(N+N)*N",
    "(N-N)*N+N",
    "(N+N+N)/N",
    "(N+N)*N+N",
    "N*N*N+N",
    "N*N-(N-N)",
    "N*N-(N+N)",
    "(N-N-N)*N",
    "N*N/N+N",
    "(N+N-N)*N",
    "N/(N/N-N)",
    "N*N-N*N"
  )

  def eval(str:String):Rational = {
    str match {
      case Bracket(part1, expr, part2) => eval(part1 + eval(expr) + part2)
      case Add(expr1, expr2) => eval(expr1) + eval(expr2)
      case Subtract(expr1, expr2) => eval(expr1) - eval(expr2)
      case Multiply(expr1, expr2) => eval(expr1) * eval(expr2)
      case Divide(expr1, expr2) =>  eval(expr1) / eval(expr2)
      case "" => new Rational(0, 1)
      case Rational(expr1, expr2) =>   new Rational(expr1.trim toInt, expr2.trim toInt)
      case _ => new Rational(str.trim toInt, 1)

    }
  }

  def calculate(template:String,numbers:List[Int])={
    val values=template.split('N')
    var expression=""
    for(i <- 0 to 3)  expression=expression+values(i) + numbers(i)
    if (values.length==5) expression=expression+values(4)
    (expression,template,eval(expression))
  }

  def cal24(input:List[Int])={
    var found = false
    for (template <- templates; list <- input.permutations ) {
      try {
        val (expression, tp, result) = calculate(template, list)
        if (result.numer == 24 && result.denom == 1) {
          println(input + ":" + tp + ":" + expression)
          found = true
        }
      } catch {
        case e:Throwable=>
      }
    }
    if (!found) {
      println(input+":"+"no result")
    }
  }

  def cal24once(input:List[Int])={
    var found = false
    for (template <- templates; list <- input.permutations if(!found)) {
      try {
        val (expression, tp, result) = calculate(template, list)
        if (result.numer == 24 && result.denom == 1) {
          println(input + ":" + tp + ":" + expression)
          found = true
        }
      } catch {
        case e:Throwable=>
      }
    }
    if (!found) {
      println(input+":"+"no result")
    }
  }
  println(cal24once(List(5,5,5,1)))
}