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


object Bracket {
  def matchBracket(str: String): Option[(Int,Int)] = {
    val left = str.indexOf('(')
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

  def apply(part_1: String, expr: String, part_2:String) = part_1 + '(' + expr + ')' + part_2
  
  def unapply(str: String): Option[(String,String,String)] = {
    Bracket.matchBracket(str) match {
      case Some((left: Int, right: Int)) => {
        val part_1 = if (left == 0) "" else str.substring(0,left)
        val expr = str.substring(left + 1, right)
        val part_2 = if (right == str.length - 1) "" else str.substring(right + 1)
        
        Some(part_1, expr, part_2)

      } 
      case _ => None
    }
  }
}

class Rational (n:Int, d:Int) {
  require(d!=0)
  private val g =gcd(n.abs,d.abs)
  val numer = n/g
  val denom = d/g
  override def toString = numer + "/" +denom
  def +(that:Rational)  =
    new Rational(
      numer * that.denom + that.numer * denom,
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


object Main extends App {

  def eval(str: String): Rational = str match {
    case Bracket(part1,expr,part2) => eval(part1 + eval(expr) + part2)
    case Add(expr1,expr2) => eval(expr1) + eval(expr2)
    case Substract(expr1,expr2) => eval(expr1) - eval(expr2)
    case Mulitiply(expr1,expr2) => eval(expr1) * eval(expr2)
    case Divide(expr1,expr2) => eval(expr1) / eval(expr2)
    case _ => new Rational(str.trim().toInt,1)
  }
   
  println(eval("1+2+(3*5)+3+3*(3+(3+5))"))
  println(eval("5*(5-1/5)"))
  println(eval("4*6"))
  println(eval("4*6+3*3+5/7"))
  
  def solve(vs: List[Int], n: Int = 24){
    def isZero(d: Double) = Math.abs(d) < 0.00001

    def toStr(any: Any): String = any match {
      case (v: Double, null, null, null) => v.toInt.toString

      case(_,v1: (Double,Any,Any,Any),v2: (Double,Any,Any,Any),op) =>
        if (op == '-' && (v2._4 == '+' || v2._4 == '-'))
          "%s%c(%s)".format(toStr(v1),op,toStr(v2))
        else if (op == '/'){
          val s1 = if (v1._4 == '+' || v1._4 == '-')  
                      "(" + toStr(v1) + ")"
                    else
                      toStr(v1)
          val s2 = if (v2._4 == null) 
                      toStr(v2)
                    else
                     "(" + toStr(v2) + ")"
          s1 + op + s2
        }
        else if (op == '*'){
          val s1 = if (v1._4 == '+' || v1._4 == '-')  
                      "(" + toStr(v1) + ")"
                    else
                      toStr(v1)
          val s2 = if (v2._4 == null) 
                      "(" + toStr(v2) + ")"
                    else
                      toStr(v2)
          s1 + op + s2
        }
        else
          toStr(v1) + op + toStr(v2)
    }

    val buf = collection.mutable.ListBuffer[String]()

    def solve0(xs: List[(Double,Any,Any,Any)]): Unit =
      xs match {
        case x::Nil => if(isZero(x._1 - n) && !buf.contains(toStr(x))) {
          buf += toStr(x)
          println(buf.last)
        }

        case _ => for {x @ (v1,_,_,_) <- xs; val ys = xs.diff(List(x))
                       y @ (v2,_,_,_) <- ys; val rs = ys.diff(List(y))}
                       {
                         solve0((v1+v2,x,y,'+')::rs)
                         solve0((v1-v2,x,y,'-')::rs)
                         solve0((v1*v2,x,y,'*')::rs)
                         if(!isZero(v2))
                            solve0((v1/v2,x,y,'/')::rs)
                       }
      }
      solve0(vs.map{v => (v.toDouble,null,null,null)})
  }
  println("test solve: ")
  println(solve(List(5,5,5,1)))
  println(solve(List(3,3,8,8))) 
  
  // def permutations(l: List[Int]): List[List[Int]] = {
  //   l match {
  //     case Nil  => List(List())
  //     case (head :: tail) => 
  //       for (p0 <- permutations(tail);i<-0 to (p0.length); (xs,ys)=p0.splitAt(i)) 
  //          yield xs ::: List(head)::: ys
  //   }
  // }
  // println(permutations(List(1,2,3)).mkString("\n"))
  // println(permutations(List(1,1,3)).mkString("\n"))
  // println(permutations(List(1,2,3)).distinct.mkString("\n"))
  // println(permutations(List(1,1,3)).distinct.mkString("\n"))
  //use  List(1,2,3,4).permutation.mkString('n')


}

