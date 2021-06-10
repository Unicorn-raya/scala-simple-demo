object Add{
  val op: String = "+"
  def apply(expr1: String, expr2: String) = expr1 + op + expr2

  def unapply(str: String): Option[(String,String)] = {
    val index = str.indexOf(op)
    if (index > 0) 
      Some(str.substring(0,index),str.substring(index + 1))
    else
      None
  }
}

object Main extends App {

  def eval(str: String): Int = str match {
    case Add(expr1,expr2) => eval(expr1) + eval(expr2)
    case _ => str.toInt
  }

  println(eval("3+3+3"))
  println(Add.apply("3","2")) 
  println(Add.unapply("3+2+2"))
}