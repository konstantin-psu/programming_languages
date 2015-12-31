// Object-oriented expression processing in Scala  

abstract class Typ
case object NumT extends Typ
case object BoolT extends Typ

abstract class Val
case class NumV(n:Int) extends Val
case class BoolV(b:Boolean) extends Val

abstract class Exp {
  type VEnv = Map[String,Val]
  type TEnv = Map[String,Typ]
  def eval(env:VEnv) : Val
  def check(env:TEnv) : Typ
}

case class NumE(private val n:Int) extends Exp {
  def eval(env:VEnv) = NumV(n)
  def check(env:TEnv) = NumT
}

case class VarE(private val x:String) extends Exp {
  def eval(env:VEnv) = env(x)
  def check(env:TEnv) = env(x)
}

case class LetE(private val x:String,private val d:Exp,private val b:Exp) extends Exp {
  def eval(env:VEnv) = {
    val v1 = d.eval(env)
    b.eval(env + (x -> v1))
  }
  def check(env:TEnv) = {
    val t1 = d.check(env)
    b.check(env + (x -> t1))
  }
}

case class AddE(private val l:Exp,private val r:Exp) extends Exp {
  def eval(env:VEnv) = (l.eval(env),r.eval(env)) match {
    case (NumV(v1),NumV(v2)) => NumV(v1 + v2)
    case _ => throw sys.error("eval")    
  }
  def check(env:TEnv) = (l.check(env),r.check(env)) match {
    case (NumT,NumT) => NumT
    case _ => throw sys.error("check")    
  }
}

case class LtE(private val l:Exp,private val r:Exp) extends Exp {
  def eval(env:VEnv) = (l.eval(env),r.eval(env)) match {
    case (NumV(v1),NumV(v2)) => BoolV(v1 < v2)
    case _ => throw sys.error("eval")    
  }
  def check(env:TEnv) = (l.check(env),r.check(env)) match {
    case (NumT,NumT) => BoolT
    case _ => throw sys.error("check")    
  }
}

 
case class IfE(private val c:Exp,private val t:Exp,private val f:Exp) extends Exp {
  def eval(env:VEnv) = c.eval(env) match {
    case BoolV(true) => t.eval(env)
    case BoolV(false) => f.eval(env)
    case _ => throw sys.error("eval")
  }
  def check(env:TEnv) = c.check(env) match {
    case BoolT => {
      val tt = t.check(env)
      val tf = f.check(env)
      if (tt == tf) tt else throw sys.error("check")
    }
    case _ => throw sys.error("check")
 }    
  
}
