// Algebraic data types in Scala 

abstract class Typ
case object NumT extends Typ
case object BoolT extends Typ

abstract class Val
case class NumV(n:Int) extends Val
case class BoolV(b:Boolean) extends Val

abstract class Exp
case class NumE(n:Int) extends Exp
case class VarE(x:String) extends Exp
case class LetE(x:String,d:Exp,b:Exp) extends Exp
case class AddE(l:Exp,r:Exp) extends Exp
case class LtE(l:Exp,r:Exp) extends Exp
case class IfE(c:Exp,t:Exp,f:Exp) extends Exp

object Code {

  type VEnv = Map[String,Val]

  // throws an error if a type-inconsistent operation is attemped
  def eval (env:VEnv, e:Exp) : Val = e match {
    case NumE(n) => NumV(n)
    case VarE(x) => env(x)
    case LetE(x,e1,e2) => {
      val v1 = eval(env,e1)
      eval (env + (x -> v1), e2)
    }
    case AddE(e1,e2) => (eval(env,e1),eval(env,e2)) match {
      case (NumV(v1),NumV(v2)) => NumV (v1 + v2)
      case _ => throw sys.error("eval")
    }
    case LtE(e1,e2) => (eval(env,e1),eval(env,e2)) match {
      case (NumV(v1),NumV(v2)) => BoolV (v1 < v2)
      case _ => throw sys.error("eval")
    }
    case IfE(e1,e2,e3) => eval(env,e1) match {
      case BoolV(true) => eval(env,e2)
      case BoolV(false) => eval(env,e3)
      case _ => throw sys.error("eval")
    } 
  }

  type TEnv = Map[String,Typ]

  // throws an error if type-checking fails
  def check (env:TEnv, e:Exp) : Typ = e match {
    case NumE(n) => NumT
    case VarE(x) => env(x)
    case LetE(x,e1,e2) => {
      val t1 = check(env,e1)
      check (env + (x -> t1), e2)
    }
    case AddE(e1,e2) =>  (check(env,e1),check(env,e2)) match {
      case (NumT,NumT) => NumT
      case _ => throw sys.error("check")
    }
    case LtE(e1,e2) => (check(env,e1),check(env,e2)) match {
      case (NumT,NumT) => BoolT
      case _ => throw sys.error("check")
    }
    case IfE(e1,e2,e3) => check(env,e1) match {
      case BoolT => {
        val t2 = check(env,e2)
        val t3 = check(env,e3)
        if (t2 == t3) t2 else throw sys.error("check")
      }
      case _ => throw sys.error("check")
    }
  }
}
