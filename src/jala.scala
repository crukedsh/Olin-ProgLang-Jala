/************************************************************

FINAL PROJECT

Team members: Zhengyang Feng, Hong Giap Tee

Emails: Zhengyang.Feng@students.olin.edu
        HongGiap.Tee@students.olin.edu

Remarks, if any:

  ************************************************************/


/*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions and methods below are stubs that you should
 * replace with your own implementation.
 *
 * PLEASE DO NOT CHANGE THE TYPES
 * Doing so risks making it impossible for me to test your code.
 *
 * Always make sure you can run this file as a whole with scala
 * by running
 *    scala <file>
 * before submitting it. It has to load without any errors.
 *
 */


//
//  Values
//


abstract class Value {

    // default behaviors for values

    def isInteger () : Boolean = false
    def isBoolean () : Boolean = false
    def isString () : Boolean = false
    def isVector () : Boolean = false
    def isFunction () : Boolean = false
    //    def isDict () : Boolean = false
    def isNone () : Boolean = false

    def error (msg : String) : Nothing = {
        throw new Exception("Value error: "+ msg + "\n   in value " + this)
    }

    def getInt () : Int = {
        throw new Exception("Value not of type INTEGER")
    }

    def getString () : String = {
        throw new Exception("Value not of type STRING")
    }

    def getBool () : Boolean = {
        throw new Exception("Value not of type INTEGER; cannot convert to BOOLEAN")
    }

    def getList () : List[Value] = {
        throw new Exception("Value not of type VECTOR")
    }

    def apply (args: List[Value]) : Value =  {
        throw new Exception("Value not of type FUNCTION")
    }

    def checkInteger () : Unit = {
        if (!isInteger()) {
            error("Value not of type INTEGER")
        }
    }

    def checkString () : Unit = {
        if (!isString()) {
            error("Value not of type STRING")
        }
    }

    def checkBoolean () : Unit = {
        if (!isBoolean()) {
            error("Value not of type BOOLEAN")
        }
    }

    def checkVector () : Unit = {
        if (!isVector()) {
            error("Value not of type VECTOR")
        }
    }

    def checkFunction () : Unit = {
        if (!isFunction()) {
            error("Value not of type FUNCTION")
        }
    }

}



class VNone () extends Value {
    override def toString () : String = "None"
    override def isNone () : Boolean = true

}

class VInteger (val i:Int) extends Value {

    override def toString () : String = {
        if (i<0) '_' + (-i).toString else i.toString()
    }
    override def isBoolean(): Boolean = true
    override def isInteger () : Boolean = true
    override def getInt () : Int = i
    override def getBool () : Boolean = i != 0

}

class VString (val s:String) extends Value {

    override def toString () : String = "\"" + s + "\""
    override def isString () : Boolean = true
    override def getString () : String = s

}


class VVector (val l:List[Value]) extends Value {

    override def toString () : String =
        return l.addString(new StringBuilder(), "[ ", " ", " ]").toString()

    override def isVector () : Boolean = true
    override def getList () : List[Value] = l
}



class VPrimOp (val oper : (List[Value]) => Value) extends Value {

    override def toString () : String = "primop(" + oper + ")"
    override def isFunction () : Boolean = true

    override def apply (args: List[Value]) : Value =
        oper(args)
}


class VRecClosure (val self: String, val params: List[String], val body:Exp, val env:Env) extends Value {

    override def toString () : String = params + " | " + self + " => " + body
    override def isFunction () : Boolean = true

    override def apply (args: List[Value]) : Value = {
        if (params.length != args.length) {
            throw new Exception("Runtime error : wrong number of arguments\n  Function "+this.toString())
        }
        var new_env = env
        for ((p,v) <- params.zip(args)) {
            new_env = new_env.push(p,v)
        }

        // push the current closure as the value bound to identifier self
        new_env = new_env.push(self,this)
        return body.eval(new_env)
    }
}

//
//  Primitive operations
//

object Ops {

    def runtimeError(msg: String): Nothing = {

        throw new Exception("Runtime error: " + msg)
    }


    def checkArgsLength(vs: List[Value], min: Int, max: Int): Unit = {

        //
        // check whether an argument list has size between min and max
        //

        if (vs.length < min) {
            runtimeError("Number of args < " + min)
        }
        if (vs.length > max) {
            runtimeError("Number of args > " + max)
        }
    }
}

object InternalOps {
    import Ops._
    def operVector (vs: List[Value]) : Value = {

        return new VVector(vs)
    }

}

object MonadicOps {

    import Ops._

    def operRavel(vs: List[Value]): Value = {
        checkArgsLength(vs, 1, 1)
        val v = vs.head
        if (v.isInteger()) return v
        var ret = List[Value]()
        def yieldItem(v : Value): Unit = {
            if (v.isInteger()) ret = ret :+ new VInteger(v.getInt())
            else if (v.isVector()) for (u <- v.getList()) yieldItem(u)
            else runtimeError("Wrong ravel")
        }
        yieldItem(v)
        new VVector(ret)
    }

    def operMinus(vs: List[Value]): Value = {
        checkArgsLength(vs, 1, 1)

        val v = vs.head
        if (v.isInteger()) return new VInteger(-v.getInt())
        else if (v.isVector()) return new VVector(v.getList().map(x => operMinus(List(x))))else {
            runtimeError("cannot do monadic - \n  " + v)
        }
    }

    def operTally(vs: List[Value]): Value = {
        checkArgsLength(vs, 1, 1)
        new VInteger(
            if (vs.head.isVector())
                vs.head.getList().length
            else 1
        )
    }

    def operShapeOf(vs: List[Value]): Value = {
        checkArgsLength(vs, 1, 1)
        def shapeOf(vs: List[Value]): Value = {
            new VVector(
                if (vs.head.isVector())
                    new VInteger(vs.head.getList().length) :: shapeOf(vs.head.getList()).getList()
                else
                    List()
            )
        }
        shapeOf(vs)
    }

    def operTranspose(vs: List[Value]) : Value = {
        checkArgsLength(vs, 1, 1)
        def rank(v: Value): Int = {
            MonadicOps.operTally(List(MonadicOps.operShapeOf(List(v)))).getInt()
        }
        val v = vs.head
        if (v.isInteger())
            return v
        else if (rank(v) == 1) {
            var res =  List[Value]()
            for (u <- v.getList())
                res = res :+ new VVector(List(new VInteger(u.getInt())))
            new VVector(res)
        } else if (rank(v) == 2) {
            val x = MonadicOps.operShapeOf(List(v)).getList()(0).getInt()
            val y = MonadicOps.operShapeOf(List(v)).getList()(1).getInt()

            var res = List[Value]()
            for (j <- 0 until y) {
                var cur = List[Value]()
                for (i <- 0 until x) {
                    cur = cur :+ new VInteger(v.getList()(i).getList()(j).getInt())
                }
                res = res :+ new VVector(cur)
            }
            new VVector(res)
        } else runtimeError("Bad transpose for  " + v)

    }

    def operSum(vs: List[Value]) : Value = {
        checkArgsLength(vs, 1, 1)
        def sumOf(vs: List[Value]): Value = {
            if (vs.isEmpty)
                new VInteger(0)
            else
                DyadicOps.operPlus(List(vs.head,sumOf(vs.tail)))
        }
        if (vs.head.isVector())
            sumOf(vs.head.getList())
        else
            vs.head
    }

    def operNot(vs: List[Value]) : Value = {
        checkArgsLength(vs, 1, 1)
        def recNot(vs: Value) : Value = {
            if (vs.isVector()) {
                var ret = List[Value]()
                for (v <- vs.getList()) ret = ret :+ recNot(v)
                new VVector(ret)
            }
            else if (vs.isBoolean()) if (vs.getBool()) new VInteger(0) else new VInteger(1)
            else runtimeError("Not operator invalid for " + vs)
        }
        recNot(vs.head)
    }

}

object DyadicOps {


    import Ops._

    def operAppend(vs: List[Value]): Value = {
        def compareList(v1: List[Value], v2:List[Value]): Boolean = {
            if (v1.isEmpty && v2.isEmpty) true else
                v1.nonEmpty && v2.nonEmpty && v1.head.getInt() == v2.head.getInt() && compareList(v1.tail, v2.tail)

        }
        checkArgsLength(vs, 2, 2)
        val v1 = vs(0)
        val v2 = vs(1)
        if (v1.isInteger() && v2.isInteger()) {
            return new VVector(List(v1, v2))
        }
        else if (v1.isInteger() && v2.isVector() && v2.getList().head.isInteger()) {
            return new VVector(v1 :: v2.getList())
        }
        else if (v2.isInteger() && v1.isVector() && v1.getList().head.isInteger()) {
            return new VVector(v1.getList() :+ v2)
        }
        else if (v1.isVector() &&  v2.isVector()) {
            val s1 = MonadicOps.operShapeOf(List(v1)).getList()
            val s2 = MonadicOps.operShapeOf(List(v2)).getList()
            if (compareList(s1.tail, s2.tail)) {
                return new VVector(v1.getList() ::: v2.getList())
            } else if (compareList(s1.tail, s2)) {
                return new VVector(v1.getList() :+ v2)
            } else if (compareList(s2.tail, s1)) {
                return new VVector(v1 :: v2.getList())
            } else {
                runtimeError("Bad append for " + v1 + " and " + v2)
            }
        } else {
            runtimeError("Bad append for " + v1 + " and " + v2)
        }
    }

    def recCompatibleOper(v1: Value, v2: Value, oper: (Value, Value) => Value): Value = {
        if (v1.isInteger() && v2.isInteger()) {
            return oper(v1, v2)
        } else if (v1.isVector() && v2.isVector()) {
            if (v1.getList().length == v2.getList().length) {
                var result: List[Value] = List()
                for ((entry1, entry2) <- v1.getList().zip(v2.getList())) {
                    result = result :+ recCompatibleOper(entry1, entry2, oper)
                }
                return new VVector(result)
            } else {
                runtimeError("vectors of different length")
            }
        } else if (v1.isVector() && !(v2.isVector())) {
            return new VVector(v1.getList().map((v: Value) => recCompatibleOper(v, v2, oper)))
        } else if (v2.isVector() && !(v1.isVector())) {
            return new VVector(v2.getList().map((v: Value) => recCompatibleOper(v1, v, oper)))
        } else {
            runtimeError("cannot operate values of different types\n  " + v1 + "\n  " + v2)
        }
    }


    def operPlus(vs: List[Value]): Value = {
        checkArgsLength(vs, 2, 2)
        val v1 = vs(0)
        val v2 = vs(1)
        if (v1.isString() && v2.isString()) {
             new VString(v1.getString().concat(v2.getString()))
        } else{
            recCompatibleOper(v1, v2 , (x, y) => new VInteger(x.getInt() + y.getInt()))
        }
    }

    def operDivide(vs: List[Value]): Value = {
        checkArgsLength(vs, 2, 2)
        val v1 = vs(0)
        val v2 = vs(1)
        v2.checkInteger()
        if (v1.isInteger()) {
            new VInteger(v1.getInt() / v2.getInt())
        } else if (v1.isVector()){
            new VVector(v1.getList().map(x => new VInteger(x.getInt() / v2.getInt())))
        } else {
            runtimeError("cannot divide " + v1 + " with " + v2)
        }
    }

    def operMinus(vs: List[Value]): Value = {
        checkArgsLength(vs, 2, 2)
        val v1 = vs(0)
        val v2 = vs(1)
        recCompatibleOper(v1, v2, (x, y) => new VInteger(x.getInt() - y.getInt()))
    }

    def operAnd(vs: List[Value]): Value = {
        checkArgsLength(vs, 2, 2)
        val v1 = vs(0)
        val v2 = vs(1)
        recCompatibleOper(v1, v2, (x, y) => new VInteger( if (x.getBool() & y.getBool()) 1 else 0))
    }

    def operOr(vs: List[Value]): Value = {
        checkArgsLength(vs, 2, 2)
        val v1 = vs(0)
        val v2 = vs(1)
        recCompatibleOper(v1, v2, (x, y) => new VInteger( if (x.getBool() | y.getBool()) 1 else 0))
    }

    def operTimes(vs: List[Value]): Value = {

        checkArgsLength(vs, 2, 2)

        val v1 = vs(0)
        val v2 = vs(1)

        if (v1.isInteger() && v2.isInteger()) {
            return new VInteger(v1.getInt() * v2.getInt())
        } else if (v1.isVector() && v2.isVector()) {
            if (v1.getList().length == v2.getList().length) {
                var result: Value = new VInteger(0)
                for ((entry1, entry2) <- v1.getList().zip(v2.getList())) {
                    result = operPlus(List(result, operTimes(List(entry1, entry2))))
                }
                return result
            } else {
                runtimeError("vectors of different length")
            }
        } else if (v1.isVector() && !(v2.isVector())) {
            return new VVector(v1.getList().map((v: Value) => operTimes(List(v, v2))))
        } else if (v2.isVector() && !(v1.isVector())) {
            return new VVector(v2.getList().map((v: Value) => operTimes(List(v1, v))))
        } else {
            runtimeError("cannot multiply values of different types")
        }
    }


    def operShape(vs: List[Value]) : Value = {
        checkArgsLength(vs, 2, 2)
        var run = 0
        def bind(e : Value) : List[Value] = {
            if (e.isVector()) e.getList() else List(e)
        }

        val v2 = bind(vs(1))
        var v1 = List[Int]()
        for (x <- bind(vs(0))) {
            if (x.getInt() < 1)
                runtimeError("domain error. one dimension as " + x)
            if (x.getInt() > 1)
                v1 = v1 :+ x.getInt()
        }
        if (v1.length == 0) return new VInteger(v2.head.getInt())
        def createShape(v1: List[Int]) : Value = {
            if (v1.length == 0) {
                run += 1
                return new VInteger(v2((run-1) % v2.length).getInt())
            }
            var ret = List[Value]()
            for (i <- 0 until v1.head) {
                val child = createShape(v1.tail)
                ret = ret :+ child
            }
            return new VVector(ret)
        }
        createShape(v1)
    }

}


//
//  Expressions
//


class Env (val content: List[(String, Value)]) {

    override def toString () : String = {
        var result = ""
        for (entry <- content) {
            result = result + "(" + entry._1 + " <- " + entry._2 + ") "
        }
        return result
    }


    // push a single binding (id,v) on top of the environment

    def push (id : String, v : Value) : Env =
        new Env((id,v)::content)


    // lookup value for an identifier in the environment

    def lookup (id : String) : Value = {
        for (entry <- content) {
            if (entry._1 == id) {
                return entry._2
            }
        }
        throw new Exception("Environment error: unbound identifier "+id)
    }
}



abstract class Exp {

    def eval (env : Env) : Value

    def error (msg : String) : Nothing = {
        throw new Exception("Eval error: "+ msg + "\n   in expression " + this)
    }

}


class ELiteral (val v:Value) extends Exp {
    // value literal

    override def toString () : String =
        "ELiteral(" + v + ")"

    def eval (env:Env) : Value =
        v
}



class EIf (val ec : Exp, val et : Exp, val ee : Exp) extends Exp {
    // Conditional expression

    override def toString () : String =
        "EIf(" + ec + "," + et + "," + ee +")"

    def eval (env:Env) : Value = {
        val ev = ec.eval(env)
        if (ev.isBoolean()) {
            if (!ev.getBool()) {
                return ee.eval(env)
            } else {
                return et.eval(env)
            }
        } else {
            error("condition not a Boolean")
        }
    }
}


class EId (val id : String) extends Exp {

    override def toString () : String =
        "EId(" + id + ")"

    def eval (env : Env) : Value = env.lookup(id)

}


class EApply (val f: Exp, val args: List[Exp]) extends Exp {
    override def toString () : String =
        "EApply(" + f + "," + args + ")"

    def eval (env : Env) : Value = {
        val vf = f.eval(env)
        val vargs = args.map((e:Exp) => e.eval(env))
        return vf.apply(vargs)
    }
}


class EFunction (val params : List[String], val body : Exp) extends Exp {

    override def toString () : String =
        "EFunction(" + params + "," + body + ")"

    def eval (env : Env) : Value =
        new VRecClosure("",params,body,env)

}

class ERecFunction (val self: String, val params: List[String], val body : Exp) extends Exp {

    override def toString () : String =
        "ERecFunction(" + self + "," + params + "," + body + ")"

    def eval (env : Env) : Value =
        new VRecClosure(self,params,body,env)
}



//
// SURFACE SYNTAX (S-expressions)
//


import scala.util.parsing.combinator._


class SExpParser extends RegexParsers {

    // tokens

    def LP : Parser[Unit] = "(" ^^ { _ => () }
    def RP : Parser[Unit] = ")" ^^ { _ => () }
    def LB : Parser[Unit] = "[" ^^ { _ => () }
    def RB : Parser[Unit] = "]" ^^ { _ => () }
    def INT : Parser[Int] = """_?[0-9]+""".r ^^ { s => if (s.charAt(0)!='_') s.toInt else -s.substring(1).toInt}
    def ID : Parser[String] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ { s => s }

    def PLUS : Parser[String] = "+" ^^ { s => s }
    def AND : Parser[String] = "+." ^^ { s => s }
    def MINUS : Parser[String] = "-" ^^ { s => s }
    def NOT : Parser[String] = "-." ^^ { s => s }
    def TIMES : Parser[String] = "*" ^^ { s => s }
    def OR : Parser[String] = "*." ^^ { s => s }
    def DOLLAR : Parser[String] = "$" ^^{ s => s }
    def COMMA : Parser[String] = "," ^^ { s => s }
    def CONTROL : Parser[String] = "#" ^^ { s => s }
    def SUM : Parser[String] = "+/" ^^ { s => s }
    def PERCENT : Parser[String] = "%" ^^ { s => s }
    def AT : Parser[String] = "@" ^^ { s => s }
    def TRANSPOSE: Parser[String] = "|:" ^^ { s => s }

    def ASSIGN : Parser[Unit] = "=." ^^ { s => () }
    // grammar

    def atomic_int : Parser[Exp] = INT ^^ { i => new ELiteral(new VInteger(i)) }

    def atomic_id : Parser[Exp] =
        ID ^^ { s => new EId(s) }

    def atomic : Parser[Exp] =
        ( atomic_int | atomic_id ) ^^ { e => e }

    def vector : Parser[Exp] =
        rep1(atomic) ^^ {
            case es =>  if (es.length == 1)
                es.head
            else
                new EApply(new ELiteral(new VPrimOp(InternalOps.operVector)),es)
        }

    def DOUBLY_MONADIC : Parser[String]  = ( NOT | SUM | TRANSPOSE ) ^^ { s => s }

    def SINGLY_MONADIC : Parser[String]  = ( MINUS | DOLLAR | CONTROL | COMMA) ^^ { s => s }

    def MONADIC : Parser[String]  = ( DOUBLY_MONADIC | SINGLY_MONADIC ) ^^ { s => s }

    def DOUBLY_DYADIC : Parser[String]  = ( AND | OR ) ^^ { s => s }

    def SINGLY_DYADIC : Parser[String]  = ( PLUS | MINUS | TIMES | DOLLAR | PERCENT | COMMA) ^^ { s => s }

    // Parse to List[String] to fit verb train
    def DYADIC : Parser[List[String]]  = ( DOUBLY_DYADIC | SINGLY_DYADIC ) ^^ { s => List(s) }

    def HOOK : Parser[List[String]] = LP ~ DYADIC ~ MONADIC ~ RP ^^ { case _ ~ e1 ~ e2 ~ _ => List(e1.head, e2)}

    def FORK : Parser[List[String]] = LP ~ MONADIC ~ DYADIC ~ MONADIC ~ RP ^^ { case _ ~ e1 ~ e2 ~ e3 ~ _ => List(e1, e2.head, e3)}

    def COMB : Parser[List[String]] = LP ~ MONADIC ~ AT ~ MONADIC ~ RP ^^ { case _ ~ e1 ~ _ ~ e2 ~ _ => List(e1, e2)}

    def pexpr : Parser[Exp] = LP ~ dexpr ~ RP ^^ { case _ ~ e ~ _ => e }

    def mexpr : Parser[Exp] = MONADIC ~ dexpr ^^ {
        case e1 ~ e2 =>  new EApply(new ELiteral(new VPrimOp(Shell.monadicOpt(e1))),List(e2))
    }

    def mhexpr : Parser[Exp] = HOOK ~ dexpr ^^ {
        case h ~ e => new EApply(new ELiteral(new VPrimOp(Shell.dyadicOpt(h(0)))),
            List(e, new EApply(new ELiteral(new VPrimOp(Shell.monadicOpt(h(1)))),List(e))))
    }

    def mfexpr : Parser[Exp] = FORK ~ dexpr ^^ {
        case f ~ e => new EApply(new ELiteral(new VPrimOp(Shell.dyadicOpt(f(1)))),
            List(new EApply(new ELiteral(new VPrimOp(Shell.monadicOpt(f(0)))),List(e)), new EApply(new ELiteral(new VPrimOp(Shell.monadicOpt(f(2)))),List(e))))
    }

    def mcexpr : Parser[Exp] = COMB ~ dexpr ^^ {
        case f ~ e => new EApply(new ELiteral(new VPrimOp(Shell.monadicOpt(f(0)))),
            List(new EApply(new ELiteral(new VPrimOp(Shell.monadicOpt(f(1)))),List(e))))
    }

    def expandDexpr(e1: Exp, e2 :List[(List[String], Exp)]) : Exp = {
        e2.last._1.length match {
            case 1 =>
                if (e2.length == 1)
                    new EApply(new ELiteral(new VPrimOp(Shell.dyadicOpt(e2.head._1.head))), List(e1, e2.head._2))
                else {
                    val er = e2.dropRight(1)
                    expandDexpr(e1, e2.dropRight(2) :+ (er.last._1, new EApply(new ELiteral(new VPrimOp(Shell.dyadicOpt(e2.last._1.head))), List(er.last._2, e2.last._2))))
                }
            case 2 =>
                if (e2.length == 1)
                    new EApply(new ELiteral(new VPrimOp(Shell.dyadicOpt(e2.head._1.head))), List(e1,
                        new EApply(new ELiteral(new VPrimOp(Shell.monadicOpt(e2.head._1.last))), List(e2.head._2))
                    ))
                else {
                    val er = e2.dropRight(1)
                    expandDexpr(e1, e2.dropRight(2) :+ (er.last._1, new EApply(new ELiteral(new VPrimOp(Shell.dyadicOpt(e2.last._1.head))), List(er.last._2,
                        new EApply(new ELiteral(new VPrimOp(Shell.monadicOpt(e2.head._1.last))), List(e2.last._2))
                    ))))
                }
        }

    }

    def dexpr : Parser[Exp] = fexpr ~ rep((DYADIC | FORK | HOOK ) ~ fexpr) ^^ {
        case e ~ Nil => e
        case e1 ~ e2 => expandDexpr(e1, e2.map(x => (x._1, x._2)))
    }


    def fexpr : Parser[Exp] = ( vector | mcexpr | mfexpr | mhexpr | mexpr | pexpr ) ^^ { e => e }

    def expr : Parser[Exp] =
        ( dexpr ) ^^ { e => e }

    def assign_entry : Parser[ShellEntry] =
        ID ~ ASSIGN ~ expr ^^ {
            case id ~ _ ~ e => new SEdefine(id, e)
        }

    def expr_entry : Parser[ShellEntry] =
        expr ^^ { e => new SEexpr(e) }



    def quit_entry : Parser[ShellEntry] =
        "quit" ^^ { case _ => new SEquit() }

    def env_entry : Parser[ShellEntry] =
        "env" ^^ { case _ => new SEenv() }

    def parse_entry : Parser[ShellEntry] =
        "parse" ~ expr ^^ { case _ ~ e => new SEparse(e) }

    def control_entry : Parser[ShellEntry] =
        CONTROL ~ ( quit_entry | env_entry | parse_entry ) ^^ { case _ ~ se => se }


    def shell_entry : Parser[ShellEntry] =
        ( control_entry | assign_entry | expr_entry ) ^^ { se => se }


}



//
//  Shell
//

class QuitException extends Exception {}

abstract class ShellEntry {

    // abstract class for shell entries
    // (representing the various entries you
    //  can type at the shell)

    def processEntry (env:Env) : Env
}

class SEexpr (e:Exp) extends ShellEntry {

    override def processEntry (env:Env) : Env = {
        val v = e.eval(env)
        println(v)
        return env
    }
}


class SEdefine(id : String, e : Exp) extends ShellEntry {
    override def processEntry(env: Env): Env = {
        val v = e.eval(env)
        return env.push(id, v)
    }
}

class SEquit() extends ShellEntry {
    override def processEntry(env: Env): Env = {
        throw new QuitException
    }
}

class SEenv() extends ShellEntry {
    override def processEntry(env: Env): Env = {
        for (entry <- env.content) {
            println(entry._1 + " = " + entry._2)
        }
        env
    }
}

class SEparse(e:Exp) extends ShellEntry {
    override def processEntry(env: Env): Env = {
        println(e)
        env
    }
}

object Shell {

    val parser = new SExpParser

    def parse (input:String) : ShellEntry = {
        val code = input.replaceFirst("NB[.].*", "")
        parser.parseAll(parser.shell_entry, code) match {
            case parser.Success(result,_) => result
            case failure : parser.NoSuccess => throw new Exception("Cannot parse "+input+": "+failure.msg)
        }
    }


    val nullEnv = new Env(List())

    //
    // Standard environment
    //

    val stdEnv = new Env(List(
        ("true",new VInteger(1)),
        ("false",new VInteger(0)),
        ("not", new VRecClosure("",List("a"), new EIf(new EId("a"), new ELiteral(new VInteger(0)), new ELiteral(new VInteger(1))),nullEnv)),

    ))

    val dyadicOpt = Map(
        "+" -> DyadicOps.operPlus _,
        "-" -> DyadicOps.operMinus _,
        "*" -> DyadicOps.operTimes _,
        "$" -> DyadicOps.operShape _,
        "%" -> DyadicOps.operDivide _,
        "+." -> DyadicOps.operOr _,
        "*." -> DyadicOps.operAnd _,
        "," -> DyadicOps.operAppend _,
    )

    val monadicOpt = Map(
        "-" -> MonadicOps.operMinus _,
        "#" -> MonadicOps.operTally _,
        "$" -> MonadicOps.operShapeOf _,
        "+/" -> MonadicOps.operSum _,
        "-." -> MonadicOps.operNot _,
        "," -> MonadicOps.operRavel _,
        "|:" -> MonadicOps.operTranspose _,
    )


    //    val dyadicEnv = new Env(dyadicOpt.mapValues(x => new VPrimOp(x)).toList)


    def shell () : Unit = {

        var env = stdEnv

        while (true) {
            print("JALA> ")
            try {
                val input = scala.io.StdIn.readLine()
                val se = if (input == "#debug") new SEexpr(
                    new ELiteral(new VInteger(1))
                )
                else parse(input)
                env = se.processEntry(env)
            } catch {
                case e : QuitException => {
                    println("Goodbye")
                    return
                }
                case e : Exception => println(e.getMessage)
            }
        }
    }

    def main (argv:Array[String]) : Unit = {
        shell()
    }

}


