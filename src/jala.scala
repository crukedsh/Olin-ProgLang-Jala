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

    override def toString () : String = i.toString()
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

class VNoun (override val l: List[Value]) extends VVector(l) {

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

    def runtimeError (msg: String) : Nothing = {

        throw new Exception("Runtime error: "+msg)
    }



    def checkArgsLength (vs:List[Value], min: Int, max : Int) : Unit = {

        //
        // check whether an argument list has size between min and max
        //

        if (vs.length < min) {
            runtimeError("Number of args < "+min)
        }
        if (vs.length > max) {
            runtimeError("Number of args > "+max)
        }
    }



    def operPlus (vs:List[Value]) : Value = {

        checkArgsLength(vs,2,2)

        val v1 = vs(0)
        val v2 = vs(1)

        if (v1.isInteger() && v2.isInteger()) {
            return new VInteger(v1.getInt() + v2.getInt())
        } else if (v1.isString() && v2.isString()) {
            return new VString(v1.getString().concat(v2.getString()))
        } else if (v1.isVector() && v2.isVector()) {
            if (v1.getList().length == v2.getList().length) {
                var result : List[Value] = List()
                for ((entry1,entry2) <- v1.getList().zip(v2.getList())) {
                    result = result :+ operPlus(List(entry1,entry2))
                }
                return new VVector(result)
            } else {
                runtimeError("vectors of different length")
            }
        } else if (v1.isVector() && !(v2.isVector())) {
            return new VVector(v1.getList().map((v:Value) => operPlus(List(v,v2))))
        } else if (v2.isVector() && !(v1.isVector())) {
            return new VVector(v2.getList().map((v:Value) => operPlus(List(v1,v))))
        } else {
            runtimeError("cannot add values of different types\n  "+v1+"\n  "+v2)
        }
    }


    def operTimes (vs: List[Value]):Value = {

        checkArgsLength(vs,2,2)

        val v1 = vs(0)
        val v2 = vs(1)

        if (v1.isInteger() && v2.isInteger()) {
            return new VInteger(v1.getInt() * v2.getInt())
        } else if (v1.isVector() && v2.isVector()) {
            if (v1.getList().length == v2.getList().length) {
                var result : Value = new VInteger(0)
                for ((entry1,entry2) <- v1.getList().zip(v2.getList())) {
                    result = operPlus(List(result, operTimes(List(entry1,entry2))))
                }
                return result
            } else {
                runtimeError("vectors of different length")
            }
        } else if (v1.isVector() && !(v2.isVector())) {
            return new VVector(v1.getList().map((v:Value) => operTimes(List(v,v2))))
        } else if (v2.isVector() && !(v1.isVector())) {
            return new VVector(v2.getList().map((v:Value) => operTimes(List(v1,v))))
        } else {
            runtimeError("cannot multiply values of different types")
        }
    }


    def operMap (vs: List[Value]):Value = {

        checkArgsLength(vs,2,2)

        val vf = vs(0)
        val vv = vs(1)

        vf.checkFunction()
        vv.checkVector()

        val l = vv.getList()
        return new VVector(l.map((v:Value) => vf.apply(List(v))))
    }


    def operFilter (vs: List[Value]):Value = {

        checkArgsLength(vs,2,2)

        val vf = vs(0)
        val vv = vs(1)

        vf.checkFunction()
        vv.checkVector()

        def asBool (v:Value) : Boolean = {
            if (!v.isBoolean()) {
                runtimeError("filter predicate not returning a Boolean")
            }
            return v.getBool()
        }

        val l = vv.getList()
        return new VVector(l.filter((v:Value) => asBool(vf.apply(List(v)))))
    }


    def operEqual (vs: List[Value]) : Value = {

        checkArgsLength(vs,2,2)

        val v1 = vs(0)
        val v2 = vs(1)

        if (v1.isBoolean() && v2.isBoolean()) {
            return new VInteger(if (v1.getBool() == v2.getBool()) 1 else 0)
        } else if (v1.isInteger() && v2.isInteger()) {
            return new VInteger(if (v1.getInt() == v2.getInt()) 1 else 0)
        } else if (v1.isString() && v2.isString()) {
            return new VInteger(if (v1.getString() == v2.getString()) 1 else 0)
        } else if (v1.isVector() && v2.isVector()) {
            if (v1.getList().length == v2.getList().length) {
                for ((vv1,vv2) <- v1.getList().zip(v2.getList())) {
                    if (!operEqual(List(vv1,vv2)).getBool()) {
                        return new VInteger(0)
                    }
                }
                return new VInteger(1)
            } else {
                return new VInteger(0)
            }
        } else if (v1.isFunction() && v2.isFunction()) {
            return new VInteger(if (v1 == v2) 1 else 0)
        } else {
            return new VInteger(0)
        }
    }


    def operLess (vs: List[Value]) : Value = {

        checkArgsLength(vs,2,2)

        val v1 = vs(0)
        val v2 = vs(1)
        v1.checkInteger()
        v2.checkInteger()

        return new VInteger(if (v1.getBool() < v2.getBool()) 1 else 0)
    }

    def operVector (vs: List[Value]) : Value = {

        return new VVector(vs)
    }



    def operEmpty (vs : List[Value]) : Value = {

        checkArgsLength(vs,1,1)
        val v = vs(0)
        v.checkVector()
        return new VInteger(if (v.getList().length == 0) 1 else 0)
    }


    def operFirst (vs : List[Value]) : Value = {
        checkArgsLength(vs,1,1)
        val v = vs(0)
        v.checkVector()
        val l = v.getList()
        if (l.length == 0) {
            runtimeError("Taking first of an empty vector")
        }
        return l(0)
    }


    def operRest (vs : List[Value]) : Value = {
        checkArgsLength(vs,1,1)
        val v = vs(0)
        v.checkVector()
        val l = v.getList()
        if (l.length == 0) {
            runtimeError("Taking rest of an empty vector")
        }
        return new VVector(l.tail)
    }


    def operCons (vs : List[Value]) : Value = {
        checkArgsLength(vs,2,2)
        val item = vs(0)
        val vec = vs(1)
        vec.checkVector()
        return new VVector(item::vec.getList())
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

    def LP : Parser[Unit] = "(" ^^ { s => () }
    def RP : Parser[Unit] = ")" ^^ { s => () }
    def LB : Parser[Unit] = "[" ^^ { s => () }
    def RB : Parser[Unit] = "]" ^^ { s => () }
    def PLUS : Parser[Unit] = "+" ^^ { s => () }
    def TIMES : Parser[Unit] = "*" ^^ { s => () }
    def INT : Parser[Int] = """[0-9]+""".r ^^ { s => s.toInt }
    def IF : Parser[Unit] = "if" ^^ { s => () }
    def ID : Parser[String] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ { s => s }
    //    def ID : Parser[String] = """[a-zA-Z_+*:.?=<>!|][a-zA-Z0-9_+*:.?=<>!|]*""".r ^^ { s => s }

    def COND : Parser[Unit] = "cond" ^^ { s => () }
    def AND : Parser[Unit] = "and" ^^ { s => () }
    def OR : Parser[Unit] = "or" ^^ { s => () }
    def LET : Parser[Unit] = "let" ^^ { s => () }
    def FUN : Parser[Unit] = "fun" ^^ { s => () }
    def BAR : Parser[Unit] = "|" ^^ { s => () }
    def GETS : Parser[Unit] = "<-" ^^ { s => () }

    def DEFINE : Parser[Unit] = "define" ^^ { s => () }
    def DEFUN : Parser[Unit] = "defun" ^^ { s => () }

    def CONTROL : Parser[Unit] = "#" ^^ { s => () }

    def DICT : Parser[Unit] = "dict" ^^ { s => () }
    def SUB : Parser[Unit] = "sub" ^^ { s => () }
    def STRING : Parser[String] = "\"" ~ """[^"]*""".r ~ "\"" ^^ { case _ ~ s ~ _ => s }

    // grammar

    def vector : Parser[Exp] =
        rep(atomic) ^^ {
            case es =>  if (es.length == 1)
                es.head
            else
                new EApply(new ELiteral(new VPrimOp(Ops.operVector)),es)
        }

    def atomic_int : Parser[Exp] = INT ^^ { i => new ELiteral(new VInteger(i)) }

    def atomic_id : Parser[Exp] =
        ID ^^ { s => new EId(s) }

    def atomic : Parser[Exp] =
        ( atomic_int | atomic_id ) ^^ { e => e }


    //
    //    def atomic_string : Parser[Exp] = STRING ^^ { s => new ELiteral(new VString(s))}
    //

    //
    //    def atomic : Parser[Exp] =
    //        ( atomic_int | atomic_id | atomic_string ) ^^ { e => e }
    //
    //    def expr_if : Parser[Exp] =
    //        LP ~ IF ~ expr ~ expr ~ expr ~ RP ^^
    //            { case _ ~ _ ~ e1 ~ e2 ~ e3 ~ _ => new EIf(e1,e2,e3) }
    //
    //    def expr_map : Parser[Exp] =
    //        LB ~ expr ~ BAR ~ ID ~ GETS ~ expr ~ RB ^^
    //            { case _ ~ e1 ~ _ ~ id ~ _ ~ e2 ~ _ =>
    //                new EApply(new ELiteral(new VPrimOp(Ops.operMap)),List(new EFunction(List(id),e1), e2)) }
    //
    //    def expr_mapfilter : Parser[Exp] =
    //        LB ~ expr ~ BAR ~ ID ~ GETS ~ expr ~ BAR ~ expr ~ RB ^^
    //            { case _ ~ e1 ~ _ ~ id ~ _ ~ e2 ~ _ ~ e3 ~ _ =>
    //                new EApply(new ELiteral(new VPrimOp(Ops.operMap)),
    //                    List(new EFunction(List(id),e1),
    //                        new EApply(new ELiteral(new VPrimOp(Ops.operFilter)),
    //                            List(new EFunction(List(id),e3),
    //                                e2)))) }
    //
    //    def expr_vec : Parser[Exp] =
    //        LB ~ rep(expr) ~ RB ^^ {
    //            case _ ~ es ~ _ => new EApply(new ELiteral(new VPrimOp(Ops.operVector)),es)
    //        }
    //
    //    def key_value : Parser[(Exp, Exp)] =
    //        LP ~ expr ~ expr ~ RP ^^ { case _ ~ k ~ v ~ _ => (k, v) }
    //
    //
    //    def expr_fun : Parser[Exp] =
    //        LP ~ FUN ~ LP ~ rep(ID) ~ RP ~ expr ~ RP ^^
    //            { case _ ~ _ ~ _ ~ params ~ _ ~ e ~ _ => new EFunction(params,e) }
    //
    //    def expr_funr : Parser[Exp] =
    //        LP ~ FUN ~ ID ~ LP ~ rep(ID) ~ RP ~ expr ~ RP ^^
    //            { case _ ~ _ ~ self ~ _ ~ params ~ _ ~ e ~ _ => new ERecFunction(self,params,e) }
    //
    //    def expr_app : Parser[Exp] =
    //        LP ~ expr ~ rep(expr) ~ RP ^^ { case _ ~ ef ~ eargs ~ _ => new EApply(ef,eargs) }
    //
    //    def andTokens(es : List[Exp]) : Exp = es match {
    //        // here
    //        //     case e1 :: Nil => e1
    //        // is also okay
    //        // just to fit the result of #parse (and true false)
    //        case e1 :: Nil => new EIf(e1, new ELiteral(
    //            new VInteger(1)), new ELiteral(new VInteger(0)))
    //        case e1 :: e2 => new EIf(e1, andTokens(e2), new ELiteral(new VInteger(0)))
    //        case Nil => throw new Exception("Empty Boolean expression for and")
    //    }
    //
    //    def expr_and : Parser[Exp] =
    //        LP ~ AND ~ rep(expr) ~ RP ^^ { case _ ~ _ ~ es ~ _ => andTokens(es) }
    //
    //    def orTokens(es : List[Exp]) : Exp = es match {
    //        case e1 :: Nil => new EIf(e1, new ELiteral(
    //            new VInteger(1)), new ELiteral(new VInteger(0)))
    //        case e1 :: e2 => new EIf(e1, new ELiteral(new VInteger(1)), orTokens(e2))
    //        case Nil => throw new Exception("Empty Boolean expression for or")
    //    }
    //
    //    def expr_or : Parser[Exp] =
    //        LP ~ OR ~ rep(expr) ~ RP ^^ { case _ ~ _ ~ es ~ _ => orTokens(es) }
    //
    //    def binding : Parser[(String, Exp)] =
    //        LP ~ ID ~ expr ~ RP ^^ { case _ ~ id ~ e ~ _ => (id, e) }
    //
    //    def expr_let : Parser[Exp] =
    //        LP ~ LET ~ LP ~ rep(binding) ~ RP ~ expr ~ RP ^^ {
    //            case _ ~ _ ~ _ ~ wrap ~ _ ~ e ~ _ =>
    //                val (pars, efs) = wrap.unzip
    //                new EApply(new EFunction(pars, e), efs)
    //        }
    //
    //    def cond : Parser[(Exp, Exp)] =
    //        LP ~ expr ~ expr ~ RP ^^ { case _ ~ e1 ~ e2 ~ _ => (e1, e2) }
    //
    //    def condTokens(es : List[(Exp, Exp)]) : Exp = es match {
    //        case (e1, e2) :: Nil => new EIf(e1, e2, new ELiteral(new VNone))
    //        case (e1, e2) :: e3 => new EIf(e1, e2, condTokens(e3))
    //        case Nil => throw new Exception("Empty Boolean expression for cond")
    //    }
    //
    //    def expr_cond : Parser[Exp] =
    //        LP ~ COND ~ rep(cond) ~ RP ^^ {case _ ~ _ ~ es ~ _ => condTokens(es) }
    //
    //    def expr : Parser[Exp] =
    //        ( atomic | expr_if | expr_map | expr_mapfilter | expr_vec |
    //            expr_fun | expr_funr | expr_let | expr_and | expr_or | expr_cond | expr_app) ^^
    //            { e => e }

    def expr : Parser[Exp] =
        ( vector ) ^^ { e => e }

    def expr_entry : Parser[ShellEntry] =
        expr ^^ { e => new SEexpr(e) }


    def define_entry : Parser[ShellEntry] =
        LP ~ DEFINE ~ ID ~ expr ~ RP ^^ {
            case _ ~ _ ~ id ~ e ~ _ => new SEdefine(id, e)
        }

    def defun_entry : Parser[ShellEntry] =
        LP ~ DEFUN ~ ID ~ LP ~ rep(ID) ~ RP ~ expr ~ RP ^^ {
            case _ ~ _ ~ fid ~ _ ~ ids ~ _ ~ e ~ _ =>
                new SEdefine(fid, new ERecFunction(fid, ids, e))
        }

    def quit_entry : Parser[ShellEntry] =
        "quit" ^^ { case _ => new SEquit() }

    def env_entry : Parser[ShellEntry] =
        "env" ^^ { case _ => new SEenv() }

    def parse_entry : Parser[ShellEntry] =
        "parse" ~ expr ^^ { case _ ~ e => new SEparse(e) }

    def control_entry : Parser[ShellEntry] =
        CONTROL ~ ( quit_entry | env_entry | parse_entry ) ^^ { case _ ~ se => se }


    def shell_entry : Parser[ShellEntry] =
        ( control_entry | define_entry | defun_entry | expr_entry ) ^^ { se => se }


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
        println(id + " defined")
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

        parser.parseAll(parser.shell_entry, input) match {
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
        ("+", new VPrimOp(Ops.operPlus)),
        ("*", new VPrimOp(Ops.operTimes)),
        ("=", new VPrimOp(Ops.operEqual)),
        ("<", new VPrimOp(Ops.operLess)),
        ("map", new VPrimOp(Ops.operMap)),
        ("filter", new VPrimOp(Ops.operFilter)),
        ("ifempty",new VPrimOp(Ops.operEmpty)),
        ("first",new VPrimOp(Ops.operFirst)),
        ("rest",new VPrimOp(Ops.operRest)),
        ("empty",new VVector(List())),
        ("cons",new VPrimOp(Ops.operCons)),
    ))


    def shell () : Unit = {

        var env = stdEnv

        while (true) {
            print("FUNC> ")
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

