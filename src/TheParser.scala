import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by Golpar on 1/11/2018 AD.
  */


object TheParser extends JavaTokenParsers {
  var header = "#include <iostream>\n#include <exception>\nusing namespace std;\nstruct Expired : public exception {\n    const char * what () const throw () {\n      return \"TOO LATE\";\n    }\n};\n\ndouble getMax(double (*f)(int _t),int start){\n  int now = start;\n  int maxVal = 0;\n  int temp;\n  while (now<365*24){\n    try {\n      temp = (*f)(now);\n      if(temp> maxVal){\n        maxVal = temp;\n      }\n    }\n    catch (Expired e){\n    }\n    now ++;\n  }\n  return maxVal;\n} \n"
  var functionCounter: Long = 0
  var fcreated = false

  //done
  def TYPE: Parser[String] = "Int" ^^ { case temp => "int" } | "Double" ^^ { case temp => "double" } | "Date" ^^ { case temp => "int" } | "Contract" ^^ { case temp => "Contract" }

  //done
  def NAME: Parser[String] = ident ^^ { a => a }


  def DEFFUNC = ("(" ~ TYPE ~ ")" ~ "->" ~ TYPE) ^^ { case p1 ~ a ~ p2 ~ felesh ~ taip => p1 + a + p2 + felesh + taip } | ("(" ~ TYPE ~ rep("," ~ TYPE) ~ ")" ~ "->" ~ TYPE) ^^ {
    case p1 ~ t ~ list ~ p2 ~ felesh ~ taip => {
      p1 + (list.foldLeft(t) { case (x, "," ~ b) => x + "," + b }) + p2 + felesh + taip
    }
  }

  //done
  def DEFVAR = TYPE

  def DEF = NAME ~ "::" ~ DEFVAR ^^ {
    case name ~ "::" ~ dec => {
      var temp = ""
      temp = dec + " " + name + ";"
      if (dec == "Contract") {
        temp = "double (*" + name + ") (int) ;"
      }
      "^ " + temp
    }
  } | NAME ~ "::" ~ DEFFUNC ^^ {
    case name ~ "::" ~ dec => {
      val temp2 = dec.split("->")
      var answer = temp2(1) + "  " + name + temp2(0) + ";"
      "^? " + answer
    }
  } | NAME ~ "::" ~ DEFTIMEFUNC ^^ {
    case name ~ "::" ~ dec => {
      val temp2 = dec.split("->")
      var answer = temp2(1) + " (*_" + functionCounter.toString + ") " + temp2(0) + ";"
      functionCounter += 1
      "^ " + answer
    }
  }

  def DEFTIMEFUNC = "TimeFunc (Date) -> Int" ^^ { a => "(int) -> double" } //todo: in dorost she.

  def ASSIGN: Parser[String] = NAME ~ "=" ~ EXPR ^^ { case name ~ "=" ~ exp => {
    var temp = exp.split('^')
    var results: String = ""
    if (temp(0).endsWith("(_t)")) {
      results += temp(1) + "$" + name + " = " + temp(0).substring(0, temp(0).length - 4) + ";"
    }
    else {
      results += temp(1) + "$" + name + " = " + temp(0).substring(0, temp(0).length) + ";"
    }
    results
  }
  }

  //done
  def INT: Parser[String] = NAME ^^ { a => a } | decimalNumber ^^ { a => a }


  def DOUBLE: Parser[String] = NAME ^^ { a => a } | floatingPointNumber ^^ { a => a }

  //done
  def DATE: Parser[String] = NAME ^^ { a => a }

  def ARG: Parser[String] = EXPR

  def ARGS: Parser[String] = (ARG ~ rep("," ~ ARG)) ^^ { case (a ~ list) => {
    var a_parts = a.split('^')
    var main = a_parts(0)
    var depend = a_parts(1)
    list.foldLeft("") { case (x, "," ~ b) => {
      var temp = b.split('^')
      main = main + " , " + temp(0)
      depend = depend + " \n " + temp(1)
      ""
    }
    }
    main + "^ " + depend
  }
  } | ARG ^^ { a => a }

  def EXPR: Parser[String] = "(" ~ EXPR ~ ")" ~ EXP_PRIME ^^ { case "(" ~ a ~ ")" ~ b => {
    var parts1 = a.split('^')
    var parts2 = b.split('^')
    "(" + parts1(0) + ")" + parts2(0) + "^ " + parts1(1) + "\n" + parts2(1)
  }
  } | "(" ~ EXPR ~ ")" ^^ { case "(" ~ a ~ ")" => {
    var parts1 = a.split('^')
    "(" + parts1(0) + ")" + " " + '^' + " " + parts1(1) + "\n"
  }
  } | PUREEXPR ~ EXP_PRIME ^^ { case a ~ b => {
    //    println("exp2  " + a)
    var parts1 = a.split('^')
    var parts2 = b.split('^')
    "(" + parts1(0) + ")" + parts2(0) + "^ " + parts1(1) + "\n" + parts2(1)
  }
  } | PUREEXPR ^^ {
    { a =>
      //    println("exp3  " + a)
      a
    }
  }

  //    (MULT ~ EXP_PRIME) ^^ {
  //    case p1 ~ p2 => {
  //      var parts1 = p1.split('^')
  //      var parts2 = p2.split('^')
  //      println("exp1: " + parts1(0) +  parts2(0) + "^" + parts1(1) + " \n " + parts2(1))
  //      parts1(0) +  parts2(0) + "^" + parts1(1) + " \n " + parts2(1)
  //    }
  //  }|  MULT ^^ { a => {
  //    println("exp2: " + a)
  //    a} }

  def EXP_PRIME: Parser[String] = ("+" ~ EXPR) ^^ { case "+" ~ a => " + " + a } | ("-" ~ EXPR) ^^ { case "-" ~ a => " - " + a } | ("*" ~ EXPR) ^^ { case "*" ~ a => " * " + a } | ("/" ~ EXPR) ^^ { case "/" ~ a => " / " + a }


  //    : Parser[String]
  //  def MULT: Parser[String] = "(" ~ MULT ~ ")" ~ MULT_PRIME ^^ {
  //    case "(" ~ a ~ ")" ~ b => {
  //      var parts1 = a.split('^')
  //      var parts2 = b.split('^')
  //      "(" + parts1(0) + ")" + parts2(0) + "^ "+ parts1(1) + "\n"+ parts2(1)
  //    } } | "(" ~ MULT ~ ")"^^ { case "(" ~ a ~ ")" => {
  //    var parts1 = a.split('^')
  //    "(" + parts1(0) + ")" + "^ "+ parts1(1) + "\n"
  //  } }|(PUREEXPR ~ MULT_PRIME)  ^^ {
  //    case p1 ~  p2 => {
  //      var parts1 = p1.split('^')
  //      var parts2 = p2.split('^')
  //      parts1(0) + parts2(0) + "^" + parts1(1) + " \n " + parts2(1)
  //    } }| PUREEXPR ^^ { a => a }
  //
  //  def MULT_PRIME: Parser[String] = ("*" ~ EXPR) ^^ {case "*"~a => " * " + a} | ("/" ~ EXPR) ^^ {case "/"~a => " / " + a}


  def PUREEXPR: Parser[String] = FUNCCALL ^^ {
    { a =>
      //    println("funccall  " + a)
      a
    }
  } | INT ^^ { a => a + "^ " } | DOUBLE ^^ { a => a + "^ " }

  //  def TWOARGS: Parser[String] = "(" ~ ARG ~ "," ~ ARG ~ ")" ^^ {  }

  def FUNCCALL: Parser[String] = (
    ("one()") ^^ { case a => {
      var theId = "_" + functionCounter.toString
      var theFunction = "double " + theId + "(int _t = 0){\n    return 1;\n  } \n"
      functionCounter += 1
      theId + "(_t)" + '^' + theFunction
    }
    }
      | ("give" ~ "(" ~ ARG ~ ")") ^^ {
      case a ~ b ~ arg ~ c => {
        var new_arg = arg
        var theId = "_" + functionCounter
        var parts = new_arg.split('^')
        if (!parts(0).contains("(_t)")) {
          parts(0) = parts(0) + "(_t)"
        }
        var theFunction: String = "double " + theId + "(int _t = 0){\n return -" + parts(0) + ";\n}\n"
        functionCounter += 1
        var appendix: String = parts(1) + theFunction
        theId + "(_t)" + '^' + appendix
      }
    }
      | ("scale" ~ "(" ~ ARG ~ "," ~ ARG ~ ")") ^^ {
      case "scale" ~ "(" ~ coef ~ "," ~ func ~ ")" => {
        var new_func = func
        var coef_parts = coef.split('^')
        var func_parts = new_func.split('^')
        if (!func_parts(0).contains("(_t)")) {
          func_parts(0) = func_parts(0) + "(_t)"
        }
        var appendix = coef_parts(1) + "\n" + func_parts(1)
        var theId = "_" + functionCounter
        var theFunction: String = "double " + theId + "(int _t = 0){\n return ( " + coef_parts(0) + ") *" + func_parts(0) + ";\n}\n"
        functionCounter += 1
        appendix = appendix + "\n" + theFunction
        theId + "(_t)" + '^' + appendix
      }
    } | ("mkdate" ~ "(" ~ ARG ~ "," ~ ARG ~ ")") ^^ {
      case "mkdate" ~ "(" ~ day ~ "," ~ hour ~ ")" => {
        var temp1 = day.split('^')
        var temp2 = hour.split('^')
        var result = "((" + temp1(0) + " -1 )" + " * 24 + " + temp2(0) + ")" + " ^ "
        result
      }
    } | ("truncate" ~ "(" ~ ARG ~ "," ~ ARG ~ ")") ^^ {
      case "truncate" ~ "(" ~ date ~ "," ~ func ~ ")" => {
        var new_func = func
        var date_parts = date.split('^')
        var func_parts = new_func.split('^')
        if (!func_parts(0).contains("(_t)")) {
          func_parts(0) = func_parts(0) + "(_t)"
        }
        var appendix = date_parts(1) + "\n" + func_parts(1)
        var theId = "_" + functionCounter
        var theFunction: String = "double " + theId + "(int _t=0){\n          if (_t<= " + date_parts(0) + "){\n            return " + func_parts(0) + "; \n } \n throw Expired(); \n }"
        functionCounter += 1
        appendix = appendix + "\n" + theFunction
        theId + "(_t)" + '^' + appendix
      }
    } | ("and" ~ "(" ~ ARG ~ "," ~ ARG ~ ")") ^^ {
      case "and" ~ "(" ~ arg1 ~ "," ~ arg2 ~ ")" => {
        //          println("in and")
        var theId = "_" + functionCounter
        var parts1 = arg1.split('^')
        var parts2 = arg2.split('^')
        if (!parts1(0).contains("(_t)")) {
          parts1(0) = parts1(0) + "(_t)"
        }
        if (!parts2(0).contains("(_t)")) {
          parts2(0) = parts2(0) + "(_t)"
        }
        //          try {
        //            return c1();
        //          }
        //          catch (Expired e){
        //            return ctruncate(t);
        //          }
        var theFunction: String = "double " + theId + "(int _t = 0){\n int payoff = 0; try{payoff += " + parts1(0) + ";}\n  catch (Expired e){} \n " + "try{payoff += " + parts2(0) + ";}\n  catch (Expired e){} \n return payoff; }"
        functionCounter += 1
        var appendix: String = parts1(1) + "\n" + parts2(1) + "\n" + theFunction
        theId + "(_t)" + '^' + appendix
      }
    } | ("then" ~ "(" ~ ARG ~ "," ~ ARG ~ ")") ^^ {
      case "then" ~ "(" ~ arg1 ~ "," ~ arg2 ~ ")" => {
        var theId = "_" + functionCounter
        var parts1 = arg1.split('^')
        var parts2 = arg2.split('^')
        if (!parts1(0).contains("(_t)")) {
          parts1(0) = parts1(0) + "(_t)"
        }
        if (!parts2(0).contains("(_t)")) {
          parts2(0) = parts2(0) + "(_t)"
        }
        //        double cthen(int t=0){
        //          try {
        //            return c1();
        //          }
        //          catch (Expired e){
        //            return ctruncate(t);
        //          }
        //        }
        var theFunction: String = "double " + theId + "(int _t = 0){\n try{ return " + parts1(0) + ";}\n  catch (Expired e){} \n " + "try{return " + parts2(0) + ";}\n  catch (Expired e){} \n return 0; }"
        functionCounter += 1
        var appendix: String = parts1(1) + "\n" + parts2(1) + "\n" + theFunction
        theId + "(_t)" + '^' + appendix
      }
    } | (NAME ~ "(" ~ ARGS ~ ")") ^^ {
      case name ~ "(" ~ args ~ ")" => {
        var arg_parts = args.split('^')
        //        if (!arg_parts(0).contains("(_t)")) {
        //          arg_parts(0) = arg_parts(0) + "(_t)"
        //        } todo find what this means
        name + "(" + arg_parts(0) + ")" + "^ " + arg_parts(1)
      }
    }
    //    | ("or" ~ TWOARGS)
    //    | ("scaleX" ~ TWOARGS)
    )


  /*var theId = "_" + functionCounter
    var parts = arg.split("$")
    var theFunction : String= "double " + theId + "(int t = 0){\n return -" + parts(0) + ";\n}\n"
    functionCounter += 1
    var appendix : String= parts(1) + theFunction
    theId + "(t)" + "$" + appendix
    */
  def CONTRACTNAME: Parser[String] = NAME ^^ { a => a }

  def PROGRAM = rep(ASSIGN | FUNCCALL | DEF) ~ "END" ~ INT ~ INT ~ rep(CONTRACTNAME) ^^ {
    case inside ~ "END" ~ lines ~ time ~ contracts => {
      var answer = ""
      var flag = false
      for (i <- 0 until inside.length) {
        if (flag) {
          flag = false
        }
        else if (inside(i).charAt(0) == '^') {
          var declare_statment = inside(i)
          var assignment_statment = inside(i + 1)
          var declare = declare_statment.substring(1, declare_statment.length - 1)
          var index = assignment_statment.indexOf('$')
          var preq = assignment_statment.substring(0, index)
          var assign = assignment_statment.substring(index + 1, assignment_statment.length)
          var rightside = assign.split("=")(1)
          if (declare.charAt(0) == ('?')) {
            declare = declare.substring(1, declare.length)
            var declare_parts = declare.split(',')
            println("declare = " + declare)
            var final_result = ""
            for (i <- 1 to declare_parts.length) {
              if (i == declare_parts.length) {
                final_result = final_result + declare_parts(i - 1).substring(0, declare_parts(i - 1).length - 1) + " arg" + i.toString
              } else {
                final_result = final_result + declare_parts(i - 1) + " arg" + i.toString
              }
              if (i != declare_parts.length) {
                final_result = final_result + ","
              }
            }
            final_result = final_result + "){ \n  return " + rightside + "\n }"
            answer = answer + preq + "\n" + final_result + "\n"
          }
          else {
            answer = answer + preq + "\n" + declare + " = " + rightside + "\n"
          }
          flag = true
        }
        else {
          answer = answer + inside(inside.length - 1 - i) + "\n"
        }
      }
      answer = header + answer + "\n int main(){ \n" + s"int start = $time;" + "\n cout<< "
      //      contracts.foreach((a) => answer = answer + " getMax(&" + a + ",start)+")
      var temp: ListBuffer[String] = ListBuffer()
      for (i <- 0 until contracts.length) {
        if (!temp.contains(contracts(i))) {
          answer = answer + " getMax(" + contracts(i) + ",start)+"
          temp += contracts(i)
        }
      }
      answer = answer + "0" + " <<endl; \n return 0; \n} "
      answer
    }
  }


  def get_input(): String = {
    var temp = "notEND"
    var input_string = ""
    while (temp != "END") {
      temp = scala.io.StdIn.readLine()
      input_string = input_string + temp + "\n"
    }
    temp = scala.io.StdIn.readLine()
    input_string = input_string + temp
    var parts = temp.split(" ")
    var number_of_lines = parts(0).toInt
    for (line <- 1 to number_of_lines) {
      temp = scala.io.StdIn.readLine()
      input_string = input_string + temp + "\n"
    }
    input_string.substring(0, input_string.length - 1)
  }

  def isAnAssignment(s: String): Boolean = {
    s.contains("=")
  }

  def preprocess(input: String): String = {
    val assigned = new mutable.HashMap[String, String]()
    var temp = "notEND"
    var toBeDeleted: ListBuffer[Int] = ListBuffer()
    var inputLines = input.split("[\\r\\n]+")
    for (i <- 0 until inputLines.length) {
      val thisLine = inputLines(i)
      if (isAnAssignment(thisLine)) {
        if (assigned.contains(thisLine.substring(0, thisLine.indexOf("=") - 1))) {
          toBeDeleted += i-1
        } else {
          assigned += (thisLine.substring(0, thisLine.indexOf("=") - 1) -> thisLine.substring(thisLine.indexOf("="), thisLine.length))
        }
      }
    }
    var newInput = ""
    for (i <- 0 until inputLines.length) {
      if (!toBeDeleted.contains(i)) {
        newInput = newInput + "\n" + inputLines(i)
      }
    }
    newInput.substring(1,newInput.length)
  }

  //todo dorost kardan type contract baraye function


  def main(args: Array[String]) {
    var lang = "i5 :: Contract\ni5 = one()\nz6 :: Contract\nz6 = scale(30,i5)\nke :: Date\nke = mkdate(6,9)\nt9 :: Contract\nt9 = truncate(ke,z6)\nd1 :: Contract\nd1 = and(i5,t9)\ni7 :: Contract\ni7 = one()\ni3 :: Contract\ni3 = one()\nx6 :: Contract\nx6 = and(t9,i3)\na3 :: Contract\na3 = one()\ns4 :: Contract\ns4 = scale(34,a3)\nsf :: Date\nsf = mkdate(1,11)\nk5 :: Contract\nk5 = truncate(sf,s4)\nh6 :: Contract\nh6 = give(k5)\nc6 :: Contract\nc6 = one()\ni9 :: Contract\ni9 = scale(6,c6)\nhj :: Date\nhj = mkdate(0,6)\nw5 :: Contract\nw5 = truncate(hj,i9)\nm8 :: Contract\nm8 = scale(38,w5)\nf2 :: Contract\nf2 = scale(24,m8)\ni6 :: Contract\ni6 = one()\nx5 :: Contract\nx5 = one()\nx4 :: Contract\nx4 = and(x6,x6)\nrn :: Date\nrn = mkdate(4,16)\ns3 :: Contract\ns3 = truncate(rn,x4)\no1 :: Contract\no1 = give(s3)\nv8 :: Contract\nv8 = and(o1,s4)\nj3 :: Contract\nj3 = and(a3,i3)\ng9 :: Contract\ng9 = and(m8,i5)\nk8 :: Contract\nk8 = scale(0,g9)\niy :: Date\niy = mkdate(1,7)\ne5 :: Contract\ne5 = truncate(iy,k8)\nk6 :: Contract\nk6 = scale(25,e5)\nf9 :: Contract\nf9 = and(x4,z6)\nx3 :: Contract\nx3 = scale(35,f9)\nb2 :: Contract\nb2 = give(x3)\na5 :: Contract\na5 = and(b2,b2)\nj5 :: Contract\nj5 = and(w5,m8)\nb7 :: Contract\nb7 = one()\nr9 :: Contract\nr9 = one()\nks :: Date\nks = mkdate(1,18)\nh0 :: Contract\nh0 = truncate(ks,r9)\nt1 :: Contract\nt1 = and(i6,b7)\nc0 :: Contract\nc0 = scale(10,t1)\nx0 :: Contract\nx0 = one()\nc9 :: Contract\nc9 = and(i7,s4)\nhm :: Date\nhm = mkdate(0,4)\nq2 :: Contract\nq2 = truncate(hm,c9)\nev :: Date\nev = mkdate(2,14)\nv4 :: Contract\nv4 = truncate(ev,q2)\ns1 :: Contract\ns1 = and(h6,t1)\nt2 :: Contract\nt2 = one()\nb4 :: Contract\nb4 = give(t2)\nf0 :: Contract\nf0 = and(e5,c6)\nf3 :: Contract\nf3 = and(x3,q2)\nt7 :: Contract\nt7 = one()\nc3 :: Contract\nc3 = give(t7)\nr2 :: Contract\nr2 = one()\nvb :: Date\nvb = mkdate(4,20)\nv6 :: Contract\nv6 = truncate(vb,r2)\ny6 :: Contract\ny6 = give(v6)\nq3 :: Contract\nq3 = and(i7,s1)\nb8 :: Contract\nb8 = one()\ne1 :: Contract\ne1 = and(i3,r2)\nn5 :: Contract\nn5 = scale(22,e1)\nm2 :: Contract\nm2 = give(n5)\npg :: Date\npg = mkdate(8,12)\nl9 :: Contract\nl9 = truncate(pg,m2)\nip :: Date\nip = mkdate(0,10)\ne0 :: Contract\ne0 = truncate(ip,l9)\nd0 :: Contract\nd0 = scale(37,e0)\nz3 :: Contract\nz3 = scale(20,d0)\nb1 :: Contract\nb1 = and(b2,x0)\na8 :: Contract\na8 = and(f2,v4)\ny5 :: Contract\ny5 = give(a8)\nd3 :: Contract\nd3 = give(y5)\nx7 :: Contract\nx7 = give(d3)\ng0 :: Contract\ng0 = give(x7)\na6 :: Contract\na6 = one()\nm3 :: Contract\nm3 = and(i3,a5)\ns0 :: Contract\ns0 = one()\nu5 :: Contract\nu5 = and(i6,e5)\ni1 :: Contract\ni1 = and(x0,j5)\ng4 :: Contract\ng4 = give(i1)\nl6 :: Contract\nl6 = scale(36,g4)\nf8 :: Contract\nf8 = and(c6,f2)\nq4 :: Contract\nq4 = and(w5,m8)\nfx :: Date\nfx = mkdate(5,2)\ni4 :: Contract\ni4 = truncate(fx,q4)\nh9 :: Contract\nh9 = give(i4)\nv0 :: Contract\nv0 = give(h9)\ny7 :: Contract\ny7 = scale(2,v0)\nu2 :: Contract\nu2 = give(y7)\nr6 :: Contract\nr6 = give(u2)\nk1 :: Contract\nk1 = give(r6)\nw4 :: Contract\nw4 = give(k1)\np7 :: Contract\np7 = give(w4)\nc7 :: Contract\nc7 = one()\nq8 :: Contract\nq8 = scale(41,c7)\ne7 :: Contract\ne7 = and(k1,u2)\nw7 :: Contract\nw7 = and(b1,a5)\nx9 :: Contract\nx9 = and(t7,t9)\nmw :: Date\nmw = mkdate(0,17)\nh2 :: Contract\nh2 = truncate(mw,x9)\nz9 :: Contract\nz9 = give(h2)\nt3 :: Contract\nt3 = scale(27,z9)\ny9 :: Contract\ny9 = one()\nl7 :: Contract\nl7 = one()\nh3 :: Contract\nh3 = one()\nd5 :: Contract\nd5 = and(q8,b7)\nrr :: Date\nrr = mkdate(3,2)\ne6 :: Contract\ne6 = truncate(rr,d5)\nv3 :: Contract\nv3 = one()\nb3 :: Contract\nb3 = and(r9,d3)\np2 :: Contract\np2 = give(b3)\nb0 :: Contract\nb0 = and(l7,s3)\nz2 :: Contract\nz2 = scale(12,b0)\nrt :: Date\nrt = mkdate(9,6)\nq6 :: Contract\nq6 = truncate(rt,z2)\ny4 :: Contract\ny4 = scale(20,q6)\nn7 :: Contract\nn7 = and(a3,w7)\njm :: Date\njm = mkdate(6,0)\nf7 :: Contract\nf7 = truncate(jm,n7)\nz8 :: Contract\nz8 = scale(42,f7)\ng6 :: Contract\ng6 = and(x9,h9)\nu7 :: Contract\nu7 = and(w5,g9)\nn1 :: Contract\nn1 = scale(48,u7)\nb9 :: Contract\nb9 = one()\nt5 :: Contract\nt5 = and(l9,k5)\nl0 :: Contract\nl0 = one()\nw1 :: Contract\nw1 = scale(4,l0)\nu0 :: Contract\nu0 = scale(10,w1)\nv9 :: Contract\nv9 = scale(42,u0)\nk4 :: Contract\nk4 = one()\na9 :: Contract\na9 = scale(1,k4)\nf4 :: Contract\nf4 = one()\nz7 :: Contract\nz7 = and(k8,x0)\ns6 :: Contract\ns6 = and(b3,w7)\ng2 :: Contract\ng2 = one()\nd4 :: Contract\nd4 = scale(13,g2)\nza :: Date\nza = mkdate(3,8)\nl4 :: Contract\nl4 = truncate(za,d4)\nq1 :: Contract\nq1 = give(l4)\na2 :: Contract\na2 = and(b0,z3)\nj6 :: Contract\nj6 = and(v0,c0)\ne3 :: Contract\ne3 = give(j6)\na1 :: Contract\na1 = one()\nw0 :: Contract\nw0 = scale(36,a1)\no9 :: Contract\no9 = and(w5,r6)\nh4 :: Contract\nh4 = give(o9)\nk7 :: Contract\nk7 = and(y4,l7)\nj0 :: Contract\nj0 = and(w7,q4)\np1 :: Contract\np1 = and(p7,n5)\ny3 :: Contract\ny3 = scale(5,p1)\nm5 :: Contract\nm5 = scale(28,y3)\nn3 :: Contract\nn3 = give(m5)\na7 :: Contract\na7 = and(i3,z7)\ne9 :: Contract\ne9 = one()\nh7 :: Contract\nh7 = give(e9)\nps :: Date\nps = mkdate(8,2)\np4 :: Contract\np4 = truncate(ps,h7)\no7 :: Contract\no7 = and(v0,x6)\nv7 :: Contract\nv7 = scale(46,o7)\nw3 :: Contract\nw3 = give(v7)\nq9 :: Contract\nq9 = scale(41,w3)\nlq :: Date\nlq = mkdate(5,1)\nm1 :: Contract\nm1 = truncate(lq,q9)\np6 :: Contract\np6 = and(m2,n7)\nn8 :: Contract\nn8 = give(p6)\nr0 :: Contract\nr0 = scale(1,n8)\nEND\n44 4\no7\nk7\nx4\ny4\ng4\ni5\nz2\ne6\nw5\nc6\na8\nx5\ns0\nc7\nh9\na2\nh4\nr0\nb0\nw4\nu2\nc0\ni4\nn7\ni1\nm1\nr2\nf9\nz6\nw3\nm5\nd0\ni7\nd3\nt9\nt7\nf4\na5\no1\nq4\no9\nq1\nw1\nl7"
    //    println(lang)
    //    var lang = get_input()
    lang = preprocess(lang)
    print(lang)
    val body = parseAll(PROGRAM, lang).get //.asInstanceOf[List[String]]
    val writer = new PrintWriter(new File("code.cpp"))
    writer.write(body)
    writer.close()
    //    println(body)
    //    val temp = body.foldLeft()((a,b) => a+"\n"+b)
  }


}


// todo maloom kardane in ke pure expression kojaha hast o ina (baraye pegah)

//a::Contract
//a = give(scale(20.0, one()))
//b:: Contract
//b = give(scale(10*2+2.0,a))
//END
//4 1
//a
//b
//a
//b