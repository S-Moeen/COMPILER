import java.io.{File, PrintWriter}

import com.sun.org.apache.bcel.internal.generic.MULTIANEWARRAY
import com.sun.org.apache.xpath.internal.Arg
import sun.reflect.annotation.ExceptionProxy

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util

// TODO: define kardane function e custom

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

  //  def DEFFUNC = "(" ~ (TYPE | TYPE ~ rep("," ~ TYPE)) ~ ")" ~ "->" ~ TYPE ^^ {
  //done
  def DEFFUNC = ("(" ~ TYPE ~ ")" ~ "->" ~ TYPE) ^^ { case p1 ~ a ~ p2 ~ felesh ~ taip => p1 + a + p2 + felesh + taip } | ("(" ~ TYPE ~ rep("," ~ TYPE) ~ ")" ~ "->" ~ TYPE) ^^ { case p1 ~ t ~ list ~ p2 ~ felesh ~ taip => p1 + (list.foldLeft(t) { case (x, "," ~ b) => x + "," + b }) + p2 + felesh + taip
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
      var answer = temp2(1) +"  "+ name  + temp2(0) + ";"
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

  def DEFTIMEFUNC = "TimeFunc (Date) -> Int" ^^ { a => "(int) -> double" }

  def ASSIGN: Parser[String] = NAME ~ "=" ~ EXPR ^^ { case name ~ "=" ~ exp => {
    println(name)
    println(exp)
    var temp = exp.split('^')
    var results :String = ""
    if(temp(0).endsWith("(_t)")){
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
      var  temp = b.split('^')
      main = main + " , " + temp(0)
      depend = depend + " \n " + temp(1)
      ""
    }}
    main + "^ " + depend
  } } | ARG ^^ { a => a }

  def EXPR: Parser[String] =   "(" ~ EXPR ~ ")" ~EXP_PRIME ^^ { case "(" ~ a ~ ")" ~ b => {
    var parts1 = a.split('^')
    var parts2 = b.split('^')
    "(" + parts1(0) + ")" + parts2(0) + "^ "+ parts1(1) + "\n"+ parts2(1)
  } } | "(" ~ EXPR ~ ")"^^ { case "(" ~ a ~ ")" => {
    var parts1 = a.split('^')
    "(" + parts1(0) + ")" +" "+ '^'+" "+ parts1(1) + "\n"
  } } | PUREEXPR ~ EXP_PRIME ^^ { case a ~ b => {
    println("exp2  " + a)
    var parts1 = a.split('^')
    var parts2 = b.split('^')
    "(" + parts1(0) + ")" + parts2(0) + "^ "+ parts1(1) + "\n"+ parts2(1)
  } } | PUREEXPR ^^ {{ a =>
    println("exp3  " + a)
    a
  }}
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

  def EXP_PRIME: Parser[String] = ("+" ~ EXPR) ^^ {case "+"~a => " + " + a} | ("-" ~ EXPR) ^^ {case "-"~a => " - " + a} | ("*" ~ EXPR) ^^ {case "*"~a => " * " + a} | ("/" ~ EXPR) ^^ {case "/"~a => " / " + a}


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


  def PUREEXPR: Parser[String] = FUNCCALL ^^ {{ a =>
    println("funccall  " + a)
    a
  }} | INT ^^ { a => a + "^ " }  | DOUBLE ^^ {a => a + "^ "} //TODO  naghese in gooya dato ina

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
        if (!parts(0).contains("(_t)")){
          parts(0) = parts(0) + "(_t)"
        }
        var theFunction: String = "double " + theId + "(int t = 0){\n return -" + parts(0) + ";\n}\n"
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
        if(!func_parts(0).contains("(_t)")){
          func_parts(0) = func_parts(0) + "(_t)"
        }
        var appendix = coef_parts(1) + "\n" + func_parts(1)
        var theId = "_" + functionCounter
        var theFunction: String = "double " + theId + "(int _t = 0){\n return ( " + coef_parts(0) + ") *" + func_parts(0) + ";\n}\n"
        functionCounter += 1
        appendix = appendix + "\n" + theFunction
        theId + "(_t)" + '^' + appendix
      }
    } |("mkdate" ~ "("~ARG~","~ARG~")")  ^^ {
      case "mkdate" ~ "(" ~ day ~ "," ~ hour ~ ")" => {
        var temp1  = day.split('^')
        var temp2  = hour.split('^')
        var result  ="((" + temp1(0) + " -1 )" +" * 24 + " + temp2(0) + ")" + " ^ "
        result
      }
    } | ("truncate"  ~ "(" ~ ARG  ~ "," ~ ARG ~ ")") ^^ {
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
    } | ("and" ~ "(" ~ ARG ~ "," ~ ARG ~")") ^^ {
        case "and" ~ "(" ~ arg1 ~ "," ~ arg2 ~ ")" => {
          println("in and")
        var theId = "_" + functionCounter
        var parts1 = arg1.split('^')
        var parts2 = arg2.split('^')
        if (!parts1(0).contains("(_t)")){
          parts1(0) = parts1(0) + "(_t)"
        }
        if (!parts2(0).contains("(_t)")){
          parts2(0) = parts2(0) + "(_t)"
        }
//          try {
//            return c1();
//          }
//          catch (Expired e){
//            return ctruncate(t);
//          }
        var theFunction: String = "double " + theId + "(int _t = 0){\n int payoff = 0; try{payoff += " + parts1(0) +  ";}\n  catch (Expired e){} \n "   + "try{payoff += " + parts2(0) +  ";}\n  catch (Expired e){} \n return payoff; }"
        functionCounter += 1
        var appendix: String = parts1(1) +"\n" + parts2(1) + "\n" +  theFunction
        theId + "(_t)" + '^' + appendix
        }
    }| ("then" ~ "(" ~ ARG ~ "," ~ ARG ~")") ^^ {
      case "then" ~ "(" ~ arg1 ~ "," ~ arg2 ~ ")" => {
        var theId = "_" + functionCounter
        var parts1 = arg1.split('^')
        var parts2 = arg2.split('^')
        if (!parts1(0).contains("(_t)")){
          parts1(0) = parts1(0) + "(_t)"
        }
        if (!parts2(0).contains("(_t)")){
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
        var theFunction: String = "double " + theId + "(int _t = 0){\n try{ return " + parts1(0) +  ";}\n  catch (Expired e){} \n "   + "try{return " + parts2(0) +  ";}\n  catch (Expired e){} \n return 0; }"
        functionCounter += 1
        var appendix: String = parts1(1) +"\n" + parts2(1) + "\n" +  theFunction
        theId + "(_t)" + '^' + appendix
      }
    }|(NAME ~ "(" ~ ARGS ~ ")") ^^ {
      case name ~ "(" ~ args ~  ")" => {
        var arg_parts = args.split('^')
        //        if (!arg_parts(0).contains("(_t)")) {
        //          arg_parts(0) = arg_parts(0) + "(_t)"
        //        } todo find what this means
        name + "(" + arg_parts(0) + ")" +   "^ " + arg_parts(1)
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

  def PROGRAM = rep(ASSIGN| FUNCCALL | DEF) ~ "END" ~ INT ~ INT ~ rep(CONTRACTNAME) ^^ {
    case inside ~ "END" ~ lines ~ time ~ contracts => {
      var answer = ""
      var flag = false
      for (i <- 0 until inside.length) {
        if(flag){
          flag = false
        }
        else if (inside(i).charAt(0) == '^') {
          var declare_statment = inside(i)
          var assignment_statment = inside(i+1)
          var declare = declare_statment.substring(1, declare_statment.length-1)
          var index = assignment_statment.indexOf('$')
          var preq = assignment_statment.substring(0, index)
          var assign = assignment_statment.substring(index + 1, assignment_statment.length)
          var rightside = assign.split("=")(1)
          if(declare.charAt(0) == ('?')){
            declare = declare.substring(1,declare.length)
            var declare_parts = declare.split(',')
            var final_result = ""
            for(i <- 1 to declare_parts.length){
              final_result =  final_result +declare_parts(i-1) + " arg" + i.toString
              if (i != declare_parts.length){
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
        if(! temp.contains(contracts(i)))
        {
          answer = answer + " getMax(" + contracts(i) + ",start)+"
          temp += contracts(i)
        }
      }
      answer = answer + "0" + " <<endl; \n return 0; \n} "
      answer
    }
  }


  // todo: iterator for generating var name

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

  //todo dorost kardan type contract baraye function
  //todo return type o syntax e timefunc kollan dorost kardanesh
  // todo porsidane vaze timefunc


  def main(args: Array[String]) {
//    var lang = "a::Contract \na = scale(20, one()) \nb:: Contract \nb = truncate(10,a) t::Date t = mkdate(2,0) \n c:: Contract \nc = truncate(t,a) \n d:: Contract \nd = truncate(t+t,one())  \nEND \n4 25 \na \nb \nc \nd"
//    var lang = "a::Contract \na = scale(20, one()) \nb:: Contract \nb = truncate(10,a) t::Date t = mkdate(2,0) \n c:: Contract \nc = truncate(t,a) \n d:: Contract \nd = truncate(t+t,one())\n costume::(Double,Double)->Double \n costume = arg1/arg2  \n newfunc :: (Double) -> Double \n newfunc = 1*costume(2.1,arg1) \nEND \n4 25 \na \nb \nc \nd"
//    " costume::(Double,Double)->Double \n costume = arg1/arg2  \n newfunc :: (Double) -> Double \n newfunc = 4+4 "
    var lang = "c1::Contract \n c1 = and(one(),one()) \n c2::Contract \n c2 = then(one(),one()) \nEND \n4 25 c1 c2"
//    println(lang)
//    var lang = get_input()
    println(lang)
    val body = parseAll(PROGRAM, lang).get //.asInstanceOf[List[String]]
    val writer = new PrintWriter(new File("code.cpp"))
    writer.write(body)
    writer.close()
//    println(body)
    //    val temp = body.foldLeft()((a,b) => a+"\n"+b)
  }


}



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