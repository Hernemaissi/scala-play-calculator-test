package models
import scala.collection.mutable.Stack

object Calculus {
    val UNSUPPORTED_CHARACTER = "err1"
    val UNMATCHED_OPENING = "err2"
    val UNMATCHED_CLOSING = "err3"
    val MALFORMED_QUERY = "err4"
    val ARITHMETIC_EXCEPTION = "err5"
    val EMPTY_QUERY = "err6"
    val ERROR_BASE = 'e'
  
  //Transform the equation string into postfix format
  def toPostfix(_query: String) : String = {
      
      if (_query.length == 0) {
          return Calculus.EMPTY_QUERY
      }
      
      val query = _query.replaceAll(" ", "")
      var postFix = StringBuilder.newBuilder
      var operators = Stack[Char]()
      var top : Char = ' '
      var current : Char = ' '
      var oper : Char = ' '
      var lastWasDigit : Boolean = false
      for (i <- query.indices) {
          current = query(i)
          if (Calculus.isOperator(current)) {
              if (lastWasDigit) {
                  postFix.append(" ")
                  lastWasDigit = false
              }
              
              if (current == '(') {
                  operators.push(current)
              } else if (current == ')') {
                  if (operators.isEmpty) {
                      //unmatched ')'
                      return Calculus.UNMATCHED_CLOSING
                  }
                  oper = operators.pop
                  while (oper != '(') {
                      postFix.append(oper)
                      postFix.append(" ")
                      
                      if (operators.isEmpty) {
                          //unmatched ')'
                          return Calculus.UNMATCHED_CLOSING
                      }
                      oper = operators.pop
                  }
              //Check for unary -
              } else if (Calculus.isUnaryOperator(i, query)) {
                  postFix.append('!')
              } else {
                  
                  if (operators.isEmpty) {
                      operators.push(current)
                  } else {
                      top = operators.top
                      if (Calculus.operatorValue(current) > Calculus.operatorValue(top)) {
                          operators.push(current)
                      } else {
                          while (operators.isEmpty == false && Calculus.operatorValue(current) <= Calculus.operatorValue(operators.top)) {
                              oper = operators.pop
                              postFix.append(oper)
                              postFix.append(" ")
                          }
                      operators.push(current)
                      }
                  }
                
              }
              
              
          } else if (current.isDigit) {
              postFix.append(current)
              lastWasDigit = true
          } else {
              //unsupported character
              return Calculus.UNSUPPORTED_CHARACTER
          }
          
      }
      if (lastWasDigit) {
          postFix.append(" ")
          lastWasDigit = false
      }
      //Pop remaining operators
      while (operators.isEmpty == false) {
          oper = operators.pop
          if (oper == '(') {
              //unmatched '('
              return Calculus.UNMATCHED_OPENING
          }
          postFix.append(oper)
          postFix.append(" ")
      }
      return postFix.toString().trim
      
  }
  //Evaluate the result of an equation that is given in postfix format
  def evaluate(postfix : String) : String = {
      //If the postfix method returned an error, let's just pass it on
      if (Calculus.isError(postfix)) {
          return postfix
      }
      
      var operands = Stack[Double]()
      for (current <- postfix.split(" ")) {
          var identifier = current(0)
          if (identifier.isDigit) {
              //If it's a operand, push it to the stack
              operands.push(current.toDouble)
          } else if (identifier == '!') {
              //If it's a negative operand, turn it negative and push it to the stack
              operands.push(current.substring(1).toDouble * -1)
          } else if (Calculus.isOperator(identifier)) {
              //If it's operator, pop two operands and perform the operation
              if (operands.size < 2) {
                  //malformed query
                  return Calculus.MALFORMED_QUERY
              }
              var secondOperand = operands.pop
              var firstOperand = operands.pop
              var result : Double = 0
              try {
                  result = Calculus.calculate(identifier, firstOperand, secondOperand)
              } catch {
                  //Arimethic exception, probably divided by zero
                  case e:ArithmeticException => return Calculus.ARITHMETIC_EXCEPTION
              }
              operands.push(result)
          }
          
      }
      if (operands.size != 1) {
          //malformed query
          return Calculus.MALFORMED_QUERY
      }
      
      return operands.pop.toString
  }
  
  //Check if a given char is a supported operator
  def isOperator(char: Char) : Boolean = {
      char == '(' || char == ')' || char == '+' || char == '-' || char == '*' || char == '/'
  }
  
  //Returns integer value showing operator precedence
  def operatorValue(char: Char) : Int = {
      char match {
          case '(' => 0
          case ')' => 0
          case '+' => 1
          case '-' => 1
          case '*' => 2
          case '/' => 2
          case _ => -1
      }
  }
  
  def isUnaryOperator(i : Int, query: String) : Boolean = {
      query(i) == '-' && (query(i+1).isDigit && (i == 0 || (Calculus.isOperator(query(i -1)) && query(i-1) != ')' ) ))
  }
  //Given two operands and a char that is a operator, performs the operation
  def calculate(operator : Char, firstOperand : Double, secondOperand : Double) : Double = {
      try {
          operator match {
              case '+' => firstOperand + secondOperand
              case '-' => firstOperand - secondOperand
              case '*' => firstOperand * secondOperand
              case '/' => firstOperand / secondOperand
          }
      } catch {
          case e:ArithmeticException => throw new ArithmeticException("Impossible calculation")
      }
  }
  
  def getErrorMsg(code : String) : String = {
      code match {
          case Calculus.UNSUPPORTED_CHARACTER => "Unsupported character in the query. Supported characters are digits and operators: +-*/()"
          case Calculus.UNMATCHED_OPENING => "Unmatched opening bracket '(' encountered."
          case Calculus.UNMATCHED_CLOSING => "Unmatched closing bracket ')' encountered"
          case Calculus.MALFORMED_QUERY => "Malformed query. Please recheck the equation"
          case Calculus.ARITHMETIC_EXCEPTION => "Arithmetic exception encountered. Did you try dividing by zero?"
          case Calculus.EMPTY_QUERY => "No query received. Usage: /calculus?query=[base64encodedquery]"
          case _ => "Unknown error"
      }
  }
  
  def isError(s : String) : Boolean = {
      s(0) == Calculus.ERROR_BASE
  }
  
}