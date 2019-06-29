package com.geekTime.stack

import scala.collection.mutable


/**
  * @author zengle
  * @DATE 2019/6/29 9:30 PM
  * 20,155,232,844,224,682,496
  */
object BracketMatching {

  def main(args: Array[String]): Unit = {

    println(isValid("[{()}([])]"))
    println(isValid("{[}()]"))
  }

  def isValid(s:String): Boolean ={
    val bracket = Map('('->')','['->']','{'->'}')
    val stack = mutable.Stack[Char]()
    for (c <- s){
      if (bracket.contains(c)){
        stack.push(c)
      }else{
        if(stack.isEmpty || c != bracket(stack.pop())){
          return false
        }
      }
    }
    if (stack.nonEmpty){
      return false
    }
    true
  }

}
