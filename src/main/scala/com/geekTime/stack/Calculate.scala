package com.geekTime.stack

import scala.util.{Success, Try}

/**
  * @author zengle
  * @DATE 2019/6/24 11:21 PM
  * 四则运算
  */
object Calculate {

  def main(args: Array[String]): Unit = {

    val express1 = "10*(1+2*3)-59"
    val express2 = "1+3*12/4-2+100/20"
    val express3 = "(3+2)*10"
    val express4 = "(3+2*5)*10"
    val express5 = "1+20/(3+2*5)*10"

    println(caculate2(express5))
  }

  // 包含小括号
  def caculate2(express:String): Long ={
    val addInt = '+'.toInt
    val minusInt = '-'.toInt
    val multiInt = '*'.toInt
    val divideInt = '/'.toInt
    val leftInt = '('.toInt
    val rightInt = ')'.toInt
    // 右括号设置最小优先级 左括号最大优先级
    val symbolMap = Map(addInt->1,minusInt->1,multiInt->2,divideInt->2,rightInt -> 0,leftInt->3)
    val numStack = new LinkedStack
    val symbolStack = new LinkedStack
    // 前一个是否是数字
    var preIfNum = true
    // 数字栈栈顶放一个0 表达式第一个一定是数字，这样不用对头做特殊处理
    numStack.push(0)
    for (s <- express){
      var i = s.toInt - 48
      // 0-9
      if(i >= 0 && i<=9){
        // 如果前一个字符也是数字，那么将前一个*10加上当前数字然后入栈
        if (preIfNum){
          i += 10*numStack.pop().get.data
        }
        numStack.push(i)
        preIfNum=true
      }else{
        preIfNum = false
        var preSymbol = symbolStack.pop()
        while (preSymbol.isDefined){
          // 如果当前的运算符优先级小于等于上一个运算符，则计算上一个运算符 右括号优先级最小，所以到了右括号就开始计算前面所有的，知道左括号
          if (preSymbol.get.data != leftInt && symbolMap(preSymbol.get.data) >= symbolMap(s.toInt)){
            val a = numStack.pop().get.data
            val b = numStack.pop().get.data
            val res = preSymbol.get.data match {
              case `addInt` => b+a
              case `minusInt` => b-a
              case `multiInt` => b*a
              case `divideInt` => b/a
            }
            numStack.push(res)
            preSymbol = symbolStack.pop()
          }else if(preSymbol.get.data == leftInt && s.toInt==rightInt){
            // 运算符只剩左右括号，跳出循环
            preSymbol = None
          }else{
            // 放回弹出的上一个运算符，跳出循环
            symbolStack.push(preSymbol.get.data)
            preSymbol = None
          }
        }
        // 将当前运算符放入栈，如果是右括号说明括号内计算完成，所以右括号不用在入栈
        if (s.toInt != rightInt){
          symbolStack.push(s.toInt)
        }
      }
    }
    // 计算栈中剩下的数据
    var symbol = symbolStack.pop()
    while (symbol.isDefined){
      val a = numStack.pop().get.data
      val b = numStack.pop().get.data
      val res = symbol.get.data match {
        case `addInt` => b+a
        case `minusInt` => b-a
        case `multiInt` => b*a
        case `divideInt` => b/a
      }
      numStack.push(res)
      symbol = symbolStack.pop()
    }
    numStack.pop().get.data
  }


  // 只有+-*/ 不包含括号
  def caculate(express:String): Long ={
    val addInt = '+'.toInt
    val minusInt = '-'.toInt
    val multiInt = '*'.toInt
    val divideInt = '/'.toInt
    val symbolMap = Map(addInt->1,minusInt->1,multiInt->2,divideInt->2)
    val numStack = new LinkedStack
    val symbolStack = new LinkedStack
    // 前一个是否是数字
    var preIfNum = true
    // 数字栈栈顶放一个0 表达式第一个一定是数字，这样不用对头做特殊处理
    numStack.push(0)
    for (s <- express){
      var i = s.toInt - 48
      // 0-9
      if(i >= 0 && i<=9){
        // 如果前一个字符也是数字，那么将前一个*10加上当前数字然后入栈
        if (preIfNum){
          i += 10*numStack.pop().get.data
        }
        numStack.push(i)
        preIfNum=true
      }else{
        preIfNum = false
        var preSymbol = symbolStack.pop()
        while (preSymbol.isDefined){
          // 如果当前的运算符优先级小于等于上一个运算符，则计算上一个运算符
          if (symbolMap(preSymbol.get.data) >= symbolMap(s.toInt)){
            val a = numStack.pop().get.data
            val b = numStack.pop().get.data
            val res = preSymbol.get.data match {
              case `addInt` => b+a
              case `minusInt` => b-a
              case `multiInt` => b*a
              case `divideInt` => b/a
            }
            numStack.push(res)
            preSymbol = symbolStack.pop()
          }else{
            // 放回弹出的上一个运算符，跳出循环
            symbolStack.push(preSymbol.get.data)
            preSymbol = None
          }
        }
        // 将当前运算符放入栈
        symbolStack.push(s.toInt)
      }
    }
    // 计算栈中剩下的数据
    var symbol = symbolStack.pop()
    while (symbol.isDefined){
      val a = numStack.pop().get.data
      val b = numStack.pop().get.data
      val res = symbol.get.data match {
        case `addInt` => b+a
        case `minusInt` => b-a
        case `multiInt` => b*a
        case `divideInt` => b/a
      }
      numStack.push(res)
      symbol = symbolStack.pop()
    }
    numStack.pop().get.data
  }

}
