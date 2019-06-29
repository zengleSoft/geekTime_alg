package com.geekTime.stack

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
/**
  * @author zengle
  * @DATE 2019/6/24 11:21 PM
  * 四则运算
  */
object Arithmetic {

  def main(args: Array[String]): Unit = {

    val express1 = "10*(1+2*3)-59"
    val express2 = "1+3*12/4-2+100/20"
    val express3 = "(3+2)*10"
    val express4 = "(3+2*5)*10"
    val express5 = "1+20/(3+2*5)*10"
    val express6 = "(2+3)*(3+4*5)"
    val express7 = "2+3*(3+4*5)"
    val express8 = "(1+20)/(3+2*5)*10"

    // caculate方法不带小括号
    println(caculate(express2))
    // 双栈法
    println(caculate2(express2))
    // 逆波兰表达式法
    println(caculate3(express2))





  }

  // 逆波兰表达式
  // 中缀表达式转后缀表达式 借助一个栈  如果有数字直接输出，如果是运算符就和栈顶运算符比较优先级
  // 如果栈顶元素优先级低，直接将当前元素入栈 如果栈顶元素优先级高或相等则输出栈顶元素，继续比较
  def caculate3(express:String): Long ={
    val addInt = '+'.toInt
    val minusInt = '-'.toInt
    val multiInt = '*'.toInt
    val divideInt = '/'.toInt
    val leftInt = '('.toInt
    val rightInt = ')'.toInt
    // 表达式字符串转成数组 方便遍历
    val arrayTuple = translate_list(express)
    // 中缀转后缀表达式
    val suffixExpress = mid2suffix(arrayTuple)
    // 计算后缀表达式
    suffixCaculate(suffixExpress)
  }

  //
  def suffixCaculate(suffixExpress:ArrayBuffer[Tuple2[Int,Int]]): Long ={
    val addInt = '+'.toInt
    val minusInt = '-'.toInt
    val multiInt = '*'.toInt
    val divideInt = '/'.toInt
    val numStack = new LinkedStack
    for (t <- suffixExpress){
      // 如果是数字
      if (t._2==1){
        numStack.push(t._1)
      }else {
        val first = numStack.pop().get.data
        val second = numStack.pop().get.data
        val res = t._1 match {
          case `addInt` =>
            second + first
          case `minusInt` =>
            second - first
          case `multiInt` =>
            second * first
          case `divideInt` =>
            second/first
        }
        numStack.push(res)
      }
    }
    numStack.peek()
  }


  // 中缀表达式转成后缀表达式
  def mid2suffix(arrayTuple:ArrayBuffer[Tuple2[Int,Int]]): ArrayBuffer[Tuple2[Int,Int]] ={
    val suffixExpress = new ArrayBuffer[Tuple2[Int,Int]](arrayTuple.length)
    val leftInt = '('.toInt
    val rightInt = ')'.toInt
    // 右括号设置最小优先级 左括号最大优先级
    val symbolMap = Map('+'.toInt->1,'-'.toInt->1,'*'.toInt->2,'/'.toInt->2,rightInt -> 0,leftInt->3)
    val operator = new LinkedStack
    for (t <- arrayTuple){
      // 数字直接输出
      if (t._2 == 1){
        suffixExpress.append((t._1,1))
      }else{
        // 右括号输入栈中运算符 直到碰到左括号
        if (t._1 == rightInt){
          var preSymbol = operator.pop().get.data
          while (preSymbol != leftInt){
            suffixExpress.append((preSymbol,0))
            preSymbol = operator.pop().get.data
          }
        } else{
          var preInt = operator.peek()
          while (preInt != -1){
            // 栈顶运算符优先级高  输出栈顶运算符
            if ( preInt != leftInt && symbolMap(preInt)>=symbolMap(t._1)){
              suffixExpress.append((operator.pop().get.data,0))
              preInt = operator.peek()
            }else{
              preInt = -1
            }
          }
          operator.push(t._1)
        }
      }
    }
    while (operator.peek() != -1){
      suffixExpress.append((operator.pop().get.data,0))
    }
    suffixExpress
  }


  // 将运算式转成顺序列表 用二元组存储 第一个存数字或运算符 第二个存是否是数字 1：数字 0：运算符
  def translate_list(express:String):ArrayBuffer[Tuple2[Int,Int]] ={
    val ab = new ArrayBuffer[Tuple2[Int,Int]]
    var preNum = 0
    for (s <- express){
      var i = s.toInt - 48
      if (i>=0 && i<=9){
        preNum = preNum*10 + i
      }else{
        if (preNum != 0){
          ab.append((preNum,1))
        }
        preNum = 0
        ab.append((s.toInt,0))
      }
    }
    if (preNum !=0){
      ab.append((preNum,1))
    }
    ab
  }


  // 使用两个栈求四则运算 包含小括号
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


  // 使用两个栈求四则运算  只有+-*/ 不包含括号
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
