package com.geekTime.stack

/**
  * @author zengle
  * @DATE 2019/6/23 11:45 PM
  * 数组实现栈
  * n:栈的大小 items:栈中的元素 count:栈中元素的个数
  */
case class ArrayStack(n:Int,items:Array[String]) {
  // 栈中元素个数
  var count=0
  // 入栈
  def push(item:String): Boolean ={
    // 栈满了，返回false
    if (count == n){
      return false
    }
    items(count) = item
    count += 1
    true
  }
  // 出栈
  def pop(): String ={
    // 栈为空 返回null
    if (count == 0){
      return null
    }
    count -= 1
    items(count)
  }
  // 清空栈
  def clear(): Unit ={
    count = 0
  }
}
