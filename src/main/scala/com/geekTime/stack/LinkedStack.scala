package com.geekTime.stack

import com.geekTime.linked.Node

/**
  * @author zengle
  * @DATE 2019/6/24 12:26 PM
  */
class LinkedStack {

  var head:Option[Node] = None
  var size:Int = 0
  // 入栈
  def push(value:Int): Boolean ={
    head = Option(Node(value,head))
    size += 1
    true
  }
  // 出栈
  def pop(): Option[Node] ={
    if (size==0){
      return None
    }
    val item = head
    head = head.get.next
    size -=1
    item
  }
  // 返回栈顶元素 不弹出
  def peek(): Int ={
    if (size==0){
      return -1
    }
    head.get.data
  }
  // 清空栈
  def clear(): Unit ={
    size = 0
    head = None
  }
}
