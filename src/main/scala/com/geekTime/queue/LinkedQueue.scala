package com.geekTime.queue

import com.geekTime.linked.Node

/**
  * @author zengle
  * @DATE 2019/6/30 4:16 PM
  * 链表实现队列
  */
class LinkedQueue {

  var head:Option[Node] = None
  var tail:Option[Node] = None

  def enqueue(item:Int): Boolean ={
    val node = Option(Node(item,None))
    if (tail.isEmpty){
      tail = node
      head = node
    }else{
      tail.get.next = node
    }
    true
  }

  def dequeue(): Int ={
    // 队列为空 返回-1
    if (head.isEmpty){
      return -1
    }
    val item = head.get.data
    head = head.get.next
    // 如果出队列后，队列为空了，重置tail
    if (head.isEmpty){
      tail = head
    }
    item
  }


}
