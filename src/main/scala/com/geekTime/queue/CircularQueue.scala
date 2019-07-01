package com.geekTime.queue

/**
  * @author zengle
  * @DATE 2019/7/1 10:18 PM
  * 循环队列
  */
case class CircularQueue(n:Int) {

  // 存储数组
  val items:Array[String] = new Array[String](n)
  // 队首
  var head:Int = 0
  // 队尾
  var tail:Int = 0

  def enqueue(item:String): Boolean ={
    if ((tail+1)%n==head){
      return false
    }
    items(tail) = item
    tail = (tail + 1) % n
    true
  }


  def dequeue(): String ={
    if (head == tail){
      return null
    }
    val item = items(head)
    head = (head + 1) %n
    item
  }


}
