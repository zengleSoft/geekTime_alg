package com.geekTime.queue

/**
  * @author zengle
  * @DATE 2019/6/30 11:31 PM
  * 顺序队列 数组实现
  */
case class ArrayQueue (n:Int){

  // 存储数组
  val items:Array[String] = new Array[String](n)
  // 队首
  var head:Int = 0
  // 队尾
  var tail:Int = 0
  // 入队
  def enqueue(item:String): Boolean ={
    // 队尾到了数组尾部
    if(tail==n){
      // 队列满了
      if (head == 0 ){
        return false
      }
      // tail到了尾部，但是数组前面是空的  队列整体前移
      for (i <-  head until tail){
        items(i - head) = items(i)
      }
      // 前移后指针复位
      tail = n - head
      head = 0
    }
    items(tail) = item
    tail += 1
    true
  }
  // 出队
  def dequeue(): String ={
    // 队列为空 复位队列
    if (head == tail){
      return null
    }
    val item = items(head)
    head += 1
    item
  }


}
