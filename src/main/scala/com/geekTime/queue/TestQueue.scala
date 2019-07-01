package com.geekTime.queue

import com.geekTime.linked.Node

import scala.collection.mutable

/**
  * @author zengle
  * @DATE 2019/6/30 11:57 PM
  */
object TestQueue {

  def main(args: Array[String]): Unit = {

    val aq = ArrayQueue(4)
    println(aq.enqueue("aaa"))
    println(aq.enqueue("bbb"))
    println(aq.enqueue("ccc"))
    println(aq.enqueue("ddd"))
    println(aq.dequeue())
    println(aq.dequeue())
//    println(aq.dequeue())
//    println(aq.dequeue())
    println(aq.enqueue("xxx"))
    println(aq.enqueue("www"))
    println(aq.enqueue("eee"))
    println(aq.enqueue("qqq"))
    println(aq.dequeue())


    for (item <- aq.items){
      println(item)
    }

  }

}
