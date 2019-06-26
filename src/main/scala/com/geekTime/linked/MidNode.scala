package com.geekTime.linked

/**
  * @author zengle
  * @DATE 2019/6/22 1:47 PM
  * 求链表的中间结点
  */
object MidNode {

  def main(args: Array[String]): Unit = {
    val node1 = Option(Node(1,None))
    val node2 = Option(Node(2,None))
    val node3 = Option(Node(3,None))
    val node4 = Option(Node(4,None))
    val node5 = Option(Node(5,None))
    val node6 = Option(Node(6,None))
    val node7 = Option(Node(7,None))
    val node8 = Option(Node(8,None))
    val node9 = Option(Node(9,None))
    val node10 = Option(Node(10,None))


    node1.get.next = node2
    node2.get.next = node3
    node3.get.next = node4
    node4.get.next = node5
    node5.get.next = node6
    node6.get.next = node7
    node7.get.next = node8
    node8.get.next = node9
    node9.get.next = node10

    println(findMidNode(node1))
  }

  // 返回中间结点，如果是偶数个结点，返回中间靠右的结点
  def findMidNode(node:Option[Node]): Option[Node] ={
    var slow,fast = node
    while (fast.isDefined && fast.get.next.isDefined){
      slow = slow.get.next
      fast = fast.get.next.get.next
    }
    slow
  }

}
