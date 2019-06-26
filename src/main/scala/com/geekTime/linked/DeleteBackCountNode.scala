package com.geekTime.linked

/**
  * @author zengle
  * 2019/6/20 11:56 PM
  * 删除倒数第n个结点
  */
object DeleteBackCountNode {

  def main(args: Array[String]): Unit = {
    val node1 = Option(Node(1,None))
    val node2 = Option(Node(2,None))
    val node3 = Option(Node(3,None))
    val node4 = Option(Node(4,None))
    val node5 = Option(Node(5,None))
    val node6 = Option(Node(6,None))

    node1.get.next = node2
    node2.get.next = node3
    node3.get.next = node4
    node4.get.next = node5
    node5.get.next = node6

    println(node1)
    println(deleteNode2(node1,2))
  }

  // 快慢指针，快指针先走n步，然后快慢指针同时走直到快指针到头
  def deleteNode2(node:Option[Node],n:Int): Option[Node] = {
    require(n>0,"n必须大于0")
    var slow,fast = node
    var index = 0
    while (fast.isDefined && index<n){
      index += 1
      fast = fast.get.next
    }
    require(index==n,"至少包含n个结点")
    // 删除头结点
    if (fast.isEmpty){
      return node.get.next
    }
    while (fast.isDefined && fast.get.next.isDefined){
      slow = slow.get.next
      fast = fast.get.next
    }
    slow.get.next = slow.get.next.get.next
    node
  }


  // 先反转链表在删除第k个结点，然后在反转链表
  def deleteNode(node:Option[Node],n:Int): Option[Node] ={
    var head = ReverseLinked.reverse(node)
    var cur = head
    // 删除头结点
    if (n==1){
      head = head.get.next
      cur.get.next = None
      return ReverseLinked.reverse(head)
    }
    // cur指向删除结点的上一个结点
    for (i <- 1 until n-1){
      cur = cur.get.next
    }
    val tmp = cur.get.next
    cur.get.next = tmp.get.next
    tmp.get.next = None

    ReverseLinked.reverse(head)
  }


}
