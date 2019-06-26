package com.geekTime.linked

/**
  * @author zengle
  * 2019/6/17 11:52 PM
  * 链表反转
  */
object ReverseLinked {

  def main(args: Array[String]): Unit = {
    val node1 = Option(Node(1,None))
    val node2 = Option(Node(2,None))
    val node3 = Option(Node(3,None))
    val node4 = Option(Node(4,None))
    val node5 = Option(Node(5,None))
    val node6 = Option(Node(6,None))

//    node1.get.next = node2
    node2.get.next = node3
    node3.get.next = node4
    node4.get.next = node5
    node5.get.next = node6

    println(node1)
    println(reverse(node1))
  }


  def reverse(node: Option[Node]): Option[Node] = {
    if (node.isEmpty){
      return node
    }
    var pre:Option[Node] = None
    var current:Option[Node] = node
    while (current.get.next.nonEmpty){
      var tmp:Option[Node] = current.get.next
      current.get.next = pre
      pre = current
      current = tmp
    }
    current.get.next = pre
    current
  }

}
