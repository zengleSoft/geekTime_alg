package com.geekTime.linked

/**
  * @author zengle
  * 2019/6/16 10:28 PM
  * 判断是否是回文串
  */
object PhraseString {

  def main(args: Array[String]): Unit = {

    val node1 = Option(Node(1,None))
    val node2 = Option(Node(2,None))
    val node3 = Option(Node(3,None))
    val node4 = Option(Node(4,None))
    val node5 = Option(Node(3,None))
    val node6 = Option(Node(2,None))
    val node7 = Option(Node(3,None))

    node1.get.next = node2
    node2.get.next = node3
    node3.get.next = node4
    node4.get.next = node5
    node5.get.next = node6
    node6.get.next = node7

    println(checkPhrase(node1))

  }

  // scala case class可以直接使用==比较，不比较地址
  def checkPhrase(node:Option[Node]): Boolean ={
    if (node.isEmpty || node.get.next.isEmpty){
      return true
    }
    // 定义快慢指针
    var slow,fast = node
    // fast总是在奇数位置，从1到3到5到7 slow指向中点或者后半段第一个结点
    while (fast.isDefined && fast.get.next.isDefined){
      slow = slow.get.next
      fast = fast.get.next.get.next
    }
    var pre:Option[Node] = None
    var current:Option[Node] = node
    while (current != slow){
      var tmp = current.get.next
      current.get.next = pre
      pre = current
      current = tmp
    }
    // fast为空，则共有偶数个结点，slow位于后半段的第一个
    if (fast.isEmpty){
      pre == slow
    }else{
      pre == slow.get.next
    }
  }

}
