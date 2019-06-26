package com.geekTime.linked

/**
  * @author zengle
  * 2019/6/18 10:08 PM
  * 单链表环检测
  * 使用快慢结点运动，如果有环，那么一定会相遇
  * 假设起点到环入口长度为l，环入口距离相遇点为a，环长度为n，相遇时慢指针跑了p圈，快指针跑了q圈
  * 那么慢指针总共运动 l+pn+a 快指针总共运动 l+qn+a
  * 所以 l+qn+a = 2(l+pn+a)  => (q-2p)n = l+a
  * 这个等式很容易满足，所以一定会相遇
  *
  * 求环的入口：快指针保留在相遇点，慢指针移动到起点，两个指针同样的速度开始运行，下一次相遇点一定是在环的入口点
  * 慢指针从起点运动到入口，走了l，距离相遇点还有a的距离，此时快指针也走了l，
  * 因为 l+a是环长度n的整数倍，所以快指针此时也在距离相遇点相差a的位置，即入口点
  */
object CycleLinked {

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
//    node10.get.next = node7
//    print(ifCycle(node1))
    print(findEntrance(node1))

  }

  def ifCycle(node:Option[Node]):Boolean={
    var slow,fast = node
    while (fast.isDefined && fast.get.next.isDefined){
      slow = slow.get.next
      fast = fast.get.next.get.next
      if (slow==fast){
        return true
      }
    }
    false
  }

  // 不能返回循环的链表，会报StackOverflowError，这里返回入口链表的值，假设链表中的数据都不一样，而且都不是0
  def findEntrance(node:Option[Node]): Int ={
    var slow,fast = node
    // cycle记录是否有环 flag记录是否停止循环
    var cycle,flag = false
    while (!flag){
      if (fast.isDefined && fast.get.next.isDefined){
        slow = slow.get.next
        fast = fast.get.next.get.next
        if (slow==fast){
          cycle = true
          flag = true
        }
      }else{
        flag = true
      }
    }
    if (cycle){
      slow = node
      while (slow!=fast){
        slow = slow.get.next
        fast = fast.get.next
      }
      slow.get.data
    }else{
      // 没有环返回0
      0
    }
  }

}
