package com.geekTime.linked

/**
  * @author zengle
  * 2019/6/20 10:57 PM
  * 两个有序链表的合并
  */
object MergeSortLinked {

  def main(args: Array[String]): Unit = {
    var node11 = Option(Node(1,None))
    var node12 = Option(Node(5,None))
    var node13 = Option(Node(8,None))
    var node14 = Option(Node(14,None))
    node11.get.next = node12
    node12.get.next = node13
    node13.get.next = node14

    var node21 = Option(Node(0,None))
    var node22 = Option(Node(9,None))
    var node23 = Option(Node(13,None))
    node21.get.next = node22
    node22.get.next = node23

    print(merge(node11,node21))


  }


  def merge(node1: Option[Node],node2:Option[Node]): Option[Node] ={
    if (node1.isEmpty){
      node2
    }
    if (node2.isEmpty){
      node1
    }
    // small永远指向small和big之间较小的，然后循环比较small的下一个结点和big结点
    var big = node1
    var small = node2
    if (big.get.data < small.get.data){
      small = node1
      big = node2
    }
    // 记录较小的头结点，返回结果
    val head = small
    // 只要small的下一个结点和big不为空，就可以比较
    while (small.get.next.isDefined && big.isDefined){
      if (small.get.next.get.data < big.get.data){
        small = small.get.next
      }else{
        var smallNext = small.get.next
        var bigNext = big.get.next
        small.get.next = big
        big.get.next = smallNext
        // 调整之后 big、smallNext、bigNext中big最小，bigNext最大
        small = big
        big = bigNext
      }
    }
    // 跳出循环有两种情况，1种是small指向了最后一个结点，此时把big接到small的后面就行了
    // 还有一种是big指向null了，此时不用变化，head直接返回就行了
    if (big.isDefined){
      small.get.next = big
    }
    head
  }

}
