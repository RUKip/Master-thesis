package com.example

import scala.collection.mutable

object TreeHelper {

  /* Graph structure like:

       root
     2      3
               4

   */
  def initATree(): TreeNode = {
    var map: Map[Int, TreeNode] = Map()

    val root = TreeNode(1, null, Seq(2, 3), "Blank")
    val node2 = TreeNode(2, root, Seq(), "Blank")
    val node3 = TreeNode(3, root, Seq(4), "Blank")
    val node4 = TreeNode(4, node3, Seq(), "Blank")

    map + (1 -> root)
    map + (2 -> node2)
    map + (3 -> node3)
    map + (4 -> node4)

    root
  }
}
