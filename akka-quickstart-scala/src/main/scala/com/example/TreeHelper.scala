package com.example

import scala.collection.mutable

object TreeHelper {

  /* Graph structure like:

       root
     2      3
               4

   */
  def initATree(): (TreeNode, Map[Int, TreeNode]) = {
    var mapping: Map[Int, TreeNode] = Map()

    val root = TreeNode(1, null, List(2, 3), "Blank")
    val node2 = TreeNode(2, root, List(), "Blank")
    val node3 = TreeNode(3, root, List(4), "Blank")
    val node4 = TreeNode(4, node3, List(), "Blank")

    mapping + (1 -> root)
    mapping + (2 -> node2)
    mapping + (3 -> node3)
    mapping + (4 -> node4)

    (root, mapping)
  }
}
