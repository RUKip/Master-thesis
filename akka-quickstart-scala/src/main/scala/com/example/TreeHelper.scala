package com.example

object TreeHelper {


  /* Graph structure like:

       root
     2      3
               4

   */
  def initATree(): Unit = {
    val root = new TreeNode(1, null)
    val node2 = new TreeNode(2, root)
    val node3 = new TreeNode(3, root)
    val node4 = new TreeNode(4, node3)

  }
}
