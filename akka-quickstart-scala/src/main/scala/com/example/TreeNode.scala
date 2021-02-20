package com.example

//For the graph coloring problem
case class TreeNode(id: Int, parent: TreeNode, children_ids: Seq[Int], color: String)