package com.example

//For the graph coloring problem
class TreeNode(val id: Int, val parent: TreeNode) {
  var children: Seq[TreeNode] = Seq()
  var color: String = "Blank"

  if (parent != null) {
    parent.addChild(this)
  }

  def addChild(node: TreeNode): Unit = {
    this.children = this.children :+ node
  }

  def setColor(color: String): Unit = {
    this.color = color
  }
}
