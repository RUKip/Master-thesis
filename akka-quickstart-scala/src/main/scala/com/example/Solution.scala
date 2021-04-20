package com.example

case class Solution(id: String, parent: TreeNode, color_mapping: Map[Int, String]) {

  def addSolution(solution: Solution): Unit = {
    color_mapping ++= solution.color_mapping
  }
}