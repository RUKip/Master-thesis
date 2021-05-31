package com.example.solver

import com.example.{TreeNode, Variable}

import scala.jdk.CollectionConverters._

object SolverScalaWrapper {

  def calcSolutions(node: TreeNode): List[Map[Int, String]] = {
    val java_mapping: Map[Integer, Variable] = node.full_graph_mapping map { case (key, value) => (key: java.lang.Integer, value) }
    val solutions: List[Map[Int, String]] = Solver.solve(node.getGraphNodes.asJava, java_mapping.asJava)
      .asScala
      .toList
      .map(internal_map => internal_map.asScala.toMap map { case (key, value) => (key.toInt, value) })
    solutions
  }
}
