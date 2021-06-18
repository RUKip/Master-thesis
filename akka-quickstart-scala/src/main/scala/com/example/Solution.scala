package com.example

case class Solution(id: String, parent: TreeNode, color_mapping: Map[Int, Variable], score: Int) {

  def aggregateSolution(added_color_mappings: List[(Map[Int, String], Int)]): Solution = {
    if ( !added_color_mappings.exists { case (mapping: Map[Int, String], _) => mapping.isEmpty } ) {
      val added_score: Int = added_color_mappings.map(tuple => tuple._2).sum
      val new_mapping = aggregateAll(added_color_mappings.map(value => value._1), color_mapping)
      this.copy(color_mapping = new_mapping, score = score + added_score)
    } else {
      this.copy(color_mapping = Map(), score = 0)
    }
  }

  def aggregateAll(added_color_mappings: List[Map[Int, String]], final_solutions: Map[Int, Variable]): Map[Int, Variable] = {
    if (added_color_mappings.isEmpty) {
      final_solutions
    } else {
      val aggregated_solution = aggregate(added_color_mappings.head, final_solutions)
      aggregateAll(added_color_mappings.tail, aggregated_solution)
    }
  }

  def aggregate(solutions: Map[Int, String], final_solutions: Map[Int, Variable]): Map[Int, Variable] = {
    if (solutions.isEmpty) {
      final_solutions
    } else {
      val solution = solutions.head
      val key = solution._1
      val color = solution._2
      if (color != "Blank") {
        aggregate(solutions.tail, final_solutions + (key -> Variable(key, List(), color)))
      } else {
        aggregate(solutions.tail, final_solutions)
      }
    }
  }

  /** Only the bare minimum for color mapping, doesn't pass Variable (used in messages) */
  def bareColorMapping(): Map[Int, String] = {
    color_mapping.map { case (key: Int, value: Variable) =>
      (key -> value.color)
    }
  }
}