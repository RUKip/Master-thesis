package com.example

case class Solution(id: String, parent: TreeNode, color_mapping: Map[Int, Variable], score: Int) {

  def aggregateSolution(added_color_mapping: Map[Int, String], added_score: Int): Solution = {
    if (added_color_mapping.isEmpty) {
      this.copy(color_mapping = Map(), score = 0)
    } else {
      val merged_mapping = added_color_mapping.map {
        case (key, value) =>
          if (value == "Blank") {
            (key -> color_mapping(key))
          } else {
            (key -> Variable(key, List(), value))
          }
      }
      this.copy(color_mapping = merged_mapping, score = score + added_score)
    }
  }

  /** Only the bare minimum for color mapping, doesn't pass Variable */
  def bareColorMapping(): Map[Int, String] = {
    color_mapping.map { case (key, value) =>
      (key -> value.color)
    }
  }
}