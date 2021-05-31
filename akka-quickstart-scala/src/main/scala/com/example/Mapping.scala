package com.example

case class Mapping(tree_node: Int, intersection : Map[Int, List[Int]]) {

  //Returns subset of full solution mapping (so to only send the values that are intersecting)
  def getSpecificMapping(solution: Solution, child_tree_node_id: Int): Map[Int, String] = {
    if (solution.parent.id != tree_node) {
      throw new Exception("Solution belongs to different tree node")
    }
    val intersecting_variables: List[Int] = intersection(child_tree_node_id)
    var map: Map[Int, String] = Map()
    intersecting_variables.foreach(variable_id =>
      map += (variable_id -> solution.bareColorMapping()(variable_id))
    )
    map
  }
}
