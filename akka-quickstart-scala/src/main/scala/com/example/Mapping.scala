package com.example

case class Mapping(solution: Solution, intersection : Map[Int, Map[Int, List[Variable]]]) {

  //Returns subset of full solution mapping (so to only send the values that are intersecting)
  def getSpecificMapping(solution: Solution, child_tree_node_id: Int): Map[Int, String] = {
    val intersecting_variables: List[Variable] = intersection(solution.parent.id)(child_tree_node_id)
    var map: Map[Int, String] = Map()
    intersecting_variables.foreach(variable =>
      map += (variable.id -> solution.color_mapping(variable.id))
    )
    map
  }
}
