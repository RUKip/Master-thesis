package com.example

//For the decomposition of the graph
case class TreeNode(
                     id: Int,
                     parent: Int,
                     tree_children: List[Int],
                     graph_variables: List[Int],
                     full_graph_mapping: Map[Int, Variable],
                     child_connected: Map[Int, List[Int]]
                   ) {

  def getFullGraphVariables: Seq[Variable] = {
    graph_variables.map(variable => {
      full_graph_mapping.get(variable) match {
        case Some(value) => value
        case None => throw new Exception("Could not match members to graph nodes")
      }
    })
  }

  def getBareFullGraphVariables: Map[Int, String] = {
    getFullGraphVariables.map(variable => variable.id -> variable.color).toMap
  }

  //TODO: maybe dont need this
  //Here making assumption of connectivity, we can assume that all graph_variables are connected else the decomposition is wrong
  def getNewSubGraph: Seq[Variable] = {
    graph_variables.map(variable => {
      val color = full_graph_mapping.getOrElse(variable, Variable(variable, graph_variables, "Blank")).color
      Variable(variable, graph_variables, color)
    })
  }

  //Update the color of all nodes given a color mapping, update happens by updating the full_graph_mapping
  def updateNodes(color_mapping: Map[Int, String]): TreeNode ={
    val graph_mapping: Map[Int, Variable] = this.full_graph_mapping map {
      case (graph_node_id: Int, graphNode: Variable) => (
        graph_node_id,
        graphNode.copy(color=color_mapping.getOrElse(graph_node_id, graphNode.color))
      )}
    this.copy(full_graph_mapping = graph_mapping)
  }
}
