package com.example

//For the decomposition of the graph
case class TreeNode(
                     id: Int,
                     parent: TreeNode,
                     tree_childeren: List[Int],
                     graph_variables: List[Int],
                     full_graph_mapping: Map[Int, GraphNode],
                     parent_connected: List[Int],
                     child_connected: List[Int]
                   ) {

  def getGraphNodes: Seq[GraphNode] = {
    graph_variables.map(variable => {
      full_graph_mapping.get(variable) match {
        case Some(value) => value
        case None => throw new Exception("Could not match members to graph nodes")
      }
    })
  }

  //TODO: maybe dont need this
  //Here making assumption of connectivity, we can assume that all graph_variables are connected else the decomposition is wrong
  def getNewSubGraph: Seq[GraphNode] = {
    graph_variables.map(variable => {
      val color = full_graph_mapping.getOrElse(variable, GraphNode(variable, graph_variables, "Blank")).color
      GraphNode(variable, graph_variables, color)
    })
  }

  //Update the color of all nodes given a color mapping, update happens by updating the full_graph_mapping
  def updateNodes(color_mapping: Map[Int, String]): TreeNode ={
    val graph_mapping: Map[Int, GraphNode] = this.full_graph_mapping map {
      case (graph_node_id: Int, graphNode: GraphNode) => (
        graph_node_id,
        graphNode.copy(color=color_mapping.getOrElse(graph_node_id, graphNode.color))
      )}
    this.copy(full_graph_mapping = graph_mapping)
  }
}