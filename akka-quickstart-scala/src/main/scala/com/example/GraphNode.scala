package com.example

import java.util
import scala.jdk.CollectionConverters._

//For the graph coloring problem
case class GraphNode(id: Int, connected_ids: List[Int], color: String) {

  def connectedAsJava(): util.List[Integer] =
  {
    connected_ids.map(i => i:java.lang.Integer ).asJava
  }
}