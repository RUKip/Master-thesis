package com.example

import scala.util.Random

object InitializationHelper {
  
  /* Graph structure like:
        1
        |
        2
       / \
      3 - 4
      |   |
      5 - 6
   */
  def initAGraph(): Map[Int, Variable] = {
    var mapping: Map[Int, Variable] = Map()

    val node1 = Variable(1, List(2), "Blank")
    val node2 = Variable(2, List(1,3,4), "Blank")
    val node3 = Variable(3, List(2,4,5), "Blank")
    val node4 = Variable(4, List(2,3,6), "Blank")
    val node5 = Variable(5, List(3,6), "Blank")
    val node6 = Variable(6, List(4,5), "Blank")

    mapping += (1 -> node1)
    mapping += (2 -> node2)
    mapping += (3 -> node3)
    mapping += (4 -> node4)
    mapping += (5 -> node5)
    mapping += (6 -> node6)

    mapping
  }

  /* TODO: what about differentiating between GraphNodes in a Tree decomposition that are (parent/child/un) connected?
      Parent nodes can only be initialized by signal (actor message) from parent node
      Child nodes when initialized need to send an actor message to their respective connected graph nodes in other Tree nodes
      Unconnected can be possibly already pre-initialized without waiting
  */


  /* Decomposition of graph:
      (2,3,4)
       /   \
   (3,4,5) (1,2)
      |
   (4,5,6)
 */
  def getHTD(graph_mapping: Map[Int, Variable]): (TreeNode, Map[Int, TreeNode]) = {
    var mapping: Map[Int, TreeNode] = Map()

    val root = TreeNode(1, 0, List(2, 3), List(2,3,4), graph_mapping, Mapping(1, Map(2 -> List(3,4), 3 -> List(2))))
    val node2 = TreeNode(2, 1, List(4), List(3,4,5), graph_mapping, Mapping(2, Map(4 -> List(4,5))))
    val node3 = TreeNode(3, 1, List(), List(1,2), graph_mapping, Mapping(3,Map()))
    val node4 = TreeNode(4, 2, List(), List(4,5,6), graph_mapping, Mapping(4,Map()))

    mapping += (1 -> root)
    mapping += (2 -> node2)
    mapping += (3 -> node3)
    mapping += (4 -> node4)

    (root, mapping)
  }

  def createTreeStructure(branching_factor: Int = 3, width: Int = 4, parent: Int, start_index: Int, depth: Int, max_depth: Int): Seq[(Int, BaseTreeNode)] = {
    var children: Seq[(Int, BaseTreeNode)] = Seq()
    if (max_depth > depth) {
      val width_node = math.max(Random.nextInt(width - 1) + 1, 2) //Min width 2, else 2 to width
      if (depth == 1) {
        //Root node case
        val id = 1
        children = children :+ (id, BaseTreeNode(id, parent, variables = Range(start_index, start_index + width_node).toList))
        children = children ++ createTreeStructure(branching_factor, width, id, start_index + width_node, depth + 1, max_depth)
      } else {
        val branches = Random.nextInt(branching_factor)
        Range(1, branches + 1).foreach { nr =>
          val overlap = math.min(Random.nextInt(width_node - 1) + 1, width_node-1) //Overlap max width - 1 and min overlap 1
          val id: Int = (depth.toString + nr.toString).toInt
          children = children :+ (id, BaseTreeNode(id, parent, variables = Range(start_index - overlap, start_index + width_node).toList))
          children = children ++ createTreeStructure(branching_factor, width, id, start_index + width_node - overlap, depth + 1, max_depth)
        }
      }
    }
    children
  }

  def createUsableTree(base: Seq[(Int, BaseTreeNode)]): Map[Int, TreeNode] = {
    val base_map: Map[Int, BaseTreeNode] = base.toMap

    //create full graph mapping
    val lose_values = base_map.values.flatMap { node =>
      node.variables.map(variable => (variable -> node.variables.filter(connected => connected != variable)))
    }
    //Grouped by key
    val grouped = lose_values.groupBy(_._1)
    //Now merge
    val full_graph_mapping: Map[Int, Variable] = grouped.view.mapValues(_.flatMap(_._2).toList).toMap.map { case (id, variables) =>
      (id -> Variable(id, variables, "Blank"))
    }

    base_map.map { case (id, node) =>
      val tree_children = base.filter {
        case (id, base_node) => base_node.parent == id
      }.map { case (id, _) =>
        id
      }.toList

      val mapping =  Mapping(id, tree_children.map(child_id => (child_id -> base_map(child_id).variables.intersect(node.variables))).toMap)

      (id -> TreeNode (id, node.parent, tree_children, node.variables, full_graph_mapping, mapping))
    }
  }
}