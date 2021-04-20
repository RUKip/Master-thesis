package com.example

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

    val root = TreeNode(1, null, List(2, 3), List(2,3,4), graph_mapping, List(), Mapping(1, Map(2 -> List(3,4), 3 -> List(2))))
    val node2 = TreeNode(2, root, List(4), List(3,4,5), graph_mapping, List(3,4), Mapping(2, Map(4 -> List(4,5))))
    val node3 = TreeNode(3, root, List(), List(1,2), graph_mapping, List(2), Mapping(3,Map()))
    val node4 = TreeNode(4, node2, List(), List(4,5,6), graph_mapping, List(4,5), Mapping(4,Map()))

    mapping += (1 -> root)
    mapping += (2 -> node2)
    mapping += (3 -> node3)
    mapping += (4 -> node4)

    (root, mapping)
  }
}
