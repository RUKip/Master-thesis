package com.example

import net.liftweb.json
import net.liftweb.json.Extraction.decompose
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._

import scala.jdk.CollectionConverters._
import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
import scala.util.Random

object InitializationHelper {

  implicit val formats = net.liftweb.json.DefaultFormats

  var highest_id = 0

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
    val node1 = Variable(1, List(2), "Blank")
    val node2 = Variable(2, List(1,3,4), "Blank")
    val node3 = Variable(3, List(2,4,5), "Blank")
    val node4 = Variable(4, List(2,3,6), "Blank")
    val node5 = Variable(5, List(3,6), "Blank")
    val node6 = Variable(6, List(4,5), "Blank")

    val mapping: Map[Int, Variable] = Map((1 -> node1), (2 -> node2), (3 -> node3), (4 -> node4), (5 -> node5), (6 -> node6))
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
    val root = TreeNode(1, 0, List(2, 3), List(2,3,4), graph_mapping, Map(2 -> List(3,4), 3 -> List(2)))
    val node2 = TreeNode(2, 1, List(4), List(3,4,5), graph_mapping, Map(4 -> List(4,5)))
    val node3 = TreeNode(3, 1, List(), List(1,2), graph_mapping, Map())
    val node4 = TreeNode(4, 2, List(), List(4,5,6), graph_mapping, Map())

    val mapping: Map[Int, TreeNode] = Map((1 -> root), (2 -> node2), (3 -> node3), (4 -> node4))
    (root, mapping)
  }

  //Generates a base tree structure to build a real HTD tree from (this is not deterministic)
  def createTreeStructure(branching_factor: Int = 3, width: Int = 4, parent: Int, start_index: Int, depth: Int, max_depth: Int): Seq[(Int, BaseTreeNode)] = {
    var children: Seq[(Int, BaseTreeNode)] = Seq()
    if (max_depth > depth) {
      val width_node = math.max(Random.nextInt(width - 1) + 1, 2) //Min width 2, else 2 to width
      if (depth == 1) {
        //Root node case
        val id = 1
        children = children :+ (id, BaseTreeNode(id, 0, variables = Range(start_index, start_index + width_node).toList))
        children = children ++ createTreeStructure(branching_factor, width, id, start_index + width_node, depth + 1, max_depth)
      } else {
        val branches = Random.nextInt(branching_factor)
        //val previous_overlap
        val range = Range(start_index, start_index + width_node*branches)
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

  def getId: Int = {
    highest_id += 1
    highest_id
  }

  def init(max_children: Int = 3, width: Int = 5, max_depth: Int = 7, overlap: Int = 0): Seq[(Int, BaseTreeNode)] = {
    highest_id = 0
    if (width > 9) {
      println("Invalid width, max width cannot be higher then 9")
      Seq()
    } else {
      val id = getId
      val root = (id, BaseTreeNode(id, 0, Range(1, 5).toList))
      make(1, Seq(root._2), max_children, width, max_depth, overlap) :+ root
    }
  }

  def make(depth: Int, parents: Seq[BaseTreeNode], max_children: Int, width: Int, max_depth: Int, overlap: Int): Seq[(Int, BaseTreeNode)] = {
    if (max_depth > depth) {
      parents.flatMap { parent =>
        val children = Random.nextInt(max_children)
        val treeNodes = Range(0, children).map { _ =>
          val id: Int = getId
          val treeNode = createTreeNode(id, parent, width, overlap)
          treeNode
        }
        treeNodes ++ make(depth + 1, treeNodes.map(value => value._2), max_children, width, max_depth, overlap)
      }
    } else {
      Seq()
    }
  }

  //Width cannot be higher then 9! else variables (id: 31, nr: 2, 312) and (id: 3, nr: 12, 312) clash
  def createTreeNode(id: Int, parent: BaseTreeNode, width: Int, picked_overlap: Int): (Int, BaseTreeNode) = {
    val chosen_overlap = if (picked_overlap < 1)  Random.nextInt(Math.max(parent.variables.size - 1, 1)) + 1 else picked_overlap
    val overlap = math.min(chosen_overlap, parent.variables.size - 1)
    val overlapped_variables = Random.shuffle(parent.variables).take(overlap)
    val values = overlapped_variables ++ Range(1, Random.nextInt(width - overlap) + 2).map { nr => (id.toString + nr.toString ).toInt}
    (id, BaseTreeNode(id, parent.id, values))
  }

  //Converts base structure to something usable (should always be deterministic)
  def createUsableTree(base: Seq[(Int, BaseTreeNode)]): Map[Int, TreeNode] = {
    val base_map: Map[Int, BaseTreeNode] = base.toMap

    //create full graph mapping
    val lose_values = base_map.values.flatMap { node =>
      node.variables.map(variable => (variable -> node.variables.filter(connected => connected != variable)))
    }
    //Grouped by key
    val grouped = lose_values.groupBy(_._1)
    //Now merge
    val full_graph_mapping: Map[Int, Variable] = grouped.view.mapValues(_.flatMap(_._2).toList.distinct).toMap.map { case (id, variables) =>
      (id -> Variable(id, variables, "Blank"))
    }

    base_map.map { case (id, node) =>
      val tree_children = base.filter {
        case (_, base_node) => base_node.parent == id
      }.map { case (base_id, _) =>
        base_id
      }.toList

      val mapping = tree_children.map(child_id => (child_id -> base_map(child_id).variables.intersect(node.variables))).toMap

      (id -> TreeNode (id, node.parent, tree_children, node.variables, full_graph_mapping, mapping))
    }
  }

  def storeTree(tree: Seq[(Int, BaseTreeNode)], file_name: String): Unit = {
    val file = new File(file_name)
    val bw = new BufferedWriter(new FileWriter(file))

    val json = tree.map {
      case (id, node: BaseTreeNode) => (id.toString -> decompose(node))
    }.toMap

    val jsonString = compactRender(json)

    println("writing: " + jsonString)
    bw.write(jsonString)
    bw.close()
  }

  def loadTree(file_name: String): Seq[(Int, BaseTreeNode)] = {
    val bufferedSource = Source.fromFile(file_name)
    val tree_string: String = bufferedSource.getLines.mkString
    bufferedSource.close
    val tree = json.parse(tree_string)

    //Below needed because of a bug in lift json, unable to convert correctly from under laying java code
    val converted_tree = tree.extract[Map[String, BaseTreeNode]].toSeq
    converted_tree.map { case (key: String, value: BaseTreeNode) => (key.toInt -> value) }
  }
}