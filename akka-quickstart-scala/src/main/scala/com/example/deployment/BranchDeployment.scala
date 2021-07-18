package com.example.deployment

import akka.actor.typed.ActorRef
import com.example.actors.SolutionNode
import com.example.actors.SolutionNode.SolutionEvent
import com.example.TreeNode

case class BranchDeployment() extends Deployment {
  override def deploy(nodes: Map[Int, TreeNode], topLevelActors: Set[ActorRef[SolutionEvent]]): Map[Int, ActorRef[SolutionNode.SolutionEvent]] = {
    var new_nodes: Map[Int, TreeNode] = nodes
    //Sorting for determinism
    val all_nodes = nodes.toList.sortBy(_._1).map(_._2)

    //Take leaf nodes, divide them over all cluster nodes, then iterate up for each avoiding conflicting parents
    val leaf_nodes = all_nodes.filter(_.isLeaf)

    val nodes_per_actor = Math.ceil(leaf_nodes.size.toDouble / topLevelActors.size.toDouble).toInt
    val leaf_per_actor = leaf_nodes.grouped(nodes_per_actor).toList
    val branches_per_actor = topLevelActors.zip(leaf_per_actor)

    branches_per_actor.flatMap { case (cluster, leaves) =>
      val divided_nodes = leaves.flatMap { leaf =>
        val branch = getBranch(new_nodes, leaf, Map(leaf.id -> leaf))
        //Remove parents, that are already claimed
        new_nodes = nodes.toSet.diff(branch.toSet).toMap
        branch.keys
      }
      divided_nodes.map { leaf => (leaf, cluster)}
    }.toMap
  }

  def getBranch(nodes: Map[Int, TreeNode], current_node: TreeNode, branch: Map[Int, TreeNode]): Map[Int, TreeNode] = {
    if (nodes.contains(current_node.parent)) {
      val new_node = nodes(current_node.parent)
      getBranch(nodes, new_node, branch + (current_node.parent -> new_node))
    } else {
      branch
    }
  }
}
