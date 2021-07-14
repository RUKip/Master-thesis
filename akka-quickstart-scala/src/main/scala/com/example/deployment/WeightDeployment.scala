package com.example.deployment
import akka.actor.typed.ActorRef
import com.example.TreeNode
import com.example.actors.SolutionNode
import com.example.actors.SolutionNode.SolutionEvent

import scala.annotation.tailrec
import scala.collection.mutable

case class WeightDeployment() extends Deployment {
  override def deploy(nodes: Map[Int, TreeNode], topLevelActors: Set[ActorRef[SolutionEvent]]): Map[Int, ActorRef[SolutionNode.SolutionEvent]] = {

    val weighted_nodes: List[(Int, Int)] = nodes.map { case (key: Int, value: TreeNode) =>
      (key -> calculateWeight(value, nodes, 1))
    }.toList.sortBy { case (id, weight) =>
      weight
    }(Ordering[Int].reverse)

    val weight_ordered_actors = mutable.PriorityQueue.empty[(Int, ActorRef[SolutionEvent])](
      implicitly[Ordering[(Int, ActorRef[SolutionEvent])]].reverse
    )

    topLevelActors.foreach { actor => weight_ordered_actors.enqueue((0, actor))}

    weighted_nodes.map { case (key, weight) =>
      val lowest_weight_actor = weight_ordered_actors.dequeue()
      weight_ordered_actors.enqueue((lowest_weight_actor._1 + weight, lowest_weight_actor._2))
      (key -> lowest_weight_actor._2)
    }.toMap
  }

    @tailrec
    final def calculateWeight(node: TreeNode, nodes: Map[Int, TreeNode], weight: Int): Int = {
      if (0 != node.parent) {
        calculateWeight(nodes(node.parent), nodes, weight+1)
      } else {
        weight
      }
    }
}
