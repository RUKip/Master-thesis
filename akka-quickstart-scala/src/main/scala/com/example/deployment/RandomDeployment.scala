package com.example.deployment

import akka.actor.typed.ActorRef
import com.example.actors.SolutionNode
import com.example.actors.SolutionNode.SolutionEvent
import com.example.TreeNode

import scala.util.Random

case class RandomDeployment() extends Deployment {
  override def deploy(nodes: Map[Int, TreeNode], topLevelActors: Set[ActorRef[SolutionEvent]]): Map[Int, ActorRef[SolutionNode.SolutionEvent]] = {
    var actor_nodes = topLevelActors.iterator
    Random.shuffle(nodes.values).map { value =>
      if ( ! actor_nodes.hasNext) {
        actor_nodes = topLevelActors.iterator
      }
      val actor_node = actor_nodes.next
      (value.id, actor_node)
    }.toMap
  }
}
