package com.example.deployment

import akka.actor.typed.ActorRef
import com.example.TreeNode
import com.example.actors.SolutionNode
import com.example.actors.SolutionNode.SolutionEvent

trait Deployment {
  def deploy(nodes: Map[Int, TreeNode], topLevelActors: Set[ActorRef[SolutionEvent]]): Map[Int, ActorRef[SolutionNode.SolutionEvent]]
}
