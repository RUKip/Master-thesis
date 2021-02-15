package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import com.example.TreeNode

class Node(val node: TreeNode) {

//  final case class SolveTree(whom: String, replyTo: ActorRef)
  final case class PrintGraph()


//  def apply(): Behavior[SolveTree] = Behaviors.receive { (context, message) =>
//    context.log.info("Hello {}!", message.whom)
//    message.replyTo ! Greeted(message.whom, context.self)
//    Behaviors.same
//  }

  def apply(): Behavior[PrintGraph] = Behaviors.receive { (context, message) =>
    context.log.info(
      "Node {}, has color: {}, childeren: {}",
      node.id,
      node.color,
      node.childeren.map(
        (node: TreeNode) => node.id
      )
    )

    node.childeren.foreach( node => {
        val child_node = context.spawn(this.apply(), node.id.toString)
        child_node ! PrintGraph()
      }
    )
    Behaviors.same
  }
}
