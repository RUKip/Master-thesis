package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.Behaviors
import com.example.actors.Node._
import com.example.actors.SolutionNode.SolutionEvent
import com.example.TreeNode

//This node should be representing a node in the Hypertree decomposition (else could not be solved nicely in parallel)
class Node(listingAdapter: ActorRef[Receptionist.Listing]) {

  def receive(tree_node: TreeNode, solution_id: Int): Behavior[Event] = {
    Behaviors.receive { (context, message) =>
      val NodeServiceKey: ServiceKey[Event] = ServiceKey[Event](tree_node.id.toString)
      message match {
        //This is a testing behaviour
        case PrintGraph() =>
          context.log.info(
            "Tree Node {}, has graph nodes: {}, parent: {}, children: {}, path:  {}",
            tree_node.id,
            tree_node.graph_variables,
            tree_node.parent,
            tree_node.tree_childeren,
            context.self.path,
          )
          Behaviors.same
        case ReceiveSolution(parent_color_mapping: Map[Int, String], solution_node: ActorRef[SolutionEvent]) =>
          context.log.info("Node received solution: " + parent_color_mapping.toString())
          var new_node : TreeNode = tree_node.updateNodes(parent_color_mapping)

          if (solution_node != null) {
            this.waitForSolution(tree_node, solution_node, solution_id)
          } else {
            context.spawn(
              NodeSearch(new_node, context.self, solution_node),
              solution_id.toString
            )
            receive(new_node, solution_id + 1)
          }
        //Response of receptionist
        case ListingResponse(NodeServiceKey.Listing(listings)) =>
          context.log.info("For the send back actor references send them a new message")
          val xs: Set[ActorRef[Event]] = listings
            xs foreach { replyTo =>
              replyTo ! Terminate()
            }
            Behaviors.stopped
        case Terminate() =>
          tree_node.tree_childeren.foreach(id => {
            context.system.receptionist ! Receptionist.Find(ServiceKey[Event](id.toString), listingAdapter)
            receive(tree_node, solution_id)
          })
          Behaviors.stopped
      }
    }
  }

  private def waitForSolution(node: TreeNode, parent_solution_node: ActorRef[SolutionEvent], solution_id: Int): Behavior[Event] = {
    Behaviors.receive { (context, message) =>
      message match {
        case SendSolution(solution: Map[Int, String]) => {
          val new_node = node.updateNodes(solution)
          context.spawn(
            NodeSearch(new_node, context.self, parent_solution_node),
            solution_id.toString
          )
          receive(new_node, solution_id+1)
        }
      }
    }
  }
}

object Node {
  sealed trait Event //Scalas enum
  final case class PrintGraph() extends Event
  final case class ReceiveSolution(parent_color_mapping: Map[Int, String], solution_node: ActorRef[SolutionEvent]) extends Event
  final case class ListingResponse(listing: Receptionist.Listing) extends Event
  final case class SendSolution(solution: Map[Int, String]) extends Event
  final case class Terminate() extends Event

  def apply(tree_node: TreeNode): Behavior[Event] = Behaviors.setup { context =>
    val NodeServiceKey: ServiceKey[Event] = ServiceKey[Event](tree_node.id.toString)
    context.system.receptionist ! Receptionist.Register(NodeServiceKey, context.self)

    //Defines what message is responded after the actor is requested from the receptionist
    val listingAdapter: ActorRef[Receptionist.Listing] =
      context.messageAdapter { listing => ListingResponse(listing)}

    val node = new Node(listingAdapter)

    context.log.info("Node " + tree_node.id + " setup, starting to receive")
    node.receive(tree_node, 0)
  }
}
