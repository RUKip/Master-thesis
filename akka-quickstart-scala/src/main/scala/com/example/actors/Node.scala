package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.Behaviors
import com.example.{TreeNode, Variable}
import com.example.solver.Solver

import scala.jdk.CollectionConverters._

//This node should be representing a node in the Hypertree decomposition (else could not be solved nicely in parallel)
class Node() {

}

object Node {
  sealed trait Event //Scalas enum
  final case class PrintGraph() extends Event
  final case class Initialize(parent_color_mapping: Map[Int, String]) extends Event
  final case class BackTrack() extends Event
  final case class ListingResponse(listing: Receptionist.Listing) extends Event

  def apply(node: TreeNode): Behavior[Event] = Behaviors.setup { context =>
    val NodeServiceKey: ServiceKey[Event] = ServiceKey[Event](node.id.toString)
    context.system.receptionist ! Receptionist.Register(NodeServiceKey, context.self)

    //Defines what message is responded after the actor is requested from the receptionist
    val listingAdapter: ActorRef[Receptionist.Listing] =
      context.messageAdapter { listing => ListingResponse(listing)}

    receive(node)
  }

  def receive(tree_node: TreeNode): Behavior[Event] = {
    Behaviors.receive { (context, message) =>
      val NodeServiceKey: ServiceKey[Event] = ServiceKey[Event](tree_node.id.toString)
      message match {
        //This is a testing behaviour
        case PrintGraph() =>
          context.log.info(
            "Tree Node {}, has graph nodes: {}, parent: {}, children: {}, path:  {}",
            tree_node.id,
            tree_node.graph_variables,
            if (tree_node.parent == null)  0  else tree_node.parent.id,
            tree_node.tree_childeren,
            context.self.path,
          )

        //Solve COP for this subtree here (using something like choco solver)
        case Initialize(parent_color_mapping: Map[Int, String]) =>
          val new_node : TreeNode = tree_node.updateNodes(parent_color_mapping)

          val solutions = this.initializeNodes(new_node)
          context.log.info("Solution: {}", solutions)
          //          solutions.map(solution => {
          //          })
          receive(new_node)

        //Response of receptionist
        case ListingResponse(NodeServiceKey.Listing(listings)) =>
          context.log.info("For the send back actor references send them a new message")
          val xs: Set[ActorRef[Event]] = listings
          xs foreach { replyTo =>
            //#greeter-send-messages
            //replyTo !
          }

        //Wait for messages from children (depends on what implementation of solver, but for example see hybrid-backtracking paper could be message good/no-good)
        case BackTrack() =>

      }
      Behaviors.same
    }
  }

  def initializeNodes(node: TreeNode): List[Map[Int, String]] = {
    val java_mapping: Map[Integer, Variable] = node.full_graph_mapping map {case (key, value) => (key:java.lang.Integer, value)}
    val solutions: List[Map[Int, String]]  = Solver.solve(node.getGraphNodes.asJava, java_mapping.asJava)
      .asScala
      .toList
      .map(internal_map => internal_map.asScala.toMap map {case (key, value) => (key.toInt, value) })
    solutions
  }
}
