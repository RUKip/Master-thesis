package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.Behaviors
import com.example.actors.SolutionNode.SolutionEvent
import com.example.{Solution, TreeNode, Variable}
import com.example.solver.Solver

import scala.jdk.CollectionConverters._

class NodeSearch {

}


object NodeSearch {
  sealed trait Event //Scalas enum
  final case class PrintGraph() extends Event
  final case class Initialize(parent_color_mapping: Map[Int, String]) extends Event
  final case class BackTrack() extends Event
  final case class ListingResponse(listing: Receptionist.Listing) extends Event
  final case class SendOptimalSolution(solution: Map[Int, String]) extends Event

  def apply(node: TreeNode, parent_node: ActorRef[Node.Event], parent_solution_node: ActorRef[SolutionEvent]): Behavior[Event] = Behaviors.setup { context =>
    val NodeServiceKey: ServiceKey[Event] = ServiceKey[Event](node.id.toString)
    context.system.receptionist ! Receptionist.Register(NodeServiceKey, context.self)

    val solutions = this.initializeNodes(node)
    context.log.info("Solution: {}", solutions)

    if(solutions.isEmpty) {
      parent_solution_node ! SolutionNode.SendSolution(Map())
      //TODO: stop here
      Behaviors.stopped
    }

    var optimal_solution: Solution = null
    var optimal_cost: Int = 0

    solutions.zipWithIndex.foreach { case (color_mapping: Map[Int, String], index: Int) => {
      val solution_id = node.id.toString + "_" + index
      val solution_node: SolutionNode = SolutionNode(Solution(solution_id, node, color_mapping), node.child_connected, parent_node)
//      val actor = context.spawn(
//        solution_node,
//        solution_id
//      )
//      actor !
    }
    }


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

  def findOptimalSolution(solutions: List[Solution]): (Solution, Int) = {
    var optimal_solution: Solution = null
    var optimal_cost: Int = 0
    solutions.foreach()
    (optimal_solution, optimal_cost)
  }
}
