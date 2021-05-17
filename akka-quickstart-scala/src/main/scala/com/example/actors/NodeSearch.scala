package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import com.example.actors.NodeSearch.{Event, SendOptimalSolution}
import com.example.actors.SolutionNode.SolutionEvent
import com.example.{Solution, TreeNode, Variable}
import com.example.solver.Solver

import scala.jdk.CollectionConverters._

class NodeSearch (node: TreeNode) {

  //For now lets just say we want the most amount of color 'red'
  def calcCost(color_mapping: Map[Int, String]): Int = {
    var cost: Int = 0
    color_mapping.foreach {
      case (_, color) =>
        if (color == "red") {
          cost += 1
        }
    }
    cost
  }

  //TODO; does this wait activly for a solution? As we also not to create the agent and need to send
  def receiveSolution(context: ActorContext[NodeSearch.Event], solutions: List[Map[Int, String]], best_solution: Map[Int, String], best_score: Int, index: Int): Behavior[Event] = {
    val current_solution = solutions(index)
    val solution_id = node.id.toString + "_" + index
    val solution_node: SolutionNode = SolutionNode(Solution(solution_id, node, current_solution), node.child_connected, parent_node)
    val solution_actor = context.spawn(
      solution_node,
      solution_id
    )
    Behaviors.receive { (context, message) =>
      message match {
        case SendOptimalSolution(solution) =>

          val cost = this.calcCost(solution)
          if (cost > best_score) {
            receiveSolution(context, solutions, solution, cost, index + 1)
          } else {
            receiveSolution(context, solutions, best_solution, best_score, index + 1)
          }
      }
    }
  }

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

    val node_search = new NodeSearch(node)

    val solutions = this.initializeNodes(node)
    context.log.info("Solution: {}", solutions)

    if(solutions.isEmpty) {
      parent_solution_node ! SolutionNode.SendSolution(Map())
      //TODO: stop here
      Behaviors.stopped
    }

    node_search.receiveSolution(context, solutions, null, 0, 0)

    //TODO: what happens here. Does above get executed first?
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
            tree_node.parent,
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
}
