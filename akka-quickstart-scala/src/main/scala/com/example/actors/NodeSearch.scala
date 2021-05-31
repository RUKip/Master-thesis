package com.example.actors

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import com.example.actors.NodeSearch.{Event, ListingResponse, PrintGraph, SendOptimalSolution}
import com.example.actors.SolutionNode.SolutionEvent
import com.example.{Solution, TreeNode}
import com.example.solver.SolverScalaWrapper

class NodeSearch (node: TreeNode, parent_solution_node: ActorRef[SolutionEvent], solutions: List[Map[Int, String]], context: ActorContext[Event]) {

  //Defines what message is responded after the actor is requested from the receptionist
  val listingAdapter: ActorRef[Receptionist.Listing] =
    context.messageAdapter { listing => {
      val from: Int = listing.getKey.id.toInt
      context.log.info("Converting: " + listing + " to: " + ListingResponse(listing, from))
      ListingResponse(listing, from)
    }
    }

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

  def mainLoop(context: ActorContext[NodeSearch.Event], best_solution: Map[Int, String], best_score: Int, index: Int, child_refs: Map[Int, ActorRef[Node.Event]]): Behavior[Event] = {
    if (solutions.size > index) {
      val current_solution = node.updateNodes(solutions(index)).full_graph_mapping
      val solution_id = node.id.toString + "_" + index

      val solution_actor = context.spawn(
        SolutionNode(Solution(solution_id, node, current_solution, 0), node.child_connected, node.tree_childeren, context.self, child_refs),
        solution_id
      )
      Behaviors.receive { (context, message) =>
        message match {
          case SendOptimalSolution(solution) =>
            val cost = this.calcCost(solution)
            if (cost > best_score) {
              mainLoop(context, solution, cost, index + 1, child_refs)
            } else {
              mainLoop(context, best_solution, best_score, index + 1, child_refs)
            }
          case PrintGraph() =>
            context.log.info(
              "Tree Node {}, has graph nodes: {}, parent: {}, children: {}, path:  {}",
              node.id,
              node.graph_variables,
              node.parent,
              node.tree_childeren,
              context.self.path,
            )
            Behaviors.same
          case _ =>
            context.log.error("Unexpected message: " + message)
            Behaviors.stopped
        }
      }
    } else {
      context.log.info("Found local best solution: {}, score: {}, stopping...", best_solution, best_score)
      parent_solution_node ! SolutionNode.SendSolution(best_solution, best_score)
      Behaviors.stopped
    }
  }

  def receiveNodeRef(): Behavior[Event] = {
    Behaviors.receive { (context, message) =>
      context.log.info("Node search ready to receive for node: " + node.id.toString)
      val NodeServiceKey: ServiceKey[Node.Event] = ServiceKey[Node.Event](node.id.toString)
      message match {
        case ListingResponse(NodeServiceKey.Listing(listings), from) =>
          val xs: Set[ActorRef[Node.Event]] = listings
          val child_refs: Map[Int, ActorRef[Node.Event]] = xs.map { replyTo =>
            val node_id = replyTo.path.name.toInt
            (node_id, replyTo)
          }.toMap
          context.log.info("Got addresses " + child_refs  + "  from receptionist for: " + from)
          mainLoop(context, null, 0, 0, child_refs)
        case _ =>
          context.log.error("Unexpected message: " + message)
          Behaviors.stopped
      }
    }
  }

  def requestChildRef(): Unit = {
    context.system.receptionist ! Receptionist.Find(ServiceKey[Node.Event](node.id.toString), listingAdapter)
  }
}


object NodeSearch {
  sealed trait Event extends SolutionEvent//Scalas enum
  final case class PrintGraph() extends Event
  final case class SendOptimalSolution(solution: Map[Int, String]) extends Event
  final case class ListingResponse(listing: Receptionist.Listing, from: Int) extends Event

  def apply(node: TreeNode, parent_node: ActorRef[Node.Event], parent_solution_node: ActorRef[SolutionEvent]): Behavior[Event] = Behaviors.setup { context =>
    val solutions = SolverScalaWrapper.calcSolutions(node)
    context.log.info("For node " + node.graph_variables + " Solution: {}", solutions)

    val node_search = new NodeSearch(node, parent_solution_node, solutions, context)

    if(solutions.isEmpty) {
      context.log.info("No solutions, stopping")
      parent_solution_node ! SolutionNode.SendSolution(Map(), 0)
      Behaviors.stopped
    } else {
      node_search.requestChildRef()
      node_search.receiveNodeRef()
    }
  }
}
