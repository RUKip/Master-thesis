package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import com.example.actors.NodeSearch.{Event, PrintGraph, SendOptimalSolution}
import com.example.actors.SolutionNode.SolutionEvent
import com.example.{Solution, TreeNode}
import com.example.solver.SolverScalaWrapper
import com.fasterxml.jackson.databind.annotation.JsonDeserialize

class NodeSearch (node: TreeNode, child_refs: Map[ActorRef[Node.Event], List[Int]], parent_solution_node: ActorRef[SolutionEvent], context: ActorContext[Event], parent_node: ActorRef[Node.Event]) {

  val COLOR_COST_MAPPING = Map(
    ("red" -> 5),
    ("blue" -> 4),
    ("yellow" -> 1),
    ("green" -> 2),
    ("white" -> 0),
    ("black" -> 0),
    ("orange" -> 2)
  )

  /** Calculate the cost of the local solution */
  def calcCost(color_mapping: Map[Int, String]): Int = {
    var cost: Int = 0
    color_mapping.foreach {
      case (_, color) =>
        cost += COLOR_COST_MAPPING.getOrElse(color, 0)
    }
    cost
  }

  def mainLoop(context: ActorContext[NodeSearch.Event], best_solution: Option[Map[Int, String]], best_score: Int, solutions: List[Map[Int, String]], recorded_goods: Map[List[(Int, String)], (Int, Map[Int, String])], recorded_no_goods: Map[List[(Int, String)], Boolean]): Behavior[Event] = {
    if (solutions.nonEmpty) {
      val solution: Map[Int, String] = solutions.head
      val new_node: TreeNode = node.updateNodes(solution)
      val applicable_solution = new_node.graph_variables.map(id =>(id -> new_node.full_graph_mapping(id))).toMap
      val solution_id = node.id.toString + "_" + solutions.size

      if (hasRecordedNoGood(recorded_no_goods, solution)) {
        //Solution is in recorded no good, so skip
        //context.log.info("No good found for: {}", solution)
        mainLoop(context, best_solution, best_score, solutions.tail, recorded_goods, recorded_no_goods)
      } else {
        val (recorded_cost, recording) = hasRecordedGood(recorded_goods, solution)
        if (recorded_cost >= 0) {
          val (new_recorded_cost, new_recorded_solution, new_recorded_goods) = compareLocalGood(solution, recording, recorded_cost, recorded_goods)
          //context.log.info("Good found for: {} with cost: {}", solution, recorded_cost)

          if (best_score >= recorded_cost) {
            mainLoop(context, best_solution, best_score, solutions.tail, new_recorded_goods, recorded_no_goods)
          } else {
            mainLoop(context, Option(new_recorded_solution), new_recorded_cost, solutions.tail, new_recorded_goods, recorded_no_goods)
          }
        } else {
          val solution_actor = context.spawn(
            SolutionNode(Solution(solution_id, new_node, applicable_solution, 0), new_node.tree_children, context.self, child_refs),
            solution_id
          )
          Behaviors.receive { (context, message) =>
            message match {
              case SendOptimalSolution(received_solution: Option[Map[Int, String]]) =>
                val new_solution: Map[Int, String] = if (received_solution.isEmpty) Map() else received_solution.get
                val cost = this.calcCost(new_solution)

                val new_recorded_goods = if (received_solution.isEmpty) recorded_goods else recorded_goods + recordGood(solution, new_solution, cost)
                val new_recorded_no_goods = if (received_solution.isEmpty) recorded_no_goods + recordNoGood(solution) else recorded_no_goods

                if (cost >= best_score) {
                  mainLoop(context, Option(new_solution), cost, solutions.tail, new_recorded_goods, new_recorded_no_goods)
                } else {
                  mainLoop(context, best_solution, best_score, solutions.tail, new_recorded_goods, new_recorded_no_goods)
                }
              case PrintGraph() =>
                context.log.info(
                  "Tree Node {}, has graph nodes: {}, parent: {}, children: {}, path:  {}",
                  node.id,
                  node.graph_variables,
                  node.parent,
                  node.tree_children,
                  context.self.path,
                )
                Behaviors.same
              case _ =>
                context.log.error("Unexpected message: " + message)
                Behaviors.stopped
            }
          }
        }
      }
    } else {
      //context.log.info("Found local best solution: {}, score: {}, stopping...", best_solution, best_score)
      parent_node ! Node.SendRecording(recorded_goods, recorded_no_goods)
      if (best_solution.isEmpty) {
        parent_solution_node ! SolutionNode.SendSolution(Map(), 0)
      } else {
        parent_solution_node ! SolutionNode.SendSolution(best_solution.get, best_score)
      }
      Behaviors.stopped
    }
  }

  def receiveNodeRef(solutions: List[Map[Int, String]],  recorded_goods: Map[List[(Int, String)], (Int, Map[Int, String])], recorded_no_goods: Map[List[(Int, String)], Boolean]): Behavior[Event] = {
    //context.log.info("Node search ready to receive for node: " + node.id.toString)
    mainLoop(context, None, 0, solutions, recorded_goods, recorded_no_goods)
  }

  //Below stuff for (no)good recording

  def recordGood(send_solution: Map[Int, String], received_solution: Map[Int, String], received_cost: Int): (List[(Int, String)], (Int, Map[Int, String])) = {
    val recording = getRecording(send_solution)
    (recording, (received_cost, received_solution))
  }

  def recordNoGood(send_solution: Map[Int, String]): (List[(Int, String)], Boolean) = {
    val recording = getRecording(send_solution)
    (recording, false)
  }

  /** Gets recording, eg. the variables and there assignment that overlap with child node and the optimal solution */
  def getRecording(send_solution: Map[Int, String]): List[(Int, String)] =  {
    val intersecting_variables = child_refs.values.flatten.toList.distinct.sorted
    intersecting_variables.map { variable =>
      (variable -> send_solution(variable))
    }
  }

  /** returns -1 if not found else the cost */
  def hasRecordedGood(goods: Map[List[(Int, String)], (Int, Map[Int, String])], solution: Map[Int, String]): (Int, Map[Int, String]) = {
    val recording = getRecording(solution)
    goods.getOrElse(recording, (-1, Map()))
  }

  def hasRecordedNoGood(no_goods: Map[List[(Int, String)], Boolean], solution: Map[Int, String]): Boolean = {
    val recording = getRecording(solution)
    no_goods.contains(recording)
  }

  /** For comparing local good vs new good */
  def compareLocalGood(solution: Map[Int, String], recording: Map[Int, String], recorded_cost: Int, recorded_goods: Map[List[(Int, String)], (Int, Map[Int, String])]): (Int, Map[Int, String], Map[List[(Int, String)], (Int, Map[Int, String])]) = {
    val local_solution = recording ++ solution
    val local_cost = calcCost(local_solution)
    if (local_cost > recorded_cost) {
      (local_cost, local_solution, recorded_goods + recordGood(solution, local_solution, local_cost))
    } else {
      (recorded_cost, recording, recorded_goods)
    }
  }
}

object NodeSearch {
  sealed trait Event extends SolutionEvent
  final case class PrintGraph() extends Event
  final case class SendOptimalSolution(@JsonDeserialize(keyAs = classOf[Int]) solution: Option[Map[Int, String]]) extends Event

  def apply(node: TreeNode, child_refs: Map[ActorRef[Node.Event], List[Int]], parent_node: ActorRef[Node.Event], parent_solution_node: ActorRef[SolutionEvent],  recorded_goods: Map[List[(Int, String)], (Int, Map[Int, String])], recorded_no_goods: Map[List[(Int, String)], Boolean]): Behavior[Event] = Behaviors.setup { context =>
    val solutions = SolverScalaWrapper.calcSolutions(node)
    //context.log.info("For variables " + node.graph_variables + " Solution: {}", solutions)

    val node_search = new NodeSearch(node, child_refs, parent_solution_node, context, parent_node)

    if(solutions.isEmpty) {
      //context.log.info("No solutions, stopping")
      parent_solution_node ! SolutionNode.SendSolution(Map(), 0)
      Behaviors.stopped
    } else {
      node_search.receiveNodeRef(solutions, recorded_goods, recorded_no_goods)
    }
  }
}
