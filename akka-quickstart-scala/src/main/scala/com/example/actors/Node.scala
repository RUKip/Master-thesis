package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import com.example.actors.Node._
import com.example.actors.SolutionNode.SolutionEvent
import com.example.{CborSerializable, TreeNode}
import com.fasterxml.jackson.databind.annotation.JsonDeserialize

//This node should be representing a node in the Hypertree decomposition (else could not be solved nicely in parallel)
class Node(child_refs: Map[ActorRef[Node.Event], List[Int]]) {

  def receive(tree_node: TreeNode, solution_id: Int, all_recorded_goods: Map[List[(Int, String)], (Int, Map[Int, String])], all_recorded_no_goods: Map[List[(Int, String)], Boolean]): Behavior[Event] = {
    Behaviors.receive { (context, message) =>
      message match {
        //This is a testing behaviour
        case PrintGraph() =>
          context.log.info(
            "Tree Node {}, has graph nodes: {}, parent: {}, children: {}, path:  {}",
            tree_node.id,
            tree_node.graph_variables,
            tree_node.parent,
            tree_node.tree_children,
            context.self.path,
          )
          Behaviors.same
        case ReceiveSolution(parent_color_mapping: Map[Int, String], solution_node: ActorRef[SolutionEvent]) =>
          context.log.info("Node received solution: " + parent_color_mapping.toString() + " from solution node: " + solution_node.path.toString)
          val new_node : TreeNode = tree_node.updateNodes(parent_color_mapping)
          context.spawn(
            NodeSearch(new_node, child_refs, context.self, solution_node, all_recorded_goods, all_recorded_no_goods),
            solution_id.toString
          )
          receive(new_node, solution_id+1, all_recorded_goods, all_recorded_no_goods)
        case SendRecording(recorded_goods, recorded_no_goods) =>
          receive(tree_node, solution_id, mergeRecordedGoods(all_recorded_goods, recorded_goods), all_recorded_no_goods ++ recorded_no_goods)
        case Terminate() =>
          context.log.info("Terminating..")
          child_refs.keys foreach { replyTo =>
            replyTo ! Terminate()
          }
          Behaviors.stopped
        case _ =>
          context.log.error("Unexpected message: " + message)
          Behaviors.stopped
      }
    }
  }

  def mergeRecordedGoods(all_recorded_goods: Map[List[(Int, String)], (Int, Map[Int, String])], recorded_goods: Map[List[(Int, String)], (Int, Map[Int, String])]): Map[List[(Int, String)], (Int, Map[Int, String])] = {
    val replace: Map[List[(Int, String)], (Int, Map[Int, String])] = recorded_goods.map { case (key, value) =>
      if (all_recorded_goods.contains(key)) {
        val recording = all_recorded_goods(key)
        if (recording._1 < value._1) {
          (key, value)
        } else {
          (key, recording)
        }
      } else {
        (key, value)
      }
    }
    all_recorded_goods ++ replace
  }
}

object Node {
  sealed trait Event extends CborSerializable //Scalas enum
  final case class PrintGraph() extends Event
  final case class ReceiveSolution(@JsonDeserialize(keyAs = classOf[Int]) parent_color_mapping: Map[Int, String], from: ActorRef[SolutionEvent]) extends Event
  final case class Terminate() extends Event
  final case class SendRecording(recorded_goods: Map[List[(Int, String)], (Int, Map[Int, String])], recorded_no_goods: Map[List[(Int, String)], Boolean]) extends Event

  def apply(tree_node: TreeNode, child_refs: Map[ActorRef[Node.Event], List[Int]]): Behavior[Event] = Behaviors.setup { context =>
    val node = new Node(child_refs)

    context.log.info("Node " + tree_node.id + " setup, starting to receive")
    node.receive(tree_node, 0, Map(), Map())
  }
}
