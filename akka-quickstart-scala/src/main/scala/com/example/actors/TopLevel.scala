package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.cluster.ClusterEvent.{MemberEvent, MemberRemoved, MemberUp, ReachabilityEvent, ReachableMember, UnreachableMember}
import akka.cluster.typed.{Cluster, Subscribe}
import com.example.TreeNode
import com.example.actors.Node.{ReceiveSolution, Terminate}
import com.example.actors.SolutionNode.{SendSolution, SolutionEvent}

class TopLevel {
}

private final case class ReachabilityChange(reachabilityEvent: ReachabilityEvent) extends SolutionEvent
private final case class MemberChange(event: MemberEvent) extends SolutionEvent

object TopLevel {

  def apply(tree_nodes: Seq[TreeNode], nr_of_cluster_nodes: Int): Behavior[SolutionNode.SolutionEvent] = Behaviors.setup[SolutionNode.SolutionEvent] { context =>

    var cluster_nodes_ready = 0

    val memberEventAdapter: ActorRef[MemberEvent] = context.messageAdapter(MemberChange)
    Cluster(context.system).subscriptions ! Subscribe(memberEventAdapter, classOf[MemberEvent])

    val reachabilityAdapter = context.messageAdapter(ReachabilityChange)
    Cluster(context.system).subscriptions ! Subscribe(reachabilityAdapter, classOf[ReachabilityEvent])

    context.log.info("starting toplevel actor")
    //Initialize all tree node actors
    val actors = tree_nodes map { tree_node => {
        context.log.info("Spawning actor for tree_node: " + tree_node.id.toString)
        context.spawn(Node(tree_node), tree_node.id.toString)
      }
    }

    Behaviors.receiveMessage {
      case ReachabilityChange(reachabilityEvent) =>
        reachabilityEvent match {
          case UnreachableMember(member) =>
            context.log.info("Member detected as unreachable: {}", member)
            Behaviors.same
          case ReachableMember(member) =>
            context.log.info("Member back to reachable: {}", member)
            Behaviors.same
        }

      case MemberChange(changeEvent) =>
        changeEvent match {
          case MemberUp(member) =>
            cluster_nodes_ready += 1
            context.log.info("Member is Up: {}", member.address)
            if (nr_of_cluster_nodes == cluster_nodes_ready && (tree_nodes.head.id == 1)) {
              executeMasterBehaviour(actors, context)
            } else {
              Behaviors.same
            }
          case MemberRemoved(member, previousStatus) =>
            context.log.info("Member is Removed: {} after {}",
              member.address, previousStatus)
            Behaviors.same
          case _: MemberEvent => Behaviors.same // ignore
        }
    }
  }

  def executeMasterBehaviour(actors: Seq[ActorRef[Node.Event]], context: ActorContext[SolutionEvent]):  Behavior[SolutionNode.SolutionEvent] ={
        //This part only has to run for one TopLevel in the distributed actorsystem
          context.log.info("Executing from master")
          //Start algorithm
          actors.head ! ReceiveSolution(Map(): Map[Int, String], context.self)

          //Wait for final solution
          Behaviors.receive { (ctx, message) =>
            message match {
              case SendSolution(solution: Map[Int, String], score) =>
                context.log.info("Final solution is: {} {}", solution, score)

                //Terminate all still running tree nodes
                actors.head ! Terminate()

                Behaviors.stopped
              case _ =>
                context.log.error("Unexpected message: " + message)
                Behaviors.stopped
            }
          }
  }
}
