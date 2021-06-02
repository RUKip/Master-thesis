package com.example.actors

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.cluster.ClusterEvent.{MemberEvent, MemberUp, ReachabilityEvent}
import akka.cluster.typed.{Cluster, Subscribe}
import com.example.TreeNode
import com.example.actors.Node.{ReceiveSolution, Terminate}
import com.example.actors.SolutionNode.{SendSolution, SolutionEvent}

class TopLevel {
}

private final case class ReachabilityChange(reachabilityEvent: ReachabilityEvent) extends SolutionEvent
private final case class MemberChange(event: MemberEvent) extends SolutionEvent
private final case class CreateNode(id: Int, children: Map[Int, ActorRef[Node.Event]], reply_to: ActorRef[SolutionEvent]) extends SolutionEvent
private final case class RegisterNodeRef(id: Int, ref: ActorRef[Node.Event]) extends SolutionEvent
final case class ListingResponse(listing: Receptionist.Listing) extends SolutionEvent

object TopLevel {

  val TopLevelServiceKey: ServiceKey[SolutionEvent] = ServiceKey[SolutionEvent]("TopLevel")
  var storedActorReferences: Map[Int, ActorRef[Node.Event]] = Map()

  def apply(tree_nodes: Map[Int, TreeNode], nr_of_cluster_nodes: Int): Behavior[SolutionNode.SolutionEvent] =
    Behaviors.setup[SolutionNode.SolutionEvent] { context =>

      context.log.info("starting toplevel actor")

      val listingAdapter: ActorRef[Receptionist.Listing] =
        context.messageAdapter { listing => ListingResponse(listing)}

      context.system.receptionist ! Receptionist
        .Register(TopLevelServiceKey, context.self)

      if (Cluster(context.system).selfMember.hasRole("master")) {
        context.system.receptionist ! Receptionist.Subscribe(TopLevelServiceKey, listingAdapter)

        Behaviors.receiveMessagePartial[SolutionEvent] {
          case ListingResponse(TopLevelServiceKey.Listing(listings)) =>
            if (listings.size == nr_of_cluster_nodes) {
              val leaf_nodes = tree_nodes.filter {
                case (id, node) =>
                  node.tree_children.isEmpty
              }
              divideNodes(leaf_nodes, listings.toArray, context, tree_nodes)
            } else {
              Behaviors.same
            }
        }
      }

      val memberEventAdapter: ActorRef[MemberEvent] = context.messageAdapter(MemberChange)
      Cluster(context.system).subscriptions ! Subscribe(memberEventAdapter, classOf[MemberEvent])

      Behaviors.receiveMessage {
        case MemberChange(changeEvent) =>
          changeEvent match {
            case MemberUp(member) =>
              context.log.info("Member is Up: {}", member.address)
              Behaviors.same
            case _: MemberEvent => Behaviors.same // ignore
          }
        case CreateNode(id: Int, children: Map[Int, ActorRef[Node.Event]], reply_to: ActorRef[SolutionEvent]) =>
          val actor_ref = spawnNode(tree_nodes(id), children, context)
          reply_to ! RegisterNodeRef(id, actor_ref)
          Behaviors.same
      }
  }

  def executeMasterBehaviour(root_actor: ActorRef[Node.Event], context: ActorContext[SolutionEvent]):  Behavior[SolutionNode.SolutionEvent] ={
        //This part only has to run for one TopLevel in the distributed actorsystem
          context.log.info("Executing from master")
          //Start algorithm
          root_actor ! ReceiveSolution(Map(): Map[Int, String], context.self)

          //Wait for final solution
          Behaviors.receive { (ctx, message) =>
            message match {
              case SendSolution(solution: Map[Int, String], score) =>
                context.log.info("Final solution is: {} {}", solution, score)

                //Terminate all still running tree nodes
                root_actor ! Terminate()

                Behaviors.stopped
              case _ =>
                context.log.error("Unexpected message: " + message)
                Behaviors.stopped
            }
          }
  }

  //Only called by master TODO: This is now the deployment part (for now round robbin)
  def divideNodes(nodes: Map[Int, TreeNode], topLevelActors: Array[ActorRef[SolutionEvent]], context: ActorContext[SolutionEvent], all_tree_nodes: Map[Int, TreeNode]): Behaviors.Receive[SolutionEvent] = {
    val divided_nodes = nodes.values.grouped(nodes.size/topLevelActors.length).toArray
    topLevelActors.indices.foreach( topLevel => {
      val top_level_actor = topLevelActors(topLevel)
      divided_nodes(topLevel).foreach(tree_node =>
        top_level_actor ! CreateNode(tree_node.id, matchToActorRef(tree_node.tree_children), context.self)
      )
    })

    Behaviors.receiveMessage {
      case RegisterNodeRef(id: Int, ref: ActorRef[Node.Event]) =>
        storedActorReferences += (id -> ref)
        if (storedActorReferences.size == nodes.size) {
          if (storedActorReferences.size == all_tree_nodes.size) {
            executeMasterBehaviour(storedActorReferences(1), context)
          } else {
            context.log.info("Dividing next level of nodes")
            val next_parent_nodes = nodes.map { node =>
              val parent_id = node._2.parent
              (parent_id -> all_tree_nodes(parent_id))
            }
            val next_level_nodes = next_parent_nodes.filter {
              case (id, _) => ! storedActorReferences.contains(id)
            }
            divideNodes(next_level_nodes, topLevelActors, context, all_tree_nodes)
          }
        }
        Behaviors.same
    }
  }

  def spawnNode(tree_node: TreeNode, children: Map[Int, ActorRef[Node.Event]], context: ActorContext[SolutionEvent]): ActorRef[Node.Event] = {
    context.log.info("Spawning actor for tree_node: " + tree_node.id.toString)
    context.spawn(Node(tree_node, children), tree_node.id.toString)
  }

  def matchToActorRef(children: List[Int]): Map[Int, ActorRef[Node.Event]] = {
    children.map(id =>
      (id -> storedActorReferences(id))
    ).toMap
  }
}
