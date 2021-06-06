package com.example.actors

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.cluster.ClusterEvent.{MemberEvent, MemberUp, ReachabilityEvent}
import akka.cluster.typed.{Cluster, Subscribe}
import com.example.TreeNode
import com.example.actors.Node.{ReceiveSolution, Terminate}
import com.example.actors.SolutionNode.{SendSolution, SolutionEvent}
import com.example.actors.TopLevel.{TopLevelServiceKey, startAlgorithm, matchToActorRef, spawnNode, storedActorReferences}

class TopLevel (val context: ActorContext[SolutionEvent], val all_tree_nodes: Map[Int, TreeNode], val nr_of_cluster_nodes: Int) {

  def receive(): Behavior[SolutionNode.SolutionEvent] ={
    Behaviors.receiveMessage {
      case MemberChange(changeEvent) =>
        changeEvent match {
          case MemberUp(member) =>
            context.log.info("Member is Up: {}", member.address)
            Behaviors.same
          case _: MemberEvent => Behaviors.same // ignore
        }
      case CreateNode(id: Int, children: Map[Int, ActorRef[Node.Event]], reply_to: ActorRef[SolutionEvent]) =>
        val actor_ref = spawnNode(all_tree_nodes(id), children, context)
        reply_to ! RegisterNodeRef(id, actor_ref)
        Behaviors.same
      case ListingResponse(TopLevelServiceKey.Listing(listings)) =>
        //This should be only called by master as the master is only subscribed to these events
        if (listings.size == nr_of_cluster_nodes) {
          val leaf_nodes = all_tree_nodes.filter {
            case (id, node) =>
              node.tree_children.isEmpty
          }
          buildNodeStructure(leaf_nodes, listings)
        } else {
          Behaviors.same
        }
    }
  }

  def buildNodeStructure(nodes: Map[Int, TreeNode], topLevelActors: Set[ActorRef[SolutionEvent]]): Behavior[SolutionEvent] = {
    var actor_nodes = topLevelActors.iterator
    nodes.foreach { case (id: Int, node: TreeNode) =>
      if ( ! actor_nodes.hasNext) {
        actor_nodes = topLevelActors.iterator
      }
      val actor_node = actor_nodes.next
      spawnAndRegister(node, actor_node, context)
    }
    listenForRegister(nodes, topLevelActors, 1)
  }

  //For every created node the master expects a register (Master only function)
  def listenForRegister(nodes: Map[Int, TreeNode], topLevelActors: Set[ActorRef[SolutionEvent]], depth: Int): Behavior[SolutionEvent] = {
    Behaviors.receiveMessage {
      case RegisterNodeRef(id: Int, ref: ActorRef[Node.Event]) =>
        context.log.info("Registering new ref: {} with id {}", ref, id)
        storedActorReferences += (id -> ref)
        if (storedActorReferences.size == all_tree_nodes.size) {
          startAlgorithm(storedActorReferences(1), context)
        } else if (depth == nodes.size) {
          context.log.info("Dividing next level of nodes")
          val next_parent_nodes = nodes.map { node =>
            val parent_id = node._2.parent
            (parent_id -> all_tree_nodes(parent_id))
          }
          val next_level_nodes = next_parent_nodes.filter {
            case (id, node) => (! storedActorReferences.contains(id)) && node.tree_children.forall(child => storedActorReferences.contains(child))
          }
          context.log.info("Next level nodes are: {}", next_level_nodes.keys)
          buildNodeStructure(next_level_nodes, topLevelActors)
        } else {
          listenForRegister(nodes, topLevelActors, depth+1)
        }
    }
  }

  def spawnAndRegister(tree_node: TreeNode, topLevelActor: ActorRef[SolutionEvent], context: ActorContext[SolutionEvent]): Unit = {
    context.log.info("Creating node with id: {} and childeren {} at {}", tree_node.id, tree_node.tree_children, topLevelActor)
    if (context.self == topLevelActor) {
      val actor_ref = spawnNode(all_tree_nodes(tree_node.id), matchToActorRef(tree_node.tree_children), context)
      context.self ! RegisterNodeRef(tree_node.id, actor_ref)
    } else {
      topLevelActor ! CreateNode(tree_node.id, matchToActorRef(tree_node.tree_children), context.self)
    }
  }
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

      val topLevelActor = new TopLevel(context, tree_nodes, nr_of_cluster_nodes)

      if (Cluster(context.system).selfMember.hasRole("master")) {
        context.system.receptionist ! Receptionist.Subscribe(TopLevelServiceKey, listingAdapter)
      }

      val memberEventAdapter: ActorRef[MemberEvent] = context.messageAdapter(MemberChange)
      Cluster(context.system).subscriptions ! Subscribe(memberEventAdapter, classOf[MemberEvent])

      topLevelActor.receive()
  }

  def startAlgorithm(root_actor: ActorRef[Node.Event], context: ActorContext[SolutionEvent]):  Behavior[SolutionNode.SolutionEvent] ={
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
