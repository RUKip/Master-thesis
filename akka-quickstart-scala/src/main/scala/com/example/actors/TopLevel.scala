package com.example.actors

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.cluster.typed.Cluster
import com.example.TreeNode
import com.example.actors.Node.{ReceiveSolution, Terminate}
import com.example.actors.SolutionNode.{SendSolution, SolutionEvent}
import com.example.actors.TopLevel.{TopLevelServiceKey, storedActorReferences}

import java.io.{BufferedWriter, File, FileWriter}
import java.time.{Duration, Instant}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

class TopLevel (val context: ActorContext[SolutionEvent], val all_tree_nodes: Map[Int, TreeNode], val nr_of_cluster_nodes: Int) {

  def receive(): Behavior[SolutionNode.SolutionEvent] ={
    Behaviors.receiveMessage {
      case CreateNode(id: Int, children: List[(ActorRef[Node.Event], List[Int])], reply_to: ActorRef[SolutionEvent]) =>
        val actor_ref = spawnNode(all_tree_nodes(id), children.toMap, context)
        reply_to ! RegisterNodeRef(id, actor_ref)
        Behaviors.same
      case ListingResponse(TopLevelServiceKey.Listing(listings)) =>
        //This should be only called by master as the master is only subscribed to these events
        if (listings.size == nr_of_cluster_nodes) {
          val leaf_nodes = all_tree_nodes.filter {
            case (id, node) =>
              node.tree_children.isEmpty
          }
          buildNodeStructureRandom(leaf_nodes, listings)
        } else {
          Behaviors.same
        }
      case StopTopLevel() =>
       Behaviors.stopped
    }
  }

  /** Deployment algorithm that decides node division */
  def buildNodeStructureRandom(nodes: Map[Int, TreeNode], topLevelActors: Set[ActorRef[SolutionEvent]]): Behavior[SolutionEvent] = {
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

  //TODO: test
  def buildNodeStructureWeight(nodes: Map[Int, TreeNode], topLevelActors: Set[ActorRef[SolutionEvent]]): Behavior[SolutionEvent] = {
    val weighted_map: Map[Int, (Int, TreeNode)] = nodes.map { case (key: Int, value: TreeNode) =>
      (key -> (calculateWeight(value, nodes, 1), value))
    }

    val actors = mutable.PriorityQueue.empty[(Int, ActorRef[SolutionEvent])](
      implicitly[Ordering[(Int, ActorRef[SolutionEvent])]].reverse
    )

    topLevelActors.foreach { actor => actors.enqueue((0, actor))}
    weighted_map.foreach { case (key, (weight, node)) =>
      val next_actor = actors.dequeue()
      actors.enqueue((next_actor._1 + weight, next_actor._2))
      spawnAndRegister(node, next_actor._2, context)
    }
    listenForRegister(nodes, topLevelActors, 1)
  }

  def calculateWeight(node: TreeNode, nodes: Map[Int, TreeNode], weight: Int): Int = {
    if (0 != node.parent) {
      calculateWeight(nodes(node.parent), nodes, weight+1)
    } else {
      weight
    }
  }

  def buildNodeStructureBranch(nodes: Map[Int, TreeNode], topLevelActors: Set[ActorRef[SolutionEvent]]): Behavior[SolutionEvent] = {
    //TODO: test
    var new_nodes: Map[Int, TreeNode] = nodes
    val all_nodes = nodes.values
    val leaf_nodes = all_nodes.filter { node => node.tree_children.isEmpty }

    val nodes_per_actor = (leaf_nodes.size + topLevelActors.size - 1) / topLevelActors.size

    val leaf_per_actor = leaf_nodes.grouped(nodes_per_actor)

    val branches_per_actor = topLevelActors.zip(
      leaf_per_actor.flatMap { leaves =>
        leaves.map { leaf =>
          val branch = getBranch(new_nodes, leaf, Map(leaf.id -> leaf))
          new_nodes = nodes.toSet.diff(branch.toSet).toMap
          branch
        }
      }
    )

    branches_per_actor.foreach { case (actor: ActorRef[SolutionEvent], branch: Map[Int, TreeNode]) =>
      branch.foreach(node => spawnAndRegister(node._2, actor, context))
    }

    listenForRegister(nodes, topLevelActors, 1)
  }

  def getBranch(nodes: Map[Int, TreeNode], current_node: TreeNode, branch: Map[Int, TreeNode]): Map[Int, TreeNode] = {
      if (nodes.contains(current_node.parent)) {
        val new_node = nodes(current_node.parent)
        getBranch(nodes, new_node, branch + (current_node.parent -> new_node))
      } else {
        branch
      }
  }

  //For every created node the master expects a register (Master only function)
  def listenForRegister(nodes: Map[Int, TreeNode], topLevelActors: Set[ActorRef[SolutionEvent]], depth: Int): Behavior[SolutionEvent] = {
    Behaviors.receiveMessage {
      case RegisterNodeRef(id: Int, ref: ActorRef[Node.Event]) =>
        context.log.info("Registering new ref: {} with id {}", ref, id)
        storedActorReferences += (id -> ref)
        if (storedActorReferences.size == all_tree_nodes.size) {
          startAlgorithm(storedActorReferences(1), topLevelActors)
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
          buildNodeStructureRandom(next_level_nodes, topLevelActors)
        } else {
          listenForRegister(nodes, topLevelActors, depth+1)
        }
    }
  }

  def spawnAndRegister(tree_node: TreeNode, topLevelActor: ActorRef[SolutionEvent], context: ActorContext[SolutionEvent]): Unit = {
    context.log.info("Creating node with id: {} and childeren {} at {}", tree_node.id, tree_node.tree_children, topLevelActor)
    if (context.self == topLevelActor) {
      val actor_ref = spawnNode(all_tree_nodes(tree_node.id), matchToActorRef(tree_node.id, tree_node.tree_children), context)
      context.self ! RegisterNodeRef(tree_node.id, actor_ref)
    } else {
      topLevelActor ! CreateNode(tree_node.id, matchToActorRef(tree_node.id, tree_node.tree_children).toList, context.self)
    }
  }

  def spawnNode(tree_node: TreeNode, children: Map[ActorRef[Node.Event], List[Int]], context: ActorContext[SolutionEvent]): ActorRef[Node.Event] = {
    context.log.info("Spawning actor for tree_node: " + tree_node.id.toString)
    context.spawn(Node(tree_node, children), tree_node.id.toString)
  }

  def matchToActorRef(tree_id: Int, children: List[Int]): Map[ActorRef[Node.Event], List[Int]] = {
    val tree_node: TreeNode = all_tree_nodes(tree_id)
    children.map { id =>
      val connected = tree_node.child_connected(id)
      (storedActorReferences(id) -> connected)
    }.toMap
  }

  def startAlgorithm(root_actor: ActorRef[Node.Event], topLevelActors: Set[ActorRef[SolutionEvent]]):  Behavior[SolutionNode.SolutionEvent] = {
    val start_time = Instant.now()

    //This part only has to run for one TopLevel in the distributed actorsystem
    context.log.info("Executing from master")
    //Start algorithm
    root_actor ! ReceiveSolution(Map(): Map[Int, String], context.self)

    //Wait for final solution
    Behaviors.receive { (ctx, message) =>
      message match {
        case SendSolution(solution: Map[Int, String], score) =>
          context.log.info("Final solution is: {} {}", solution, score)
          val finish_time = Instant.now()
          val duration = Duration.between(start_time, finish_time).toMillis/1000
          context.log.info("Execution took: {} seconds", duration)
          writeResults(solution, score, duration)

          //Terminate all still running tree nodes
          root_actor ! Terminate()
          topLevelActors.foreach(toplevelActor => toplevelActor ! StopTopLevel())

          Behaviors.stopped
        case _ =>
          context.log.error("Unexpected message: " + message)
          Behaviors.stopped
      }
    }
  }

  def writeResults(solution: Map[Int, String], score: Int, time: Long): Unit = {
    val file = new File("last_result.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("Duration: " + time + ", score: " + score + ", Solution: " + solution.toString())
    bw.close()
  }
}

private final case class CreateNode(id: Int, children: List[(ActorRef[Node.Event], List[Int])], reply_to: ActorRef[SolutionEvent]) extends SolutionEvent
private final case class RegisterNodeRef(id: Int, ref: ActorRef[Node.Event]) extends SolutionEvent
private final case class StopTopLevel() extends SolutionEvent
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

      topLevelActor.receive()
  }
}
