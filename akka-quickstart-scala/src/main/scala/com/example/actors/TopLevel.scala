package com.example.actors

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.cluster.typed.Cluster
import com.example.TreeNode
import com.example.actors.Node.{ReceiveSolution, Terminate}
import com.example.actors.SolutionNode.{SendSolution, SolutionEvent}
import com.example.actors.TopLevel.{storedActorReferences, topLevelServiceKey}
import com.example.deployment.{BranchDeployment, RandomDeployment, WeightDeployment}

import java.io.{BufferedWriter, File, FileWriter}
import java.time.format.{DateTimeFormatter, FormatStyle}
import java.time.{Duration, Instant, ZoneId}
import java.util.Locale

class TopLevel (val context: ActorContext[SolutionEvent], val all_tree_nodes: Map[Int, TreeNode], val nr_of_cluster_nodes: Int, deployment_type: String) {

  def receive(): Behavior[SolutionNode.SolutionEvent] ={
    Behaviors.receiveMessage {
      case CreateNode(id: Int, children: List[(ActorRef[Node.Event], List[Int])], reply_to: ActorRef[SolutionEvent]) =>
//        context.log.info("Creating node: {}, send reply back to: {}", id, reply_to)
        val actor_ref = spawnNode(all_tree_nodes(id), children.toMap, context)
        reply_to ! RegisterNodeRef(id, actor_ref)
        Behaviors.same
      case ListingResponse(topLevelServiceKey.Listing(listings)) =>
        //This should be only called by master as the master is only subscribed to these events
        if (listings.size == nr_of_cluster_nodes) {
//          context.log.info("Trying to deploy nodes {} to listings {}", all_tree_nodes.keys, listings)
          startDeploy(all_tree_nodes, listings)
        } else {
          Behaviors.same
        }
      case StopTopLevel() =>
       Behaviors.stopped
    }
  }

  //For every created node the master expects a register (Master only function)
  def listenForRegister(nodes: Map[Int, TreeNode], topLevelActors: Set[ActorRef[SolutionEvent]], division: Map[Int, ActorRef[SolutionNode.SolutionEvent]], depth: Int): Behavior[SolutionEvent] = {
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
            case (id, node) => (!storedActorReferences.contains(id)) && node.tree_children.forall(child => storedActorReferences.contains(child))
          }
          context.log.info("Next level nodes are: {}", next_level_nodes.keys)
          deploy(next_level_nodes, topLevelActors, division, 1)
        } else {
          //Not everything is registered yet so wait
          listenForRegister(nodes, topLevelActors, division, depth+1)
        }
    }
  }

  def startDeploy(nodes: Map[Int, TreeNode], actors: Set[ActorRef[SolutionEvent]]): Behavior[SolutionEvent] ={
    val division: Map[Int, ActorRef[SolutionNode.SolutionEvent]] = deployment_type match {
      case "random" => RandomDeployment().deploy(nodes, actors)
      case "weight" => WeightDeployment().deploy(nodes, actors)
      case "branch" => BranchDeployment().deploy(nodes, actors)
      case _ =>
        context.log.error("Unknown value for deployment argument")
        Map()
    }
    if (division.isEmpty) {
      Behaviors.stopped
    } else {
      val leaf_nodes = all_tree_nodes.filter(_._2.isLeaf)
      deploy(leaf_nodes, actors, division, 1)
    }
  }

  def deploy(nodes: Map[Int, TreeNode], cluster_refs: Set[ActorRef[SolutionEvent]], division: Map[Int, ActorRef[SolutionNode.SolutionEvent]], depth: Int): Behavior[SolutionEvent] = {
    nodes.foreach { case (id: Int, node: TreeNode) =>
      val cluster_ref = division(id)
      spawnAndRegister(node, cluster_ref, context)
    }
    listenForRegister(nodes, cluster_refs, division, depth)
  }

  /** Send message to topLevel nodes to create node, called only by master */
  def spawnAndRegister(tree_node: TreeNode, topLevelActor: ActorRef[SolutionEvent], context: ActorContext[SolutionEvent]): Unit = {
//    context.log.info("Creating node with id: {} and childeren {} at {}", tree_node.id, tree_node.tree_children, topLevelActor)
    if (context.self == topLevelActor) {
      val actor_ref = spawnNode(all_tree_nodes(tree_node.id), matchToActorRef(tree_node.id, tree_node.tree_children), context)
      context.self ! RegisterNodeRef(tree_node.id, actor_ref)
    } else {
      topLevelActor ! CreateNode(tree_node.id, matchToActorRef(tree_node.id, tree_node.tree_children).toList, context.self)
    }
  }

  /** Spawn node */
  def spawnNode(tree_node: TreeNode, children: Map[ActorRef[Node.Event], List[Int]], context: ActorContext[SolutionEvent]): ActorRef[Node.Event] = {
    //context.log.info("Spawning actor for tree_node: " + tree_node.id.toString)
    context.spawn(Node(tree_node, children), tree_node.id.toString)
  }

  /** Create for a tree node, its children with mapping of its location (actoref) (depends on actoref of children)*/
  def matchToActorRef(tree_id: Int, children: List[Int]): Map[ActorRef[Node.Event], List[Int]] = {
    val tree_node: TreeNode = all_tree_nodes(tree_id)
    children.map { id =>
      val connected = tree_node.child_connected(id)
      (storedActorReferences(id) -> connected)
    }.toMap
  }

  def startAlgorithm(root_actor: ActorRef[Node.Event], topLevelActors: Set[ActorRef[SolutionEvent]]):  Behavior[SolutionNode.SolutionEvent] = {
    context.log.info("Starting algorithm")
    val start_time = Instant.now()

    //This part only has to run for one TopLevel in the distributed actorsystem
    //context.log.info("Executing from master")
    //Start algorithm
    root_actor ! ReceiveSolution(Map(): Map[Int, String], context.self)

    //Wait for final solution
    Behaviors.receive { (ctx, message) =>
      message match {
        case SendSolution(solution: Map[Int, String], score) =>
          //context.log.info("Final solution is: {} {}", solution, score)
          val finish_time = Instant.now()
          val duration = Duration.between(start_time, finish_time).toMillis/1000
          //context.log.info("Execution took: {} seconds", duration)
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
    val time_stamp = Instant.now()
    val processors_used = Runtime.getRuntime.availableProcessors()
    val formatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("yyyy-MM-dd_hh-mm-ss")
        .withLocale( Locale.UK )
        .withZone( ZoneId.systemDefault() )
    val file = new File("/home/s2756781/last_result_" + formatter.format(time_stamp) + ".txt")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("Duration: " + time
      + ", score: " + score
      + ", Nr of cluster nodes: " + nr_of_cluster_nodes
      + ", Deployement type: " + deployment_type
      + ", Solution: " + solution.toString()
    )
    bw.close()
  }
}

private final case class CreateNode(id: Int, children: List[(ActorRef[Node.Event], List[Int])], reply_to: ActorRef[SolutionEvent]) extends SolutionEvent
private final case class RegisterNodeRef(id: Int, ref: ActorRef[Node.Event]) extends SolutionEvent
private final case class StopTopLevel() extends SolutionEvent
final case class ListingResponse(listing: Receptionist.Listing) extends SolutionEvent

object TopLevel {

  val topLevelServiceKey: ServiceKey[SolutionEvent] = ServiceKey[SolutionEvent]("TopLevel")
  var storedActorReferences: Map[Int, ActorRef[Node.Event]] = Map()

  def apply(tree_nodes: Map[Int, TreeNode], nr_of_cluster_nodes: Int, deployment_type: String): Behavior[SolutionNode.SolutionEvent] =
    Behaviors.setup[SolutionNode.SolutionEvent] { context =>

      //context.log.info("starting toplevel actor")

      val listingAdapter: ActorRef[Receptionist.Listing] =
        context.messageAdapter { listing => ListingResponse(listing)}

      context.system.receptionist ! Receptionist
        .Register(topLevelServiceKey, context.self)

      val topLevelActor = new TopLevel(context, tree_nodes, nr_of_cluster_nodes, deployment_type)

      if (Cluster(context.system).selfMember.hasRole("master")) {
        context.system.receptionist ! Receptionist.Subscribe(topLevelServiceKey, listingAdapter)
      }

      topLevelActor.receive()
  }
}
