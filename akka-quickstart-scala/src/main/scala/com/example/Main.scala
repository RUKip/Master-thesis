package com.example

import akka.actor.{Address, AddressFromURIString}
import akka.actor.typed.ActorSystem
import akka.cluster.typed.{Cluster, Join, JoinSeedNodes}
import com.example.actors.{NodeSearch, TopLevel}
import com.typesafe.config.ConfigFactory

import java.net.InetAddress

object Main extends App {
//
//    //Init graph and tree
//    val graph: Map[Int, Variable] = InitializationHelper.initAGraph()
//
//    //Calculate graph tree-decomposition
//    val (root_node, tree_decomposition): (TreeNode, Map[Int, TreeNode]) = InitializationHelper.getHTD(graph)

      val loaded_tree: String = System.getProperty("tree")

      val base = InitializationHelper.loadTree(loaded_tree)
      val tree_decomposition = InitializationHelper.createUsableTree(base)

      val hostname = System.getProperty("hostname")
      val current_hostname = InetAddress.getLocalHost.getHostAddress

      if (hostname == current_hostname) {
        println("Starting master")
        startup("master", 2552, tree_decomposition)
      } else {
        println("Starting worker")
        startup("worker", 2552, tree_decomposition)
      }

      //Divide here the nodes over the cluster based on tree-decomposition
      def startup(role: String, port: Int, tree_nodes: Map[Int, TreeNode]): Unit = {
        // Override the configuration of the port
        val config = ConfigFactory.parseString(
          s"""
      akka.remote.artery.canonical.port=$port
      akka.cluster.roles = [$role]
      """).withFallback(ConfigFactory.load())

        val nodes = System.getProperty("nodes")
        val deployment_type = System.getProperty("deployment")

        // Create an Akka system
        val system: ActorSystem[NodeSearch.Event] = ActorSystem(TopLevel(tree_nodes, nodes.toInt, deployment_type), name = "COPSolver", config = config)

        val seed_node = System.getProperty("akka.cluster.seed-nodes.0")
        val list: List[Address] = List(seed_node).map(AddressFromURIString.parse)
        Cluster(system).manager ! JoinSeedNodes(list)
      }
}
