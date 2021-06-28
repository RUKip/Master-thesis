package com.example

import akka.actor.{Address, AddressFromURIString}
import akka.actor.typed.ActorSystem
import akka.cluster.typed.{Cluster, JoinSeedNodes}
import com.example.actors.{NodeSearch, TopLevel}
import com.typesafe.config.ConfigFactory
import jdk.nashorn.internal.runtime.ScriptingFunctions.exec

object Main extends App {
//
//    //Init graph and tree
//    val graph: Map[Int, Variable] = InitializationHelper.initAGraph()
//
//    //Calculate graph tree-decomposition
//    val (root_node, tree_decomposition): (TreeNode, Map[Int, TreeNode]) = InitializationHelper.getHTD(graph)

      val base = InitializationHelper.loadTree("generated_trees/1_generated_tree.json")
      val tree_decomposition = InitializationHelper.createUsableTree(base)

      val hostname = System.getProperty("hostname")
      val current_hostname = exec("hostname")

      if (hostname == current_hostname) {
        startup("master", 25252, tree_decomposition)
      } else {
        startup("worker", 25252, tree_decomposition)
      }

      //Divide here the nodes over the cluster based on tree-decomposition
      def startup(role: String, port: Int, tree_nodes: Map[Int, TreeNode]): Unit = {
        // Override the configuration of the port
        val config = ConfigFactory.parseString(
          s"""
      akka.remote.artery.canonical.port=$port
      akka.cluster.roles = [$role]
      """).withFallback(ConfigFactory.load())

        val nodes = System.getProperty("hostname")

        // Create an Akka system
        val system: ActorSystem[NodeSearch.Event] = ActorSystem(TopLevel(tree_nodes, nodes.toInt), name = "COPSolver", config = config)

        val list: List[Address] = List(System.getProperty("hostname")).map(AddressFromURIString.parse)
        Cluster(system).manager ! JoinSeedNodes(list)
      }
}
