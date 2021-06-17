package com.example

import akka.actor.typed.ActorSystem
import com.example.actors.{NodeSearch, TopLevel}
import com.typesafe.config.ConfigFactory

object COPSolver extends App {

    //Init graph and tree
    val graph: Map[Int, Variable] = InitializationHelper.initAGraph()

    //Calculate graph tree-decomposition
    val (root_node, tree_decomposition): (TreeNode, Map[Int, TreeNode]) = InitializationHelper.getHTD(graph)

//    val base = InitializationHelper.loadTree("generated_trees/1_generated_tree.json")
//    val tree_decomposition = InitializationHelper.createUsableTree(base)

    //This matches with the config file
    val ports = Seq(25251, 25252)

    val master_port = ports.head
    startup("master", master_port, tree_decomposition)
    ports.tail.foreach(port => startup("worker", port, tree_decomposition))

  //Divide here the nodes over the cluster based on tree-decomposition
    def startup(role: String, port: Int, tree_nodes: Map[Int, TreeNode]): Unit = {
        // Override the configuration of the port
        val config = ConfigFactory.parseString(s"""
      akka.remote.artery.canonical.port=$port
      akka.cluster.roles = [$role]
      """).withFallback(ConfigFactory.load())

        // Create an Akka system
        val system: ActorSystem[NodeSearch.Event] = ActorSystem(TopLevel(tree_nodes, ports.size), name= "COPSolver", config= config)
    }
}
