//#full-example
package com.example

import akka.actor.typed.ActorSystem
import com.example.actors.Node

object AkkaQuickstart extends App {
//  //#actor-system
//  val greeterMain: ActorSystem[GreeterMain.SayHello] = ActorSystem(GreeterMain(), "AkkaQuickStart")
//  //#actor-system
//
//  //#main-send-messages
//  greeterMain ! SayHello("Charles")
//  //#main-send-messages

    val tree: TreeNode = TreeHelper.initATree()

    val system: ActorSystem[Node.PrintGraph] = ActorSystem(Node(tree), "AkkaQuickStart")

    system ! Node.PrintGraph()

}
