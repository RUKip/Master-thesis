package com.example.solver;


import com.example.TreeNode;

import java.util.HashMap;

public class Solver {

    public String solve(TreeNode node)
    {
        HashMap<String, Integer> color_options = new HashMap<>();
        color_options.put("red", 1);
        color_options.put("blue", 2);
        color_options.put("yellow", 3);

        Model model = new Model("Graph coloring problem");

        //Create variable to calculate (the color of the node)
        SetVar node_color =  model.setVar("Color_"+node.id(), color_options.values());

        //Create constraints
        int i=0;
        for (TreeNode connected_node : node.children()) {
            model.arithm(node_color, "!=",connected_node.color()).post();
            i++;
        }

        //Solve
        Solution solution = model.getSolver().findSolution();
        if(solution != null){
            System.out.println(solution.toString());
        }

    }
}
