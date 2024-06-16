affirm $finish;
affirm $display("Fatal [elaboration] elab_task.sv:9:5 - top");
affirm $finish(0);
affirm $display("Fatal [elaboration] elab_task.sv:10:5 - top - ", "%b", 4);
affirm $finish(1);
