module top;
    initial $display("Info [elaboration] elab_task.sv:2:5 - top");
    initial $display("Info [elaboration] elab_task.sv:3:5 - top - %b", 1);
    initial $display("Warning [elaboration] elab_task.sv:4:5 - top");
    initial $display("Warning [elaboration] elab_task.sv:5:5 - top - %b", 2);
    initial $display("Error [elaboration] elab_task.sv:6:5 - top");
    initial $display("Error [elaboration] elab_task.sv:7:5 - top - %b", 3);
    initial begin
        $display("Fatal [elaboration] elab_task.sv:8:5 - top");
        $finish;
    end
    initial begin
        $display("Fatal [elaboration] elab_task.sv:9:5 - top");
        $finish(0);
    end
    initial begin
        $display("Fatal [elaboration] elab_task.sv:10:5 - top - %b", 4);
        $finish(1);
    end
endmodule
