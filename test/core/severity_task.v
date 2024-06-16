module top;
    initial begin
        $display("Info [%0t] severity_task.sv:3:9 - top.<unnamed_block>", $time);
        $display("Info [%0t] severity_task.sv:4:9 - top.<unnamed_block> - ", $time, "%b", 1);
        $display("Warning [%0t] severity_task.sv:5:9 - top.<unnamed_block>", $time);
        $display("Warning [%0t] severity_task.sv:6:9 - top.<unnamed_block> - ", $time, "%b", 2);
        $display("Error [%0t] severity_task.sv:7:9 - top.<unnamed_block>", $time);
        $display("Error [%0t] severity_task.sv:8:9 - top.<unnamed_block> - ", $time, "%b", 3);
        $display("Fatal [%0t] severity_task.sv:9:9 - top.<unnamed_block>", $time);
        $finish;
        $display("Fatal [%0t] severity_task.sv:10:9 - top.<unnamed_block>", $time);
        $finish(0);
        $display("Fatal [%0t] severity_task.sv:11:9 - top.<unnamed_block> - ", $time, "%b", 4);
        $finish(1);
    end
endmodule
