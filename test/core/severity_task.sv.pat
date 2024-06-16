affirm $finish;
affirm $display("Fatal [%0t] severity_task.sv:10:9 - top.<unnamed_block>", $time);
affirm $finish(0);
affirm $display("Fatal [%0t] severity_task.sv:11:9 - top.<unnamed_block> - ", $time, "%b", 4);
affirm $finish(1);
