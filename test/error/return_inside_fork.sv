// pattern: encountered return inside fork-join
module top;
    task t;
        fork
            return;
        join
    endtask
    initial t;
endmodule
