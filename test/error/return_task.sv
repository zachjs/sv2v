// pattern: non-void return inside task or void function
module top;
    task t;
        return 1;
    endtask
    initial t;
endmodule
