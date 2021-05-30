// pattern: procedural block contains a declaration after a statement
module top;
    task t;
        $display("t()");
    endtask
    initial begin
        t;
        localparam X = 3;
    end
endmodule
