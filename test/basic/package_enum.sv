package foo_pkg;
    typedef enum logic [2:0] {
        AccessAck     = 3'd0,
        AccessAckData = 3'd1
    } inp_t;
endpackage

module top;
    foo_pkg::inp_t test;
    assign test = foo_pkg::AccessAck;
    initial $display(test);
endmodule
