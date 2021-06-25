package foo_pkg;
    typedef enum logic [2:0] {
        AccessAck     = 3'd0,
        AccessAckData = 3'd1
    } inp_t;
endpackage

module top;
    import foo_pkg::*;
    wire [2:0] test;

    always_comb begin
        case (test)
            AccessAck: $display("Ack");
            default  : $display("default");
        endcase
    end
endmodule
