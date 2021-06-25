module top;
    localparam [2:0] AccessAck = 3'd0;
    wire [2:0] test;

    always @(*) begin
        case (test)
            AccessAck: $display("Ack");
            default  : $display("default");
        endcase
    end
endmodule
