module top;
    localparam [2:0] AccessAck = 3'd0;
    wire [2:0] test;

    reg start;
    always @(*) begin
        if (start);
        case (test)
            AccessAck: $display("Ack");
            default  : $display("default");
        endcase
    end
    initial start = 0;
endmodule
