module child(
    input wire [8:0] inp,
    output reg [2:0] out
);
    always @* begin
        case (inp[8:7])
            2'd0: out = inp[6:4];
            default: out = 3'd0;
        endcase
    end
endmodule
