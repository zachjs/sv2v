module child (
    input  wire [8:0] ins_i,
    output reg  [2:0] o
);
    always @* begin
        case (ins_i[8:7])
            2'd0:    o = ins_i[6:4];
            default: o = 3'd0;
        endcase
    end
endmodule

module top;
    reg  [8:0] raw;
    wire [2:0] o;
    child dut (.ins_i(raw), .o(o));
    initial begin
        raw = 9'b00_101_0000; #1; $display("%0d", o);
        raw = 9'b00_010_1111; #1; $display("%0d", o);
        raw = 9'b01_111_0000; #1; $display("%0d", o);
    end
endmodule
