module top;
    reg [16:0] inp;
    wire [15:0] data;
    wire valid;
    assign data = inp[16:1];
    assign valid = inp[0];
    initial inp = 17'b1_01100011_11001111;
endmodule
