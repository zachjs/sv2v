module example(
    input wire [7:0] inp,
    output wire [7:0] out
);
    assign out = ~inp;
endmodule

module top;
    reg [7:0] arr1 [1:0];
    reg [7:0] arr2 [1:0][1:0];
    wire [7:0] out1, out2;
    example e1(arr1[0], out1);
    example e2(arr2[0][0], out2);
    initial begin : blk
        integer i;
        for (i = 0; i < 8; i = i + 1) begin
            #1 arr1[0][i] = (8'hAD >> i) & 1'b1;
            #1 arr2[0][0][i] = (8'h42 >> i) & 1'b1;
        end
    end
endmodule
