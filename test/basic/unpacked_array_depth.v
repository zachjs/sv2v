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
    initial begin
        #1 arr1[0] = 8'hAD;
        #1 arr2[0][0] = 8'h42;
    end
endmodule
