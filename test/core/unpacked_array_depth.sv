module example(
    input wire inp [7:0],
    output wire out [7:0]
);
    for (genvar i = 0; i < 8; ++i)
        assign out[i] = ~inp[i];
endmodule

module top;
    reg arr1 [1:0][7:0];
    reg arr2 [1:0][1:0][7:0];
    wire out1 [7:0];
    wire out2 [7:0];
    example e1(arr1[0], out1);
    example e2(arr2[0][0], out2);
    initial begin
        for (integer i = 0; i < 8; ++i) begin
            #1 arr1[0][i] = (8'hAD >> i) & 1'b1;
            #1 arr2[0][0][i] = (8'h42 >> i) & 1'b1;
        end
    end
endmodule
