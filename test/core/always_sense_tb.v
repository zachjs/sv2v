module top;
    reg inp1, inp2;
    wire out1, out2, out3, out4, out5, out6, out7, out8, out9, outA, outB;
    mod m(inp1, inp2, out1, out2, out3, out4, out5, out6, out7, out8, out9, outA, outB);
    initial begin
        $monitor(inp1, inp2,
            out1, out2, out3, out4, out5, out6, out7, out8, out9, outA, outB);
        repeat (2) begin
            #1 inp1 = 0;
            #1 inp2 = 0;
            #1 inp2 = 1;

            #1 inp1 = 1;
            #1 inp2 = 0;
            #1 inp2 = 1;

            #1 inp2 = 0;
            #1 inp1 = 0;
            #1 inp1 = 1;

            #1 inp2 = 1;
            #1 inp1 = 0;
            #1 inp1 = 1;
        end
    end
endmodule
