module Module;
    parameter S = 0;
    parameter T = 0;
    wire [T-1:0] x;
    generate
        if (S) begin : a
            if (S) begin : b
                assign Module.x = 1'sb1;
                wire [T:0] y = 1'sbz;
            end
        end
    endgenerate
    initial $display("Module %0d: %b %0d %b %0d", S, Module.x, T, Module.a.b.y, T + 1);
endmodule

module top;
    parameter ONE = 1;
    wire [ONE*7:ONE*0] x;
    if (1) begin : blk
        localparam W = ONE * 3;
        wire [W-1:$bits(x)] x;
    end
    Module #(1, 8) m1();
    Module #(2, 7) m2();
    Module #(3, 3) m3();
endmodule
