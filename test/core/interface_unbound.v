module Module(x);
    input wire x;
    initial $display("Hello!");
    initial #1 $display(x);
endmodule

module top;
    generate
        if (1) begin
            wire x;
            Module m1(x);
            Module m2(x);
        end
    endgenerate
endmodule
