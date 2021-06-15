module foo #(
    parameter type T = logic,
    parameter size = 0
);
    generate
        if (size != 0) begin : foo
            bar #(T, size - 1) x();
        end
    endgenerate
    initial $display("foo %d %d", $bits(T), size);
endmodule

module bar #(
    parameter type U = logic,
    parameter size = 0
);
    generate
        if (size != 0) begin : bar
            foo #(U, size - 1) x();
        end
    endgenerate
    initial $display("bar %d %d", $bits(U), size);
endmodule

module top_1; foo #(byte, 2) x(); endmodule
module top_2; bar #(byte, 3) x(); endmodule

module top_3; foo #(bit, 4) x(); endmodule
module top_4; bar #(bit, 5) x(); endmodule

module top; foo x(); endmodule
