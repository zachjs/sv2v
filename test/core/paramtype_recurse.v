module foo_byte #(
    parameter size = 0
);
    generate
        if (size != 0) begin : foo
            bar_byte #(size - 1) x();
        end
    endgenerate
    initial $display("foo %d %d", 8, size);
endmodule

module bar_byte #(
    parameter size = 0
);
    generate
        if (size != 0) begin : bar
            foo_byte #(size - 1) x();
        end
    endgenerate
    initial $display("bar %d %d", 8, size);
endmodule

module foo_bit #(
    parameter size = 0
);
    generate
        if (size != 0) begin : foo
            bar_bit #(size - 1) x();
        end
    endgenerate
    initial $display("foo %d %d", 1, size);
endmodule

module bar_bit #(
    parameter size = 0
);
    generate
        if (size != 0) begin : bar
            foo_bit #(size - 1) x();
        end
    endgenerate
    initial $display("bar %d %d", 1, size);
endmodule

module top_1; foo_byte #(2) x(); endmodule
module top_2; bar_byte #(3) x(); endmodule

module top_3; foo_bit #(4) x(); endmodule
module top_4; bar_bit #(5) x(); endmodule

module top; foo_bit #(0) x(); endmodule
