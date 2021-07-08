`define MACRO_A(x, y) \
initial begin \
    $display(`"x %b`", x); \
    $display(`"y %b`", y); \
end

`define MACRO_B(x, y, z) initial $display(x, y, z);

module top;
    `MACRO_A(1 + 2, 1'b1 & 1)
    `MACRO_B(
        "/* not a block comment */",
        "// not a line comment",
        "cool \046 \042 ( } { beans\\"
    )
endmodule
