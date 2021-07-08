`define MACRO_A(
    // comment
    	/* intentional tab */
    /* comment */ x /* comment */
    	/* intentional tab */
    // comment
    ,
    // comment
    	/* intentional tab */
    /* comment */ y /* comment */
    	/* intentional tab */
    // comment
) \
initial begin \
    $display(`"x %b`", x); \
    $display(`"y %b`", y); \
end

`define MACRO_B(x, y, z) initial $display(x, y, z);

module top;
    `MACRO_A(
    // comment
    	/* intentional tab */
        /* comment */ 1 + 2 /* comment */
    	/* intentional tab */
    // comment
        ,
    	/* intentional tab */
        /* comment */ 1'b1 & 1 /* comment */
    	/* intentional tab */
    // comment
    )
    `MACRO_B(
        "/* not a block comment */",
        "// not a line comment",
        "cool \046 \" ( } { beans\\"
    )
endmodule
