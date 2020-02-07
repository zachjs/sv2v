`define SIZE 4
`define NESTED_SIZE `SIZE
`define NAME op
`define FOO ha
`define BAR 'ha
`define MULTI 1, 2, 5
`define DULE dule
mo`DULE t`NAME;
    initial $display("%b", `SIZE'ha);
    initial $display("%b", `NESTED_SIZE'ha);
    initial $display("%b", 10'h`NESTED_SIZE);
    initial $display("%b", 10`BAR);
    initial $display("%b", 10`SIZE);
    initial $display("%b %b %b", `MULTI'ha);

    initial begin : block_name
        reg [4:0] foo;
        foo <= #1 `SIZE;
        $display("%b", foo);
        #2;
        $display("%b", foo);
    end
endmodule
