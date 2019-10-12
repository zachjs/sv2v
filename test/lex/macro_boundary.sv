`define SIZE 4
`define NESTED_SIZE `SIZE
`define NAME op
module t`NAME;
    initial $display(`SIZE'ha);
    initial $display(`NESTED_SIZE'ha);
endmodule
