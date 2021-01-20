// pattern: cannot convert expression to LHS
module top;
    logic x;
    assign {<< {x, 2'b00}} = 3'b101;
endmodule
