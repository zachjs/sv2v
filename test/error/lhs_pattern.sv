// pattern: cannot convert expression to LHS
module top;
    logic x, y, z;
    assign {<< {x, '{y:y, z:z}}} = 3'b101;
endmodule
