// pattern: cannot convert expression to LHS
// location: asgn_expr_non_lhs.sv:5:20
module top;
    integer x;
    initial x = (1 = x);
endmodule
