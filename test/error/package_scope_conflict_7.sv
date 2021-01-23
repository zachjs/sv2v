// pattern: import of Q::X conflicts with prior import of P::X
package P;
    localparam X = 1;
endpackage
package Q;
    localparam X = 2;
endpackage
module top;
    import P::*;
    if (1) begin : blk1
        // forces import of P::X at the top level
        initial $display(X);
    end
    import Q::X; // illegal
endmodule
