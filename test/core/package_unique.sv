package P;
    typedef logic T;
endpackage

module top;
    P::T P_T;
    assign P_T = 0;
    initial $display("%b", P_T);
endmodule
