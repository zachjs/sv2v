package P;
    typedef logic T;
    typedef logic [1:0] U;
endpackage

module top;
    P::T P_T;
    assign P_T = 0;
    initial $display("%b", P_T);
    P::U P_U = 0;
endmodule
