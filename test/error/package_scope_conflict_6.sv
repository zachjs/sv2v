// pattern: import of Q::X conflicts with prior import of P::X
package P;
    localparam X = 1;
endpackage
package Q;
    localparam X = 2;
endpackage
package W;
    import P::*;
    export P::*;
    task help;
        $display("W::help() %0d", X);
    endtask
    import Q::X;
endpackage
module top;
    import W::*;
    initial $display(X);
endmodule
