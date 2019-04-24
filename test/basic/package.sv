package A;
    localparam FOO = 37;
    localparam BAR = 97;
endpackage
package B;
    localparam FOO = -37;
    localparam BAR = -97;
endpackage
module top;
    import A::FOO;
    import B::BAR;
    initial begin
        $display(A::FOO);
        $display(A::BAR);
        $display(B::FOO);
        $display(B::BAR);
        $display(FOO);
        $display(BAR);
    end
endmodule
