package A;
    localparam FOO = 37;
    localparam BAR = 97;
endpackage
package B;
    localparam FOO = -37;
    localparam BAR = -97;
endpackage
package C;
    typedef logic [3:0] pack_t;
endpackage
package D;
    function C::pack_t pack;
        input logic x;
        pack = {$bits(C::pack_t){x}};
    endfunction
endpackage
package E;
    import D::*;
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
        $display("%d", D::pack(0));
        $display("%d", D::pack(1));
        $display("%d", E::pack(0));
        $display("%d", E::pack(1));
    end
endmodule
