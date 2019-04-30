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
    export D::*;
endpackage
package F;
    localparam MAGIC = -42;
    localparam PIZZAZZ = -5;
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
    import F::*;
    initial begin
        $display("imported MAGIC %d", MAGIC);
        $display("imported MAGIC %d", F::MAGIC);
        begin
            localparam MAGIC = 42;
            $display("local MAGIC %d", MAGIC);
            $display("imported MAGIC %d", F::MAGIC);
        end
        $display("imported MAGIC %d", MAGIC);
        $display("imported MAGIC %d", F::MAGIC);
    end
    localparam PIZZAZZ = -6;
    initial begin
        $display("local PIZZAZZ %d", PIZZAZZ);
        $display("imported PIZZAZZ %d", F::PIZZAZZ);
        begin
            localparam PIZZAZZ = -7;
            $display("shadowed local PIZZAZZ %d", PIZZAZZ);
            $display("imported PIZZAZZ %d", F::PIZZAZZ);
        end
        $display("local PIZZAZZ %d", PIZZAZZ);
        $display("imported PIZZAZZ %d", F::PIZZAZZ);
    end
endmodule
