package PKG;
    typedef struct packed {
        int W;
    } F_t;
    localparam F_t F_v = '{
        W: 64
    };
endpackage

module M0 #(
    parameter type T,
    parameter ID = "NONE"
);
    T v;
    initial $display("%s %0d %0d", ID, $bits(T), $bits(v));
endmodule

module M1 #(parameter PKG::F_t F = PKG::F_v) ();
    typedef struct packed {
        logic [F.W-1:0] field;
    } local_t;
    local_t v;
    M0 #(.ID("A"), .T(local_t)) a();
    M0 #(.ID("B"), .T(PKG::F_t)) b();
endmodule

module top;
    typedef struct packed {
        byte W;
    } F_t;
    localparam F_t F = '{
        W: 16
    };
    typedef struct packed {
        logic [F.W-1:0] field;
    } local_t;
    local_t v;
    M1 m1();
    M0 #(.ID("C"), .T(local_t)) c();
    M0 #(.ID("D"), .T(PKG::F_t)) d();
    M0 #(.ID("E"), .T(F_t)) e();
endmodule
