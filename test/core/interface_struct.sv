typedef struct packed {
    logic c, d;
} T;
typedef struct packed {
    logic a, b;
    T y;
} S;

interface I;
    S x;
    type(x.y) w;
    modport D(input x);
endinterface

module M(m);
    I.D m;
    initial begin
        $display("%b", m.x.a);
        $display("%b", m.x.b);
        $display("%b", m.x.y.c);
        $display("%b", m.x.y.d);
    end
endmodule

module top;
    I i();
    M m(i);
    assign i.x = 4'b1001;
    initial i.w = 2'b10;
endmodule
