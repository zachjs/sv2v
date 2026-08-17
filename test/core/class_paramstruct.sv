package P;
    typedef struct packed { int unsigned a; int unsigned b; } cfg_t;
    localparam cfg_t cfg = '{a: 8, b: 4};
    typedef enum logic [1:0] { S_A, S_B } enum_e;
endpackage

class C #(parameter P::cfg_t cfg = P::cfg);
    typedef logic [$clog2(cfg.a) - 1:0] x_t;
    typedef logic [cfg.b - 1:0] y_t;
    typedef struct packed { P::enum_e e; x_t x; y_t y; } s_t;
endclass

module child #(
    parameter P::cfg_t cfg = P::cfg,
    parameter type s_t = C#(cfg)::s_t,
    localparam type x_t = C#(cfg)::x_t
) (
    input s_t ins,
    output x_t out
);
    always_comb
        unique case (ins.e)
            P::S_A: out = ins.x;
            default: out = '0;
        endcase
endmodule
