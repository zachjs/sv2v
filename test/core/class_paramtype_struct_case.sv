package p;
    typedef struct packed { int unsigned a; int unsigned b; } cfg_t;
    localparam cfg_t Param = '{a: 8, b: 4};
    typedef enum logic [1:0] { S_A, S_B } enum_e;
endpackage

class C #(parameter p::cfg_t Cfg = p::Param);
    typedef logic [$clog2(Cfg.a)-1:0] x_t;
    typedef logic [Cfg.b-1:0]         y_t;
    typedef struct packed { p::enum_e e; x_t x; y_t y; } s_t;
endclass

module dut #(
    parameter  p::cfg_t Cfg = p::Param,
    parameter  type     s_t = C#(Cfg)::s_t,
    localparam type     x_t = C#(Cfg)::x_t
) (
    input  s_t ins,
    output x_t o
);
    always_comb
        unique case (ins.e)
            p::S_A:  o = ins.x;
            default: o = '0;
        endcase
endmodule

module top;
    logic [8:0] ins;
    logic [2:0] o;
    dut d (.ins(ins), .o(o));
    initial begin
        ins = 9'b00_101_0000; #1; $display("%0d", o);
        ins = 9'b00_010_1111; #1; $display("%0d", o);
        ins = 9'b01_111_0000; #1; $display("%0d", o);
    end
endmodule
