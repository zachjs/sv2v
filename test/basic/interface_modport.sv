interface Interface #(parameter WIDTH = 4) (
    input clock,
    output [$clog2(WIDTH) - 1:0] indices [2]
);
    logic [2*WIDTH-1:0] x;
    modport ModportA(
        input clock,
        output indices,
        input .x(x[2*WIDTH-1:WIDTH]), .y(x[WIDTH-1:0])
    );
    modport ModportB(
        input clock,
        output .x(x)
    );
endinterface

module ModuleA(Interface.ModportA m);
    assign m.indices[0] = $clog2(m.x);
    assign m.indices[1] = $clog2(m.y);
endmodule

module ModuleB(Interface.ModportB m);
    initial m.x = 1;
    localparam WIDTH = 2 * m.WIDTH;
    always @(posedge m.clock) begin
        logic temp;
        temp = m.x[WIDTH-1];
        for (integer i = WIDTH-1; i > 0; --i) begin
            m.x[i] = m.x[i-1];
        end
        m.x[0] = temp;
    end
endmodule

module ModuleBWrapper(Interface.ModportB m);
    ModuleB b(m);
endmodule

module ModuleAWrapper(Interface.ModportA m);
    ModuleA a(m);
endmodule

module Tester(input clock);
    parameter WIDTH = 1;
    logic [WIDTH-1:0] idx1, idx2;
    Interface #(2 ** WIDTH) i(clock, '{idx1, idx2});
    ModuleAWrapper a(i);
    ModuleBWrapper b(i);
    always @(negedge clock)
        $display("%d %0d %2d %2d %b", $time, WIDTH, idx1, idx2, i.x);
endmodule
