class C #(
    parameter X = 1
);
    localparam Y = X * 2;
endclass

package P;
    localparam W = 5;
endpackage

`define DUMP(expr) $display(`"expr = %0d`", expr);
`define DUMP_BOTH(prefix) `DUMP(prefix::X) `DUMP(prefix::Y)

module top;
    localparam Z = 3;
    initial begin
        `DUMP(Z)
        `DUMP(P::W)
        `DUMP_BOTH(C#())
        `DUMP_BOTH(C#(Z))
        `DUMP_BOTH(C#(P::W))
        `DUMP_BOTH(C#(C#(Z)::Y))
    end
endmodule
