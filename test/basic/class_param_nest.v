`define DUMP(expr, value) $display(`"expr = %0d`", value);
`define DUMP_BOTH(prefix, value) `DUMP(prefix::X, value) `DUMP(prefix::Y, (value) * 2)

module top;
    localparam Z = 3;
    initial begin
        `DUMP(Z, Z)
        `DUMP(P::W, 5)
        `DUMP_BOTH(C#(), 1)
        `DUMP_BOTH(C#(Z), Z)
        `DUMP_BOTH(C#(P::W), 5)
        `DUMP_BOTH(C#(C#(Z)::Y), Z * 2)
    end
endmodule
