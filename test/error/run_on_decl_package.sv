// pattern: run_on_decl_package\.sv:3:16: Parse error: unexpected token in declaration
package P;
    integer x, byte y;
    localparam Z = 1;
endpackage

module top;
    initial $display(P::Z);
endmodule
