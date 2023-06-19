// pattern: can't determine the type of x\[1\] because the inner type reg can't be indexed
// location: typeof_atom_bit.sv:5:13
module top;
    reg x;
    initial $display($bits(x[1]));
endmodule
