// pattern: can't determine the type of x\[1:0\] because the inner type reg can't be indexed
// location: typeof_atom_range.sv:5:5
module top;
    reg x;
    var type(x[1:0]) y;
    initial $display(y);
endmodule
