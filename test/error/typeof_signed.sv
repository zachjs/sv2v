// pattern: unexpected signed applied to type\(y\)
module top;
    logic y;
    var type(y) signed x;
endmodule
