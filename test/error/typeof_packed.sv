// pattern: unexpected packed range\(s\) applied to type\(y\)
module top;
    logic y;
    var type(y) [1:0] x;
endmodule
