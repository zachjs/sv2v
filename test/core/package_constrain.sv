package Q;
    localparam W = 5;
    localparam unrelated = 1;
endpackage

package P;
    import Q::*;
    export Q::W;
endpackage

module Example
    import P::*;
(
    input logic [W - 1:0] inp
);
    import Q::unrelated;
    initial $display("%b %0d %0d", inp, $bits(inp), unrelated);
endmodule
