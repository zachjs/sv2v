module top;
    localparam type T = int unsigned;
    wire [T'(32'sd32) - 1:0] x;
    function automatic [T'(32'sd32) - 1:0] foo;
        input reg [T'(32'sd32) - 1:0] inp;
        foo = inp;
    endfunction
    assign x = foo(1'sb1);
endmodule
