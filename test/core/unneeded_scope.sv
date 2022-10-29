module top;
    function automatic integer incr;
        input integer inp;
        return inp + 1;
    endfunction
    parameter P = 3;
    logic [incr(P):0] x;
    logic [$bits(x) * 2:0] y, z;
    if (1) begin
        assign top.y = {top.x, x};
    end
    if (1) begin : blk
        wire x;
        assign z = {blk.x, top.x};
    end
endmodule
