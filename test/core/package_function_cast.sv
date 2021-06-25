package P;
    function automatic logic [7:0] f(input logic [2:0] p);
        logic [7:0] r;
        localparam T = $bits(r[7:0]);
        r = T'(1'sb0);
        r[p+:2] = $bits(r[p+:2])'(1'sb1);
        return r;
    endfunction
endpackage

module top;
    logic [2:0] p;
    logic [7:0] q;
    assign q = P::f(p);
    initial begin
        $monitor("%0d, p=%b q=%b", $time, p, q);
        #1 p = 0;
        while (p != 7)
            #1 p = p + 1;
    end
endmodule
