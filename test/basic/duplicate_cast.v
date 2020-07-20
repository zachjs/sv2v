module top;
    wire b;
    wire [1:0] a;
    function automatic val;
        input inp;
        val = inp;
    endfunction
    assign b = val(1);
    assign a = {2 {val(1)}};
    initial #1 $display("%b %b", a, b);
endmodule
