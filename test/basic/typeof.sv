module top;
    function f;
        input x;
        f = 1'b1 ^ x;
        $display("f(%b) called", x);
    endfunction

    initial begin
        type(f(0)) x = f(0);
        $display("%b", x);
        $display("%b", $bits(x));
        $display("%b", $bits(type(x)));
        $display("%b", $bits(logic [0:1+$bits(type(x))]));
    end
endmodule
