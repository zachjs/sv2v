module top;
    function f;
        input x;
        begin
            f = 1'b1 ^ x;
            $display("f(%b) called", x);
        end
    endfunction

    initial begin : block
        reg x;
        x = f(0);
        $display("%b", x);
        $display("%b", 32'd1);
        $display("%b", 32'd1);
        $display("%b", 32'd3);
    end
endmodule
