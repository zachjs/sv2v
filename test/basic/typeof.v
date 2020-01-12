module top;
    function f;
        input x;
        begin
            f = 1'b1 ^ x;
            $display("f(%b) called", x);
        end
    endfunction
    task t;
        input x;
        $display("t(%b) called", x);
    endtask

    initial begin : block
        reg x;
        x = f(0);
        $display("%b", x);
        $display("%b", 32'd1);
        $display("%b", 32'd1);
        $display("%b", 32'd3);
        x = f(1);
        x = f(0);
        t(1);
    end
endmodule
