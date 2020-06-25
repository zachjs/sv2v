module top;
    reg x, y;
    wire z;
    task t;
        x = 1;
    endtask
    function f;
        input x;
        begin
            y = 1;
            f = 0;
        end
    endfunction
    assign z = 0;
    initial begin
        t;
        $display("%b %b %b %b", x, y, z, f(0));
        $display("%b %b %b %b", x, y, z, f(0));
    end
endmodule
