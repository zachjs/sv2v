module top;
    logic x, y, z;
    task t;
        x = 1;
    endtask
    function f;
        y = 1;
        f = 0;
    endfunction
    assign z = 0;
    initial begin
        t;
        $display("%b %b %b %b", x, y, z, f());
        $display("%b %b %b %b", x, y, z, f());
    end
endmodule
