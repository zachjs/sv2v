module top;
    function f;
        input integer z;
        input integer a;
        $display(z, a);
        return z + a;
    endfunction
    task t;
        input integer z;
        input integer a;
        $display(z, a);
    endtask
    initial begin
        $display(f(.a(3), .z(7)));
        t(.a(5), .z(9));
    end
endmodule
