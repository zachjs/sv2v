module top;
    function f;
        input integer z;
        input integer a;
        begin
            $display(z, a);
            f = z + a;
        end
    endfunction
    task t;
        input integer z;
        input integer a;
        $display(z, a);
    endtask
    initial begin
        $display(f(7, 3));
        t(9, 5);
    end
endmodule
