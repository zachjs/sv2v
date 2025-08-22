module top;
        task t;
                $display("t = %d", 1'd0);
        endtask
        function f;
                input reg _sv2v_unused;
                f = 1'd1;
        endfunction
        initial t;
        initial $display("f = %d", f(0));
endmodule
