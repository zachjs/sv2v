module top;
    task t;
        typedef byte u;
        $display("t %b", u'('1));
    endtask
    function integer f;
        input reg signed i;
        typedef shortint v;
        $display("i %b", v'(i));
        return $bits(v);
    endfunction
    initial begin
        t();
        $display("f %b", f(0));
        $display("f %b", f(1));
    end
endmodule
