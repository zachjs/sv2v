module top;
    task t;
        typedef bit u;
        $display("t = %d", u'(0));
    endtask
    function f;
        typedef bit u;
        return u'(1);
    endfunction
    initial t();
    initial $display("f = %d", f());
endmodule
