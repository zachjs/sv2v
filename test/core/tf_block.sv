module top;
    function [2:0] f;
        input [2:0] n;
        n += 1;
        return n + 3;
    endfunction
    task t;
        $display("hello");
        $display("world");
    endtask
    initial t();
    initial $display("f(0) = ", f(0));
    initial $display("f(1) = ", f(1));
endmodule
