module top;
    function [2:0] f;
        input [2:0] n;
        f = n + 4;
    endfunction
    task t;
        begin
            $display("hello");
            $display("world");
        end
    endtask
    initial t();
    initial $display("f(0) = ", f(0));
    initial $display("f(1) = ", f(1));
endmodule
