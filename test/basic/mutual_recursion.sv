module top;
    function automatic integer bar;
        input integer inp;
        begin
            if (inp == 0)
                bar = 0;
            else
                bar = 1 + foo(inp - 1);
        end
    endfunction
    function automatic integer foo;
        input integer inp;
        begin
            if (inp == 0)
                foo = 0;
            else
                foo = 1 + bar(inp - 1);
        end
    endfunction
    initial begin
        $display("%d", foo(10));
        $display("%d", bar(11));
    end
endmodule
