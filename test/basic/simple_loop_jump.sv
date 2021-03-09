module top;
    function automatic integer f;
        input integer inp;
        f = 1;
        for (integer idx = 0; idx < inp; idx = idx + 1) begin
            if (f == 32)
                break;
            f = f * 2;
        end
    endfunction
    integer i;
    initial
        for (i = 0; i < 10; i = i + 1)
            $display("f(%0d) = %0d", i, f(i));
endmodule
