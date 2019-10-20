module top;
    localparam MULTIPLIER_1 = 7;
    localparam MULTIPLIER_2 = 8;
    function integer bar_1;
        input integer x;
        bar_1 = x * x * MULTIPLIER_1;
    endfunction
    function integer bar_2;
        input integer x;
        bar_2 = x * x * MULTIPLIER_2;
    endfunction
    initial begin
        $display(bar_1(3));
        $display(bar_2(3));
    end
endmodule
