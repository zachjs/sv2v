module top;
    function automatic integer f;
        input integer x;
        output integer y;
        f = x * 3;
        y = x * 5;
    endfunction
    logic signed [31:0] x, y;
    initial x = f(-1, y);
    initial $display(x, y);
endmodule
