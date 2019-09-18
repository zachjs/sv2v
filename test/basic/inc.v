module top;
    integer x = 0;
    initial begin
        x = x + 1;
        $display(x);
    end
endmodule
