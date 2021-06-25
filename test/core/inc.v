module top;
    integer x = 0;
    initial repeat (4) begin
        x = x + 1;
        $display(x);
    end
endmodule
