module top;
    (* foo="bar" *) reg x;
    initial begin
        x = 1;
        $display(x);
    end
endmodule
