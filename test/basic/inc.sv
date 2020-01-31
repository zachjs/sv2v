module top;
    integer x = 0;
    initial begin
        x++;
        $display(x);
    end
    initial begin
        ++x;
        $display(x);
    end
    initial begin
        repeat (0);
        x++;
        $display(x);
    end
    initial begin
        repeat (0);
        ++x;
        $display(x);
    end
endmodule
