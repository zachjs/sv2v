module top;
    task foo;
        integer x;
        begin
            x = 2;
            $display(x * x);
        end
    endtask
    task bar;
        integer y;
        begin
            y = 3;
            $display(y * y);
        end
    endtask
    task baz;
        integer w;
        begin
            w = 3;
            $display(w ** w);
        end
    endtask
    initial foo;
    initial bar;
    initial baz;
endmodule
