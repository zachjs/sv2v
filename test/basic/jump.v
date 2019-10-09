module top;

    task skip1;
        $display("HELLO skip1");
    endtask
    task skip2;
        $display("HELLO skip2");
    endtask
    function integer skip3;
        input x;
        begin
            $display("HELLO skip3");
            skip3 = 1;
        end
    endfunction
    task skip4;
        $display("HELLO skip4");
    endtask
    task skip5;
        begin
            $display("HELLO skip5-1");
            $display("HELLO skip5-2");
        end
    endtask
    initial begin
        skip1;
        skip2;
        $display(skip3(0));
        skip4;
        skip5;
    end

    initial begin : loop_y
        integer i;
        for (i = 0; i < 10; ++i)
            $display("Loop Y:", i);
    end

    initial begin : loop_z
        integer i;
        i = 0;
        $display("Loop Z:", i);
    end

    initial begin : loop_a
        integer i;
        for (i = 0; i < 5; ++i)
            $display("Loop A:", i);
    end

    initial begin : loop_b
        integer i;
        for (i = 0; i < 10; ++i) begin
            if (i < 3) begin
                $display("Loop B-1:", i);
                $display("Loop B:", i);
            end
            else if (i < 7) begin
                $display("Loop B-2:", i);
                $display("Loop B:", i);
            end
            else
                $display("Loop B-3:", i);
        end
    end

endmodule
