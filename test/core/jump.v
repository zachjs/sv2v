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
    task skip6;
        $display("HELLO skip6");
    endtask
    task skip7;
        $display("HELLO skip7");
    endtask
    task skip8;
        begin
            $display("HELLO skip8-1");
            $display("HELLO skip8-2");
            $display("HELLO skip8-3");
        end
    endtask
    initial begin
        skip1;
        skip2;
        $display(skip3(0));
        skip4;
        skip5;
        skip6;
        skip7;
        skip8;
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

    always #1 begin : loop_c
        integer i;
        for (i = 0; i < 10; ++i) begin
            $display("Loop C-1:", i);
            i = 10;
        end
    end
    initial #5 $finish(0);

    initial begin : loops_de
        reg unsigned [31:0] i;
        for (i = 0; i < 5; ++i) begin
            $display("Loop D-1:", i);
            if (i == 3) begin
                $display("Loop D-2:", i);
                i = 5;
            end
            else
                $display("Loop D-3:", i);
        end
        for (i = 0; i < 5; i++) begin
            $display("Loop E-1:", i);
            if (i == 2) begin
                $display("Loop E-2:", i);
                i = 5;
            end
            else
                $display("Loop E-3:", i);
        end
        $display("Block F-1");
    end

    initial begin : loop_f
        integer i;
        for (i = 0; i < 5; ++i)
            $display("Loop F:", i);
    end

endmodule
