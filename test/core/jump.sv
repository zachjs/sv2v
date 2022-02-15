module top;

    task skip1;
        $display("HELLO skip1");
        return;
        $display("UNREACHABLE ", `__LINE__);
    endtask
    function void skip2;
        $display("HELLO skip2");
        return;
        $display("UNREACHABLE ", `__LINE__);
    endfunction
    function int skip3;
        $display("HELLO skip3");
        return 1;
        $display("UNREACHABLE ", `__LINE__);
    endfunction
    task skip4;
        for (int i = 0; i < 10; ++i) begin
            $display("HELLO skip4");
            return;
            $display("UNREACHABLE ", `__LINE__);
        end
        $display("UNREACHABLE ", `__LINE__);
    endtask
    task skip5;
        for (int i = 0; i < 10; ++i) begin
            $display("HELLO skip5-1");
            for (int j = 0; j < 10; ++j) begin
                $display("HELLO skip5-2");
                return;
                $display("UNREACHABLE ", `__LINE__);
            end
            $display("UNREACHABLE ", `__LINE__);
        end
        $display("UNREACHABLE ", `__LINE__);
    endtask
    task skip6;
        for (int i = 0; i < 0; ++i) begin
            $display("UNREACHABLE ", `__LINE__);
            return;
        end
        $display("HELLO skip6");
    endtask
    task skip7;
        begin
            parameter x = 1;
            $display("HELLO skip7");
            if (x == 1) return;
            $display("UNREACHABLE ", `__LINE__);
        end
        $display("UNREACHABLE ", `__LINE__);
    endtask
    task skip8;
        begin
            parameter x = 1;
            $display("HELLO skip8-1");
            if (x == 2) return;
            $display("HELLO skip8-2");
        end
        $display("HELLO skip8-3");
    endtask
    initial begin
        skip1;
        skip2;
        $display(skip3());
        skip4;
        skip5;
        skip6;
        skip7;
        skip8;
    end

    initial
        for (int i = 0; i < 10; ++i) begin
            $display("Loop Y:", i);
            continue;
            $display("UNREACHABLE ", `__LINE__);
        end

    initial
        for (int i = 0; i < 10; ++i) begin
            $display("Loop Z:", i);
            break;
            $display("UNREACHABLE ", `__LINE__);
        end

    initial
        for (int i = 0; i < 10; ++i)
            if (i < 5)
                $display("Loop A:", i);
            else
                break;

    initial
        for (int i = 0; i < 10; ++i) begin
            if (i < 3)
                $display("Loop B-1:", i);
            else if (i < 7)
                $display("Loop B-2:", i);
            else begin
                $display("Loop B-3:", i);
                continue;
                $display("UNREACHABLE ", `__LINE__);
            end
            $display("Loop B:", i);
        end

    always #1
        for (int i = 0; i < 10; ++i) begin
            $display("Loop C-1:", i);
            break;
            $display("UNREACHABLE ", `__LINE__);
        end
    initial #5 $finish(0);

    initial begin
        for (int unsigned i = 0; i < 5; ++i) begin
            $display("Loop D-1:", i);
            if (i == 3) begin
                $display("Loop D-2:", i);
                break;
                $display("UNREACHABLE ", `__LINE__);
            end
            $display("Loop D-3:", i);
        end
        for (int unsigned i = 0; i < 5; i++) begin
            $display("Loop E-1:", i);
            if (i == 2) begin
                $display("Loop E-2:", i);
                break;
                $display("UNREACHABLE ", `__LINE__);
            end
            $display("Loop E-3:", i);
        end
        $display("Block F-1");
    end

    initial begin
        int i;
        for (i = 0; i < 10; ++i)
            if (i < 5)
                $display("Loop F:", i);
            else
                break;
    end

endmodule
