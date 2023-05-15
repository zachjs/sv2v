module top;
    function [2:0] f;
        input [2:0] n;
        n += 1;
        return n + 3;
    endfunction
    task t;
        $display("hello");
        $display("world");
    endtask
    initial t();
    initial $display("f(0) = ", f(0));
    initial $display("f(1) = ", f(1));

    task t1;
        localparam X = 1;
        begin
            localparam X = 2;
            $display("t1", X);
        end
    endtask
    task t2;
        localparam X = 10;
        begin
            localparam X = 20;
            $display("t2", X);
        end
        $display("t2", X);
    endtask
    task t3;
        localparam X = 100;
        begin
            localparam Y = 200;
            $display("t3", Y);
        end
        $display("t3", X);
    endtask
    task t4;
        begin
            begin
                localparam Y = 99;
                $display("t4", Y);
            end
            $display("t4", 123);
        end
    endtask
    initial begin
        t1;
        t2;
        t3;
        t4;
    end

    localparam X = 1;
    task a;
        localparam X = 2;
        begin
            begin : blk
                localparam X = 3;
                $display("a", X);
            end
            $display("a", X);
        end
    endtask
    task b;
        begin : blk
            localparam X = 5;
            $display("b", X);
        end
        $display("b", X);
    endtask
    initial begin
        a;
        b;
    end
endmodule
