module top;
    function [2:0] f;
        input [2:0] n;
        f = n + 4;
    endfunction
    task t;
        begin
            $display("hello");
            $display("world");
        end
    endtask
    initial t();
    initial $display("f(0) = ", f(0));
    initial $display("f(1) = ", f(1));

    task t1;
        localparam X = 2;
        $display("t1", X);
    endtask
    task t2;
        localparam X = 10;
        localparam Y = 20;
        begin
            $display("t2", Y);
            $display("t2", X);
        end
    endtask
    task t3;
        localparam X = 100;
        localparam Y = 200;
        begin
            $display("t3", Y);
            $display("t3", X);
        end
    endtask
    task t4;
        localparam Y = 99;
        begin
            $display("t4", Y);
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
        localparam Y = 3;
        begin
            $display("a", Y);
            $display("a", X);
        end
    endtask
    task b;
        localparam Y = 5;
        begin
            $display("b", Y);
            $display("b", X);
        end
    endtask
    initial begin
        a;
        b;
    end
endmodule
