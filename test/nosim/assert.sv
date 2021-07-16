module top;
    reg clk;
    initial begin
        clk = 0;
        repeat (100) #1 clk = ~clk;
        $finish;
    end
    assert property (@(posedge clk) 1);
    assume property (@(posedge clk) 1);
    cover property (@(posedge clk) 1);
    initial begin
        assert (1);
        assume (1);
        cover (1);
        assume (1);
    end
    assert property (@(posedge clk) 1)
        else $display("FOO");
    assume property (@(posedge clk) 1)
            $display("FOO");
        else
            $display("BAR");
    assert property (@(posedge clk)
        (1 |-> (1 |=> (1 #-# (1 #=# (1 iff 1))))));
    assert property (@(posedge clk)
        1 and 1 or 1 intersect 1 throughout 1 within 1);
    assert property (@(posedge clk) 1 ##1 1);
    assert property (@(posedge clk) ##1 1);
    integer x;
    assert property (@(posedge clk) first_match(1, x++, $display("a", clk), $display("b", clk)));
endmodule
