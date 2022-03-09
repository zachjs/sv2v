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
        assert #0 (1);
        assume #0 (1);
        cover #0 (1);
        assert #0_0 (1);
        assume #0_0 (1);
        cover #0_0 (1);
        assert final (1);
        assume final (1);
        cover final (1);
    end

    assert final (1);
    assume final (1);
    cover final (1);
    a1: assert final (1);
    a2: assume final (1);
    a3: cover final (1);

    assert #0 (1);
    assume #0 (1);
    cover #0 (1);
    b1: assert #0 (1);
    b2: assume #0 (1);
    b3: cover #0 (1);

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
    localparam C = 1;
    assert property (@(posedge clk) ##C 1);
    assert property (@(posedge clk) ##(C + 1) 1);
    assert property (@(posedge clk) ##[C:1] 1);
    assert property (@(posedge clk) ##[+] 1);
    assert property (@(posedge clk) ##[*] 1);
    assert property (@(posedge clk) ##[ *] 1);

    integer x;
    // TODO: The assignment below should only be allowed in a property decleration.
    assert property (@(posedge clk) first_match(1, x++, $display("a", clk), $display("b", clk)));
endmodule
