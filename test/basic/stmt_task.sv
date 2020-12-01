module top;
    task tick;
        $display("tick() called");
    endtask
    generate
        begin : foo
            task tick;
                $display("foo.tick() called");
            endtask
        end
        genvar i;
        for (i = 0; i < 2; i = i + 1) begin : bar
            task tick;
                $display("bar[%0d].tick() called", i);
            endtask
        end
    endgenerate

    initial begin
        tick;
        foo.tick;
        bar[0].tick;
        bar[1].tick;
        tick();
        foo.tick();
        bar[0].tick();
        bar[1].tick();
    end
endmodule
