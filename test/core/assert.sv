module Module(input clock, input clear, input data);
    logic x, y;
    assign y = data;
    assign x = y;
    assert property (
        @(posedge clock) disable iff(clear) x == y
    );
    task hello;
        $display("Hello!");
        assert property (x == y);
    endtask
endmodule
