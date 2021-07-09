module Module(input clock, input clear, input data);
    logic x, y;
    assign y = data;
    assign x = y;
    assert property (
        @(posedge clock) disable iff(clear) x == y
    );
    named: assert property (
        @(posedge clock) disable iff(clear) x == y
    );
    task hello;
        $display("Hello!");
    endtask
    always @(posedge clock) begin
        assert property (x == y);
        named_stmt: assert property (x == y);
    end
endmodule
