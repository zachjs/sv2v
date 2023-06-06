`define TEST(id, ctrl) \
    always @* #(2 * id) \
        if (a) \
            ctrl if (b) \
                $display("%0d A 1", id); \
            else \
                $display("%0d A 2", id); \
    always @* #(2 * id + 1) \
        if (a) begin \
            ctrl if (b) \
                $display("%0d B 1", id); \
        end else \
            $display("%0d B 2", id);

`define NOTHING
`define ATTRIBUTE (* test *)
`define FOR for (i = 0; i < 1; i = i + 1)
`define WHILE while (i < 1)
`define REPEAT repeat (b)
`define FOREVER forever #10
`define TIMING #10

// The other tested constructs are supported in Verilog-2005.
`ifdef REF
    `define FOREACH repeat ($bits(i))
`else
    `define FOREACH foreach (i[j])
`endif

module top;
    reg a, b;
    integer i;

    `TEST(0, `NOTHING)
    `TEST(1, `ATTRIBUTE)
    `TEST(2, `FOR)
    `TEST(3, `WHILE)
    `TEST(4, `REPEAT)
    `TEST(5, `FOREACH)
    `TEST(6, `FOREVER)
    `TEST(7, `TIMING)

    initial begin
        repeat (2) begin
            #50 a = 0; b = 0;
            #50 a = 0; b = 1;
            #50 a = 1; b = 0;
            #50 a = 1; b = 1;
        end
        $finish(0);
    end
endmodule
