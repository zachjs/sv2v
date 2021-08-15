module top;
    task t(
        input reg [31:0] inp,
        output reg [7:0] out1,
        output reg [15:0] out2
    );
        begin
            $display("t(inp = %0d)", inp);
            out1 = inp;
            out2 = inp;
        end
    endtask
    initial begin : blk
        reg [31:0] a;
        reg [7:0] b;
        reg [15:0] c;
        a = 5;
        t(a, b, c);
        $display("a = %0d, b = %0d, c = %0d", a, b, c);
    end
endmodule
