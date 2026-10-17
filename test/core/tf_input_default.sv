module top;
    function automatic [4:0] plus_one(input logic [3:0] a = 4'd3);
        plus_one = a + 5'd1;
    endfunction

    task automatic show(input logic [3:0] a = 4'd5);
        automatic logic [3:0] value = a;
        $display("task %0d", value);
    endtask

    reg [3:0] x;
    wire [4:0] y = plus_one(x);
    integer i;
    initial begin
        for (i = 0; i < 16; i = i + 1) begin
            x = i;
            #1;
            $display("function %0d", y);
            show(x);
        end
    end
endmodule
