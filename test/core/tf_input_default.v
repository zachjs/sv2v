module top;
    function automatic [4:0] plus_one;
        input [3:0] a;
        plus_one = a + 5'd1;
    endfunction

    task automatic show;
        input [3:0] a;
        reg [3:0] value;
        begin
            value = a;
            $display("task %0d", value);
        end
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
