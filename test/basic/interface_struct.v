module top;
    generate
        begin : i
            wire [3:0] x;
            reg [1:0] w;
        end
    endgenerate
    assign i.x = 4'b1001;
    initial i.w = 2'b10;
    initial begin
        $display("%b", i.x[3]);
        $display("%b", i.x[2]);
        $display("%b", i.x[1]);
        $display("%b", i.x[0]);
    end
endmodule
