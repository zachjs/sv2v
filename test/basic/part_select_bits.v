module top;
    parameter A = 3;
    parameter B = 4;
    reg [A*B-1:0] arr;
    initial begin : foo
        integer i;
        arr = 0;
        for (i = 0; i < A; ++i) begin
            arr[i * B +: B] = i;
        end
        $display("%b", arr);
    end
endmodule

