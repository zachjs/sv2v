module top;
    parameter A = 3;
    parameter B = 4;
    logic [A*B-1:0] arr;
    initial begin
        arr = 0;
        for (integer i = 0; i < A; ++i) begin
            // yes this is silly but it captures an interesting edge case
            arr[i * B +: B] = $bits(arr[i * B +: B])'(i);
        end
        $display("%b", arr);
    end
endmodule
