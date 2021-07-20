`define IDX(a, r, c) a[(r * 3 + c) * 8 +: 8]
module mod(
    input clk,
    input [7:0] row, col,
    output [47:0] flat
);
    function automatic [47:0] f;
        input [47:0] inp;
        integer i, j;
        for (i = 0; i < 2; i = i + 1)
            for (j = 0; j < 3; j = j + 1)
                `IDX(f, i, j) = (i + 1) * (j + 1) * `IDX(inp, i, j);
    endfunction

    reg [47:0] arr;
    reg [47:0] res;
    assign flat = res;

    initial arr = 0;
    always @(posedge clk) begin
        `IDX(arr, row, col) = `IDX(arr, row, col) + 1;
        res = f(arr);
    end
endmodule
