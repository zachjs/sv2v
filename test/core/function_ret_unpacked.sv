module mod(
    input logic clk,
    input byte row, col,
    output logic [47:0] flat
);
    typedef byte T [2][3];
    function automatic T f;
        input T inp;
        for (int i = 0; i < 2; i++)
            for (int j = 0; j < 3; j++)
                f[i][j] = (i + 1) * (j + 1) * inp[i][j];
    endfunction

    byte arr [2][3];
    byte res [2][3];
    assign flat =
        { res[1][2], res[1][1], res[1][0]
        , res[0][2], res[0][1], res[0][0] };

    initial
        { arr[1][2], arr[1][1], arr[1][0]
        , arr[0][2], arr[0][1], arr[0][0] } = 0;
    always @(posedge clk) begin
        arr[row][col] += 1;
        res = f(arr);
    end
endmodule
