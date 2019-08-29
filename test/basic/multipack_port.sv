module foo(clock, data);
    input logic clock;
    output logic [10:0] data [5];
    initial data[0][0] = 0;
    always @(clock) begin
        integer i, j;
        for (i = 4; i >= 0; i--) begin
            for (j = 9; j >= 0; j--) begin
                data[i][j + 1] = data[i][j];
            end
            if (i != 0)
                data[i][0] = data[i-1][10];
        end
        data[0][0] = ~data[0][0];
    end
endmodule
