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

module top;
  logic [10:0] data [5];
    reg clock;
    foo f(clock, data);

    initial begin
        clock = 1;
        repeat (100)
            #1 clock = ~clock;
    end

    initial begin : foo
        $monitor("%d %b%b%b%b%b", $time, data[0], data[1], data[2], data[3], data[4]);
    end
endmodule
