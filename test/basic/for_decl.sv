module top;

    wire [0:31] a;
    for (genvar n = 0; n < 32; n++) begin : gen_filter
        assign a[n] = n & 1;
    end

    initial
        for (integer i = 0; i < 32; i++)
            $display("1: ", a[i]);

    integer i = 0;
    initial
        for (; i < 32; i++)
            $display("2: ", ~a[i]);

    initial begin
        for (integer i = 0, j = 42; i < 32; i++)
            $display("3: ", ~a[i] + 5, " j=", j);
    end

    initial begin
        integer i, j;
        for (i = 0, j = 97; i < 32; i++)
            $display("4: ", ~a[i] + 10, " j=", j);
    end

    integer j = 0, k;
    initial begin
        for (; j < 4; j++) begin
            k = 0;
            for (; k < 8; k++)
                $display("5: ", ~a[j * 8 + k] + 11);
        end
    end

endmodule
