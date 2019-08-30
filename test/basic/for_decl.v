module top;

    wire [0:31] a;
    generate
        genvar n;
        for (n = 0; n < 32; n = n + 1) begin : gen_filter
            assign a[n] = n & 1;
        end
    endgenerate

    integer i;
    initial begin : foo_1
        for (i = 0; i < 32; i = i + 1)
            $display("1: ", a[i]);
    end

    initial begin : foo_2
        integer i;
        for (i = 0; i < 32; i = i + 1)
            $display("2: ", ~a[i]);
    end

    initial begin : foo_3
        integer i;
        integer j;
        j = 42;
        for (i = 0; i < 32; i = i + 1)
            $display("3: ", ~a[i] + 5, " j=", j);
    end

    initial begin : foo_4
        integer i, j;
        j = 97;
        for (i = 0; i < 32; i = i + 1)
            $display("4: ", ~a[i] + 10, " j=", j);
    end

    integer j, k;
    initial begin
        for (j = 0; j < 4; j++)
            for (k = 0; k < 8; k++)
                $display("5: ", ~a[j * 8 + k] + 11);
    end

endmodule
