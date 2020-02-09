module top;

    wire [0:31] a;
    for (genvar n = 0; n < 32; n++) begin : gen_filter
        assign a[n] = n & 1;
        wire x;
        assign x = a[n];
    end

    wire [0:31] b;
    for (genvar n = 0; n < 32; n++) begin : gen_filter_other
        assign b[n] = ~gen_filter[n].x;
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

    initial begin
        integer i = 0;
        for (; i < 32; i++)
            $display("6: ", ~a[i]);
    end

    initial begin
        integer j = 0, k;
        for (; j < 4; j++) begin
            k = 0;
            for (; k < 8; k++)
                $display("7: ", ~a[j * 8 + k] + 11);
        end
    end

    initial
        for (integer i = 0; i < 32; i++)
            $display("8: ", a[i], b[i]);

    logic start;
    assign start = gen_filter[0].x;
    initial $display(start);

    logic [0:31] c;
    generate
        ;
        for (genvar n = 0; n < 32; n = n + 1)
            assign c[n] = n & 1;
        for (genvar m = 0; m < 32; m = m + 1) begin end
    endgenerate

endmodule
