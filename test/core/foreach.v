module top;
    reg [7:0] foo = {2'b10,2'b01,2'b11,2'b00};
    initial begin : f
        integer x;
        for (x = 0; x <= 3; x = x + 1)
            $display(x, foo[6 - 2*x+:2]);
    end

    reg [32*2*3*4 - 1:0] A;
    reg [5*4*4*2 + 32: 1 + 32] B;
    initial begin
        A = 0;
        B = 0;
        begin : g
            integer i, j, k;
            for (i = 0; i <= 1; i = i + 1)
            for (j = 0; j <= 2; j = j + 1)
            for (k = 0; k <= 3; k = k + 1)
            $display(i, j, k);
        end
        begin : h
            integer q, r, s;
            for (q = 5; q >= 1; q = q - 1)
            for (r = 0; r <= 3; r = r + 1)
            for (s = 2; s >= 1; s = s - 1)
            $display(q, r, s);
        end
    end
endmodule
