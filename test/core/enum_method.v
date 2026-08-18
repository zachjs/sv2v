module top;
    localparam A = 5;
    localparam B0 = 6;
    localparam B1 = 7;
    localparam B2 = 8;
    localparam C = 9;
    localparam D = 100;
    localparam E = 101;
    reg [31:0] x = D;
    initial begin : blk1
        $display("first = %0d", A);
        $display("last = %0d", E);
        $display("num = %0d", 7);

        $display(" A = %0d", A);
        $display("B0 = %0d", B0);
        $display("B1 = %0d", B1);
        $display("B2 = %0d", B2);
        $display(" C = %0d", C);
        $display(" D = %0d", D);
        $display(" E = %0d", E);

        $display(" E = %0d", E);
        $display(" D = %0d", D);
        $display(" C = %0d", C);
        $display("B2 = %0d", B2);
        $display("B1 = %0d", B1);
        $display("B0 = %0d", B0);
        $display(" A = %0d", A);

        $display(" A = %0d", A);
        $display("B1 = %0d", B1);
        $display(" C = %0d", C);
        $display(" E = %0d", E);
        $display("B0 = %0d", B0);
        $display("B2 = %0d", B2);
        $display(" D = %0d", D);

        $display("name = %2s", "");
        $display("next = %0d", 32'dx);
        $display("prev = %0d", 32'dx);
        $display("next(2) = %0d", 32'dx);
        $display("prev(2) = %0d", 32'dx);
    end

    localparam [7:0] X = 0;
    localparam [7:0] Y = 1;
    localparam [7:0] Z = 2;
    initial begin : blk2
        reg signed [7:0] i, a, b;

        $display("first = %b %b", X, X);
        $display("last = %b %b", Z, Z);
        $display("num = %b %b", 3, 3);

        for (i = -3; i <= 4; i += 1) begin
            a = $unsigned(i) % 8'd3;
            b = (8'd3 - ($unsigned(i) % 8'd3)) % 8'd3;
            $display("next(%0d) = %b %b", i, a, a);
            $display("prev(%0d) = %b %b", i, b, b);
        end
    end
endmodule
