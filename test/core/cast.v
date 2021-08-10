module top;

    generate
        if (1) begin : A
            reg signed [31:0] x;
        end
    endgenerate
    initial begin : foo_block
        reg [31:0] w;
        reg signed [31:0] y;
        reg [4:0] z;
        w = 1234;
        A.x = -235;
        y = 1234;
        z = y;
        $display("%0d %0d", w, w[4:0]);
        $display("%0d %0d", A.x, $signed(A.x[4:0]));
        $display("%0d %0d", y, $signed(y[4:0]));
        $display("%0d %0d", z, z[4:0]);
        $display("%0d %0d", w+1, w[4:0]+1);
        $display("%0d %0d", A.x+1, $signed(A.x[4:0])+1);
        $display("%0d %0d", y+1, $signed(y[4:0])+1);
        $display("%0d %0d", z+1, z[4:0]+1);
        $display("%b %b", w, {8'b0, w});
        $display("%b %b", A.x, {8'hFF, A.x});
        $display("%b %b", y, {8'b0, y});
        $display("%b %b", z, {35'b0, z});
        $display("%0d %0d", w, w[4:0]);
        $display("%0d %0d", A.x, $signed(A.x[4:0]));
        $display("%0d %0d", y, $signed(y[4:0]));
        $display("%0d %0d", z, z[4:0]);
        $display("%b", 32'd4);
        $display("%b", 33'd4);
        $display("%b", 33'h1_FFFF_FFFF);
    end

    localparam [0:0] foo = 0;
    localparam [31:0] bar = 32'b0;
    initial $display("%b %b", foo, bar);

    initial begin
        $display("%b", 5'sb11111);
        $display("%b", 5'sb11111);
    end

    parameter W = 9;
    initial begin : block
        reg signed [7:0] i;
        reg [7:0] j;
        reg [8:0] i_extended;
        reg [8:0] j_extended;
        i = -1;
        j = -1;
        i_extended = i;
        j_extended = j;
        $display("%b", i_extended);
        $display("%b", j_extended);
    end

    initial begin
        $display("T1 %0d", -1);
        $display("T2 %0d", -1);
        $display("T3 %0d", 32'hFFFF_FFFF);
        $display("T1 %0d", 1);
        $display("T2 %0d", 1);
        $display("T3 %0d", 1);
        $display("T1 %0d", 1);
        $display("T2 %0d", 1);
        $display("T3 %0d", 1);
    end

endmodule
