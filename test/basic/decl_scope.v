module top;
    wire t;
    initial $display("A t %0d", 1);
    initial $display("A top.t %0d", 1);
    generate
        if (1) begin : X
            wire [1:0] t;
            initial $display("B t %0d", 2);
            initial $display("B top.t %0d", 1);
            initial $display("B X.t %0d", 2);
            initial $display("B top.X.t %0d", 2);
            if (1) begin : Y
                wire [2:0] t;
                initial $display("C t %0d", 3);
                initial $display("C top.t %0d", 1);
                initial $display("C X.t %0d", 2);
                initial $display("C top.X.t %0d", 2);
                initial $display("C Y.t %0d", 3);
                initial $display("C X.Y.t %0d", 3);
                initial $display("C top.X.Y.t %0d", 3);
            end
        end
        genvar i;
        for (i = 0; i < 3; i = i + 1) begin : Z
            wire [i:0] t;
        end
    endgenerate
    initial $display("A t %0d", 1);
    initial $display("A top.t %0d", 1);
    initial $display("A X.t %0d", 2);
    initial $display("A top.X.t %0d", 2);
    initial $display("A X.Y.t %0d", 3);
    initial $display("A top.X.Y.t %0d", 3);
    initial $display("A top.Z[0].t %0d", 1);
    initial $display("A Z[0].t %0d", 1);
    initial $display("A Z[1].t %0d", 2);
    initial $display("A Z[2].t %0d", 3);

    wire x;
    initial begin : name
        reg [1:0] x;
        reg [5:0] y;
        $display("size of x = %0d", $bits(x));
        $display("size of y = %0d", $bits(y));
    end

    wire [11:0] arr;
    generate
        if (1) begin : M
            wire [19:0] arr;
            initial $display("M arr[0] = %b", arr[4:0]);
            initial $display("M M.arr[0] = %b", M.arr[4:0]);
            initial $display("M top.arr[0] = %b", top.arr[3:0]);
        end
    endgenerate
    initial $display("arr[0] = %b", arr[3:0]);
    initial $display("M.arr[0] = %b", M.arr[4:0]);
    initial $display("top.arr[0] = %b", top.arr[3:0]);

    localparam [0:5] arr2 = 6'b011100;
    generate
        genvar j;
        for (i = 0 ; i < 2 ; i = i + 1) begin
            for (j = 0 ; j < 3 ; j = j + 1) begin
                localparam value = arr2[i * 3 + j];
                initial $display("%0d %0d %0d", i, j, value);
            end
        end
    endgenerate
endmodule
