module top;
    logic t;
    initial $display("A t %0d", $bits(t));
    initial $display("A top.t %0d", $bits(top.t));
    generate
        begin : X
            logic [1:0] t;
            initial $display("B t %0d", $bits(t));
            initial $display("B top.t %0d", $bits(top.t));
            initial $display("B X.t %0d", $bits(X.t));
            initial $display("B top.X.t %0d", $bits(top.X.t));
            begin : Y
                logic [2:0] t;
                initial $display("C t %0d", $bits(t));
                initial $display("C top.t %0d", $bits(top.t));
                initial $display("C X.t %0d", $bits(X.t));
                initial $display("C top.X.t %0d", $bits(top.X.t));
                initial $display("C Y.t %0d", $bits(Y.t));
                initial $display("C X.Y.t %0d", $bits(X.Y.t));
                initial $display("C top.X.Y.t %0d", $bits(top.X.Y.t));
            end
        end
        for (genvar i = 0; i < 3; ++i) begin : Z
            logic [i:0] t;
        end
    endgenerate
    initial $display("A t %0d", $bits(t));
    initial $display("A top.t %0d", $bits(top.t));
    initial $display("A X.t %0d", $bits(X.t));
    initial $display("A top.X.t %0d", $bits(top.X.t));
    initial $display("A X.Y.t %0d", $bits(X.Y.t));
    initial $display("A top.X.Y.t %0d", $bits(top.X.Y.t));
    initial $display("A top.Z[0].t %0d", $bits(top.Z[0].t));
    initial $display("A Z[0].t %0d", $bits(Z[0].t));
    initial $display("A Z[1].t %0d", $bits(Z[1].t));
    initial $display("A Z[2].t %0d", $bits(Z[2].t));

    logic x;
    initial begin
        type(x) x [1:0];
        type(x) y [2:0];
        $display("size of x = %0d", $bits(x));
        $display("size of y = %0d", $bits(y));
    end

    logic [2:0][3:0] arr;
    generate
        begin : M
            logic [3:0][4:0] arr;
            initial $display("M arr[0] = %b", arr[0]);
            initial $display("M M.arr[0] = %b", M.arr[0]);
            initial $display("M top.arr[0] = %b", top.arr[0]);
        end
    endgenerate
    initial $display("arr[0] = %b", arr[0]);
    initial $display("M.arr[0] = %b", M.arr[0]);
    initial $display("top.arr[0] = %b", top.arr[0]);

    localparam arr2 [2][3] = '{
        '{1'b0, 1'b1, 1'b1},
        '{1'b1, 1'b0, 1'b0}
    };
    for (genvar i = 0 ; i < 2 ; ++i) begin
        for (genvar j = 0 ; j < 3 ; ++j) begin
            localparam value = arr2[i][j];
            initial $display("%0d %0d %0d", i, j, value);
        end
    end
endmodule
