module top;

    parameter WIDTH = 32;
    generate
        begin : A
            int x = -235;
        end
    endgenerate
    initial begin
        logic [31:0] w = 1234;
        int y = 1234;
        logic [4:0] z = y;
        $display("%0d %0d", w, 5'(w));
        $display("%0d %0d", A.x, 5'(A.x));
        $display("%0d %0d", y, 5'(y));
        $display("%0d %0d", z, 5'(z));
        $display("%0d %0d", w+1, 5'(w+1));
        $display("%0d %0d", A.x+1, 5'(A.x+1));
        $display("%0d %0d", y+1, 5'(y+1));
        $display("%0d %0d", z+1, 5'(z+1));
        $display("%b %b", w, 40'(w));
        $display("%b %b", A.x, 40'(A.x));
        $display("%b %b", y, 40'(y));
        $display("%b %b", z, 40'(z));
        $display("%0d %0d", w, ($clog2(WIDTH))'(w));
        $display("%0d %0d", A.x, ($clog2(WIDTH))'(A.x));
        $display("%0d %0d", y, ($clog2(WIDTH))'(y));
        $display("%0d %0d", z, ($clog2(WIDTH))'(z));
        $display("%b", 32'(4));
        $display("%b", 33'(4));
        $display("%b", 33'(64'hFFFF_FFFF_FFFF_FFFF));
    end

    localparam bit foo = '0;
    localparam logic [31:0] bar = 32'(foo);
    initial $display("%b %b", foo, bar);

    initial begin
        $display("%b", 5'('1));
        $display("%b", 5'(1'sb1));
    end

    parameter W = 9;
    initial begin
        byte i = -1;
        byte unsigned j = -1;
        $display("%b", W'(i));
        $display("%b", W'(j));
    end

    typedef integer T1;
    typedef integer signed T2;
    typedef integer unsigned T3;
    initial begin
        $display("T1 %0d", T1'(1'sb1));
        $display("T2 %0d", T2'(1'sb1));
        $display("T3 %0d", T3'(1'sb1));
        $display("T1 %0d", T1'(32'sd1));
        $display("T2 %0d", T2'(32'sd1));
        $display("T3 %0d", T3'(32'sd1));
        $display("T1 %0d", T1'(32'd1));
        $display("T2 %0d", T2'(32'd1));
        $display("T3 %0d", T3'(32'd1));
    end

endmodule
