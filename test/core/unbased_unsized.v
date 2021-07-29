`define TEST(value) \
    reg [63:0] val_``value = {64{1'b``value}}; \
    initial $display(`"'value -> %b (%0d) %b (%0d)`", \
        val_``value, $bits(val_``value), \
        1'b``value, $bits(1'b``value) \
        );

module top;
    `TEST(1)
    `TEST(0)
    `TEST(x)
    `TEST(z)

    reg flag;
    reg [31:0] i;
    reg [31:0] a;
    reg [31:0] b;
    reg [31:0] c;
    reg [63:0] j;
    reg [63:0] d;
    reg [63:0] e;
    initial begin
        i = 42;
        j = 42;
        flag = 1;
        a = (flag ? 32'hFFFFFFFF : i);
        b = (flag ? 32'hXXXXXXXX : i);
        c = (flag ? 32'hFFFFFFFF : i);
        d = (flag ? 64'hFFFFFFFFFFFFFFFF : j);
        e = (flag ? 64'hXXXXXXXXXXXXXXXX : j);
        $display("%b", a);
        $display("%b", b);
        $display("%b", c);
        $display("%b", d);
        $display("%b", e);
    end

    initial begin
        $display("%b", 4'b1xz0);
        $display("%b", {4'b1xz0, 4'b1xz0});
    end

    initial begin
        $display(1);
        $display(1);
        $display(1);
        $display(1);
    end

    M       m1({ 1 {1'b0}}, { 2 {1'b1}}, { 3 {1'bx}}, { 4 {1'bz}});
    M #( 2) m2({ 2 {1'b0}}, { 3 {1'b1}}, { 4 {1'bx}}, { 5 {1'bz}});
    M #(28) m3({28 {1'b0}}, {29 {1'b1}}, {30 {1'bx}}, {31 {1'bz}});
    M #(29) m4({29 {1'b0}}, {30 {1'b1}}, {31 {1'bx}}, {32 {1'bz}});
    M #(30) m5({30 {1'b0}}, {31 {1'b1}}, {32 {1'bx}}, {33 {1'bz}});
    M #(31) m6({31 {1'b0}}, {32 {1'b1}}, {33 {1'bx}}, {34 {1'bz}});
    M #(32) m7({32 {1'b0}}, {33 {1'b1}}, {34 {1'bx}}, {35 {1'bz}});
    M #(33) m8({33 {1'b0}}, {34 {1'b1}}, {35 {1'bx}}, {36 {1'bz}});
    M #(34) m9({34 {1'b0}}, {35 {1'b1}}, {36 {1'bx}}, {37 {1'bz}});

    M #(31) mA({31 {1'b0}}, {32 {1'b1}}, {33 {1'bx}}, {34 {1'bz}});
    M #(34) mB({34 {1'b0}}, {35 {1'b1}}, {36 {1'bx}}, {37 {1'bz}});
    M #(31) mC({31 {1'b0}}, {32 {1'b1}}, {33 {1'bx}}, {34 {1'bz}});
    M #(34) mD({34 {1'b0}}, {35 {1'b1}}, {36 {1'bx}}, {37 {1'bz}});

`define TEST_OP(left, op, right, expected) \
    $display(`"PASS: (left) op (right) -> %b (ref: %b)`", expected, expected);

    initial begin
        `TEST_OP( 1'h1        , ==, '1, 1'b1)
        `TEST_OP( 2'h3        , ==, '1, 1'b1)
        `TEST_OP(31'h7fffffff , ==, '1, 1'b1)
        `TEST_OP(32'hffffffff , ==, '1, 1'b1)
        `TEST_OP(33'h1ffffffff, ==, '1, 1'b1)

        `TEST_OP('1, ==,  1'h1        , 1'b1)
        `TEST_OP('1, ==,  2'h3        , 1'b1)
        `TEST_OP('1, ==, 31'h7fffffff , 1'b1)
        `TEST_OP('1, ==, 32'hffffffff , 1'b1)
        `TEST_OP('1, ==, 33'h1ffffffff, 1'b1)

        `TEST_OP( 1'h1        , <=, '1, 1'b1)
        `TEST_OP( 2'h3        , <=, '1, 1'b1)
        `TEST_OP(31'h7fffffff , <=, '1, 1'b1)
        `TEST_OP(32'hffffffff , <=, '1, 1'b1)
        `TEST_OP(33'h1ffffffff, <=, '1, 1'b1)

        `TEST_OP( 1'h1        , >=, '1, 1'b1)
        `TEST_OP( 2'h3        , >=, '1, 1'b1)
        `TEST_OP(31'h7fffffff , >=, '1, 1'b1)
        `TEST_OP(32'hffffffff , >=, '1, 1'b1)
        `TEST_OP(33'h1ffffffff, >=, '1, 1'b1)

        `TEST_OP( 1'h1        , &, '1,  1'h1        )
        `TEST_OP( 2'h3        , &, '1,  2'h3        )
        `TEST_OP(31'h7fffffff , &, '1, 31'h7fffffff )
        `TEST_OP(32'hffffffff , &, '1, 32'hffffffff )
        `TEST_OP(33'h1ffffffff, &, '1, 33'h1ffffffff)

        `TEST_OP(33'h1ffffffff, &, P ? '1 : '0, 33'h1ffffffff)
        `TEST_OP(33'h1ffffffff, &, '1 & '1, 33'h1ffffffff)
        `TEST_OP('1 & '1, &, 33'h1ffffffff, 33'h1ffffffff)
        `TEST_OP(33'h1ffffffff, &, !P ? '1 : '0 - 1, 33'h1ffffffff)
        `TEST_OP(34'h3ffffffff, &, '0 - 1, 34'h3ffffffff)

        `TEST_OP(1, ==, 2'h3 == '1, 1'b1)
    end

    parameter A = 8;
    parameter B = 5;
    reg [A*B-1:0] arr;
    initial begin
        arr = 1'sb1; $display("%b", arr);
        arr = 1'sb0; $display("%b", arr);
        arr = 1'sbx; $display("%b", arr);
        arr = 1'sbz; $display("%b", arr);
    end

    reg pick;
    wire [8:0] w0, w1, w2, w3;
    assign w0 = pick ? 9'h1FF : 9'h000;
    assign w1 = pick ? 9'h1FF : 9'h000;
    assign w2 = pick ? 9'h1FF : 9'h000;
    assign w3 = pick ? 9'h1FF : 9'h000;
    initial begin
        $monitor("%0d %b %b %b %b %b", $time, pick, w0, w1, w2, w3);
        #1 pick = 0;
        #1 pick = 1;
        #1 pick = 0;
        #1 pick = 1;
    end

    initial begin
        $display("tern %b", 1'b1);
        $display("tern %b", A ? -1 : A);
        $display("tern %b", A ? A : -1);
    end
endmodule

module M(a, b, c, d);
    parameter W = 1;
    input wire [W+0:1] a;
    input wire [W+1:1] b;
    input wire [W+2:1] c;
    input wire [W+3:1] d;
    initial $display("M W=%0d %b %b %b %b", W, a, b, c, d);
endmodule
