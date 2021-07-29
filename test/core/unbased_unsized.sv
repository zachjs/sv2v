`define TEST(value) \
    logic [63:0] val_``value = 'value; \
    initial $display(`"'value -> %b (%0d) %b (%0d)`", \
        val_``value, $bits(val_``value), \
        'value, $bits('value) \
        );

module top;
    `TEST(1);
    `TEST(0);
    `TEST(x);
    `TEST(z);

    logic flag;
    logic [31:0] i;
    logic [31:0] a;
    logic [31:0] b;
    logic [31:0] c;
    logic [63:0] j;
    logic [63:0] d;
    logic [63:0] e;
    initial begin
        i = 42;
        j = 42;
        flag = 1;
        a = (flag ? '1 : i);
        b = (flag ? 'x : i);
        c = (flag ? '1 : '0);
        d = (flag ? '1 : j);
        e = (flag ? 'x : j);
        $display("%b", a);
        $display("%b", b);
        $display("%b", c);
        $display("%b", d);
        $display("%b", e);
    end

    initial begin
        $display("%b", {'1, 'x, 'z, '0});
        $display("%b", {2 {'1, 'x, 'z, '0}});
    end

    initial begin
        $display($bits('1));
        $display($bits(flag ? '1 : 'x));
        $display($bits(type('1)));
        $display($bits(type(flag ? '1 : 'x)));
    end

    parameter P = 1;

    M       m1('0, '1, 'x, 'z);
    M #( 2) m2('0, '1, 'x, 'z);
    M #(28) m3('0, '1, 'x, 'z);
    M #(29) m4('0, '1, 'x, 'z);
    M #(30) m5('0, '1, 'x, 'z);
    M #(31) m6('0, '1, 'x, 'z);
    M #(32) m7('0, '1, 'x, 'z);
    M #(33) m8('0, '1, 'x, 'z);
    M #(34) m9('0, '1, 'x, 'z);

    M #(31) mA(P ? '0 : '1, !P ? '0 : '1, 'x, 'z);
    M #(34) mB(P ? '0 : '1, !P ? '0 : '1, 'x, 'z);
    M #(31) mC(P ? '0 : '0 + '1, !P ? '0 : '0 + '1, 'x, 'z);
    M #(34) mD(P ? '0 : '0 + '1, !P ? '0 : '0 + '1, 'x, 'z);

`define TEST_OP(left, op, right, expected) \
    $display(`"%s: (left) op (right) -> %b (ref: %b)`", \
        ((left) op (right)) == expected ? "PASS" : "FAIL", \
        (left) op (right), expected \
    );

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
    logic [A-1:0][B-1:0] arr;
    initial begin
        arr = '{default: '1}; $display("%b", arr);
        arr = '{default: '0}; $display("%b", arr);
        arr = '{default: 'x}; $display("%b", arr);
        arr = '{default: 'z}; $display("%b", arr);
    end

    reg pick;
    logic [8:0] w0, w1, w2, w3;
    assign w0 = pick ? '1 : $unsigned(4'd0);
    assign w1 = pick ? '1 : unsigned'(5'd0);
    assign w2 = pick ? '1 : $signed(6'd0);
    assign w3 = pick ? '1 : signed'(7'd0);
    initial begin
        $monitor("%0d %b %b %b %b %b", $time, pick, w0, w1, w2, w3);
        #1 pick = 0;
        #1 pick = 1;
        #1 pick = 0;
        #1 pick = 1;
    end

    initial begin
        $display("tern %b", A ? '1 : 'X);
        $display("tern %b", A ? '1 : A);
        $display("tern %b", A ? A : '1);
    end
endmodule

module M(a, b, c, d);
    parameter W = 1;
    parameter type T = logic;
    input T [W+0:1] a;
    input T [W+1:1] b;
    input T [W+2:1] c;
    input T [W+3:1] d;
    initial $display("M W=%0d %b %b %b %b", W, a, b, c, d);
endmodule
