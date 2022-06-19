`define ASSERT_SIGNEDNESS(expr, signedness_str, signedness_val) \
    initial begin \
        type(expr) typeof; \
        logic signed [$bits(expr):0] ones; \
        logic [$bits(expr):0] extd; \
        typeof = 1'sb1; \
        ones = 1'sb1; \
        extd = typeof; \
        if (signedness_val != (extd == ones)) \
            $display(`"FAIL: expected (expr) to be treated as signedness_str extd=%b`", extd); \
    end

`define ASSERT_SIGNED(expr) `ASSERT_SIGNEDNESS(expr, signed, 1)
`define ASSERT_UNSIGNED(expr) `ASSERT_SIGNEDNESS(expr, unsigned, 0)

`define MAKE_PRIM(typ) \
    typ typ``_unspecified; \
    typ unsigned typ``_unsigned; \
    typ signed typ``_signed; \
    initial typ``_unspecified = 1; \
    initial typ``_unsigned = 1; \
    initial typ``_signed = 1; \
    `ASSERT_SIGNED(typ``_signed) \
    `ASSERT_UNSIGNED(typ``_unsigned)

`define MAKE_PRIM_SIGNED(typ) \
    `MAKE_PRIM(typ) \
    `ASSERT_SIGNED(typ``_unspecified)
`define MAKE_PRIM_UNSIGNED(typ) \
    `MAKE_PRIM(typ) \
    `ASSERT_UNSIGNED(typ``_unspecified)

module top;
    logic signed x;
    logic signed [1:0] y;
    assign x = 0;
    assign y = 2;
    type(x % y) z;
    assign z = x % y;
    logic [3:0] w;
    assign w = z;
    initial #1 $display("%b %b %b %b", x, y, z, w);

    `MAKE_PRIM_SIGNED(byte)
    `MAKE_PRIM_SIGNED(shortint)
    `MAKE_PRIM_SIGNED(int)
    `MAKE_PRIM_SIGNED(integer)
    `MAKE_PRIM_SIGNED(longint)

    `MAKE_PRIM_UNSIGNED(bit)
    `MAKE_PRIM_UNSIGNED(reg)
    `MAKE_PRIM_UNSIGNED(logic)

    reg signed [5:0] arr;

    `ASSERT_SIGNED(0)
    `ASSERT_SIGNED('sb0)
    `ASSERT_UNSIGNED('b0)
    `ASSERT_UNSIGNED('o0)
    `ASSERT_UNSIGNED('d0)
    `ASSERT_UNSIGNED('h0)
    `ASSERT_SIGNED('sb0)
    `ASSERT_SIGNED('so0)
    `ASSERT_SIGNED('sd0)
    `ASSERT_SIGNED('sh0)

    `ASSERT_UNSIGNED({1'b0})
    `ASSERT_UNSIGNED({1'o0})
    `ASSERT_UNSIGNED({1'd0})
    `ASSERT_UNSIGNED({1'h0})
    `ASSERT_UNSIGNED({1'sb0})
    `ASSERT_UNSIGNED({1'so0})
    `ASSERT_UNSIGNED({1'sd0})
    `ASSERT_UNSIGNED({1'sh0})
    `ASSERT_UNSIGNED({reg_unspecified})
    `ASSERT_UNSIGNED({reg_unsigned})
    `ASSERT_UNSIGNED({reg_signed})

    `ASSERT_UNSIGNED({1 {1'b0}})
    `ASSERT_UNSIGNED({1 {1'o0}})
    `ASSERT_UNSIGNED({1 {1'd0}})
    `ASSERT_UNSIGNED({1 {1'h0}})
    `ASSERT_UNSIGNED({1 {1'sb0}})
    `ASSERT_UNSIGNED({1 {1'so0}})
    `ASSERT_UNSIGNED({1 {1'sd0}})
    `ASSERT_UNSIGNED({1 {1'sh0}})
    `ASSERT_UNSIGNED({1 {reg_unspecified}})
    `ASSERT_UNSIGNED({1 {reg_unsigned}})
    `ASSERT_UNSIGNED({1 {reg_signed}})

    `ASSERT_SIGNED($signed(reg_unspecified))
    `ASSERT_SIGNED($signed(reg_unsigned))
    `ASSERT_SIGNED($signed(reg_signed))
    `ASSERT_UNSIGNED($unsigned(reg_unspecified))
    `ASSERT_UNSIGNED($unsigned(reg_unsigned))
    `ASSERT_UNSIGNED($unsigned(reg_signed))

`define UNIOP_ALWAYS_UNSIGNED(op) \
    `ASSERT_UNSIGNED(op 2'sb11) \
    `ASSERT_UNSIGNED(op 2'b11) \
    `ASSERT_UNSIGNED(op reg_signed) \
    `ASSERT_UNSIGNED(op reg_unsigned)

`define UNIOP_SAME_SIGN(op) \
    `ASSERT_SIGNED(op 2'sb11) \
    `ASSERT_UNSIGNED(op 2'b11) \
    `ASSERT_SIGNED(op reg_signed) \
    `ASSERT_UNSIGNED(op reg_unsigned)

    `UNIOP_ALWAYS_UNSIGNED(!)
    `UNIOP_SAME_SIGN(~)
    `UNIOP_SAME_SIGN(+)
    `UNIOP_SAME_SIGN(-)
    `UNIOP_ALWAYS_UNSIGNED(&)
    `UNIOP_ALWAYS_UNSIGNED(~&)
    `UNIOP_ALWAYS_UNSIGNED(|)
    `UNIOP_ALWAYS_UNSIGNED(~|)
    `UNIOP_ALWAYS_UNSIGNED(^)
    `UNIOP_ALWAYS_UNSIGNED(~^)

`define BINOP_BOTH_SIGNED(op) \
    `ASSERT_SIGNED(reg_signed op reg_signed) \
    `ASSERT_UNSIGNED(reg_unsigned op reg_signed) \
    `ASSERT_UNSIGNED(reg_signed op reg_unsigned) \
    `ASSERT_UNSIGNED(reg_unsigned op reg_unsigned) \
    `ASSERT_UNSIGNED(reg_unsigned op arr) \
    `ASSERT_UNSIGNED(arr op reg_unsigned) \
    `ASSERT_SIGNED(arr op arr) \
    `ASSERT_SIGNED(arr op reg_signed) \
    `ASSERT_SIGNED(reg_signed op arr) \
    `ASSERT_SIGNED(1'sb1 op 2'sb11) \
    `ASSERT_UNSIGNED(1'b1 op 2'sb11) \
    `ASSERT_UNSIGNED(1'sb1 op 2'b11) \
    `ASSERT_UNSIGNED(1'b1 op 2'b11)

`define BINOP_FIRST_SIGNED(op) \
    `ASSERT_SIGNED(reg_signed op reg_signed) \
    `ASSERT_UNSIGNED(reg_unsigned op reg_signed) \
    `ASSERT_SIGNED(reg_signed op reg_unsigned) \
    `ASSERT_UNSIGNED(reg_unsigned op reg_unsigned) \
    `ASSERT_SIGNED(1'sb1 op 2'sb11) \
    `ASSERT_UNSIGNED(1'b1 op 2'sb11) \
    `ASSERT_SIGNED(1'sb1 op 2'b11) \
    `ASSERT_UNSIGNED(1'b1 op 2'b11)

`define BINOP_NEVER_SIGNED(op) \
    `ASSERT_UNSIGNED(reg_signed op reg_signed) \
    `ASSERT_UNSIGNED(reg_unsigned op reg_signed) \
    `ASSERT_UNSIGNED(reg_signed op reg_unsigned) \
    `ASSERT_UNSIGNED(reg_unsigned op reg_unsigned) \
    `ASSERT_UNSIGNED(1'sb1 op 2'sb11) \
    `ASSERT_UNSIGNED(1'b1 op 2'sb11) \
    `ASSERT_UNSIGNED(1'sb1 op 2'b11) \
    `ASSERT_UNSIGNED(1'b1 op 2'b11)

    `BINOP_NEVER_SIGNED(&& )
    `BINOP_NEVER_SIGNED(|| )
    `BINOP_NEVER_SIGNED(-> )
    `BINOP_NEVER_SIGNED(<->)
    `BINOP_BOTH_SIGNED( &  )
    `BINOP_BOTH_SIGNED( ^  )
    `BINOP_BOTH_SIGNED( ~^ )
    `BINOP_BOTH_SIGNED( |  )
    `BINOP_BOTH_SIGNED( *  )
    `BINOP_BOTH_SIGNED( /  )
    `BINOP_BOTH_SIGNED( +  )
    `BINOP_BOTH_SIGNED( -  )
    `BINOP_FIRST_SIGNED(** )
    `BINOP_FIRST_SIGNED(<< )
    `BINOP_FIRST_SIGNED(>> )
    `BINOP_FIRST_SIGNED(<<<)
    `BINOP_FIRST_SIGNED(>>>)
    `BINOP_NEVER_SIGNED(== )
    `BINOP_NEVER_SIGNED(!= )
    `BINOP_NEVER_SIGNED(===)
    `BINOP_NEVER_SIGNED(!==)
    `BINOP_NEVER_SIGNED(==?)
    `BINOP_NEVER_SIGNED(!=?)
    `BINOP_NEVER_SIGNED(<  )
    `BINOP_NEVER_SIGNED(<= )
    `BINOP_NEVER_SIGNED(>  )
    `BINOP_NEVER_SIGNED(>= )

    `ASSERT_UNSIGNED(4'('0))
    `ASSERT_UNSIGNED(3'('x))
    `ASSERT_SIGNED(2'(1'sb1))
    `ASSERT_SIGNED(10'(2'sb11))
    `ASSERT_UNSIGNED(10'(2'b11))
    `ASSERT_UNSIGNED(2'(1'b1))
    `ASSERT_SIGNED(2'(reg_signed))
    `ASSERT_UNSIGNED(2'(reg_unsigned))

    `ASSERT_SIGNED(arr)
    `ASSERT_UNSIGNED(arr[0])
    `ASSERT_UNSIGNED(arr[5:0])
    `ASSERT_UNSIGNED(arr[1+:2])
    `ASSERT_UNSIGNED(arr[1-:2])
    `ASSERT_UNSIGNED(integer_signed[0])
    `ASSERT_UNSIGNED(integer_signed[1])
    `ASSERT_UNSIGNED(integer_unsigned[0])
    `ASSERT_UNSIGNED(integer_unsigned[1])

    parameter STR_P = "foo";
    localparam STR_L = "foo";
    `ASSERT_UNSIGNED(STR_P)
    `ASSERT_UNSIGNED(STR_L)
    `ASSERT_UNSIGNED(64'(STR_P))
    `ASSERT_UNSIGNED(64'(STR_L))

    struct packed {
        logic w;
        logic signed x;
        logic [1:0] y;
        logic signed [1:0] z;
        struct packed {
            logic w;
            logic signed x;
            logic [1:0] y;
            logic signed [1:0] z;
        } i;
    } s = '1;
    struct packed signed {
        logic w;
        logic signed x;
        logic [1:0] y;
        logic signed [1:0] z;
    } t = '1;
    `ASSERT_UNSIGNED(s)
    `ASSERT_UNSIGNED(s.w)
    `ASSERT_UNSIGNED(s.y)
    `ASSERT_SIGNED(s.x)
    `ASSERT_SIGNED(s.z)
    `ASSERT_UNSIGNED(s.i)
    `ASSERT_UNSIGNED(s.i.w)
    `ASSERT_UNSIGNED(s.i.y)
    `ASSERT_SIGNED(s.i.x)
    `ASSERT_SIGNED(s.i.z)
    `ASSERT_SIGNED(t)
endmodule
