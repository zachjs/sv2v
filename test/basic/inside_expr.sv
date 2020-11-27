module top;

    function integer sideEffect;
        input integer inp;
        $display("sideEffect(%b)", inp);
        sideEffect = inp;
    endfunction

    initial
        for (logic [1:0] a = 0; a < 3; a++) begin
            if (a inside {2'b01, 2'b00})
                $display("fizz");
            if (a inside {2'b10})
                $display("buzz");
        end

    initial $display("A", 3'bz11 inside {3'b?01});
    initial $display("B", 3'bz11 inside {3'b1?1});
    initial $display("C", 3'bz11 inside {3'b011});
    initial $display("D", 3'bz11 inside {3'b1?1, 3'b011});
    initial $display("E", 3'bz11 inside {3'b?01, 3'b011});
    // TODO: Add support for inside expressions with side effects.
    // initial $display("F", sideEffect(3'bz11) inside {3'b?11});
    // initial $display("G", 3'bz11 inside {sideEffect(3'b?11)});

    generate
        begin : patterns
            localparam A = 3'b1?1;
        end
    endgenerate
    function test1;
        input logic [2:0] inp;
        return inp inside {patterns.A};
    endfunction
    initial begin
        // should match
        $display("test1: %b %b", 3'b101, test1(3'b101));
        $display("test1: %b %b", 3'b111, test1(3'b111));
        $display("test1: %b %b", 3'b1x1, test1(3'b1x1));
        $display("test1: %b %b", 3'b1z1, test1(3'b1z1));
        // shouldn't match
        $display("test1: %b %b", 3'b001, test1(3'b001));
        $display("test1: %b %b", 3'b011, test1(3'b011));
        $display("test1: %b %b", 3'b0x1, test1(3'b0x1));
        $display("test1: %b %b", 3'b0z1, test1(3'b0z1));
    end

    integer arr [] = { 32'd60, 32'd61, 32'd63 };
    function test2;
        input integer inp;
        // TODO: Add support for array value ranges.
        test2 = 0;
        for (integer i = 0; i < 3; ++i)
            if (inp == arr[i])
                return 1'b1;
        return test2 || inp inside { [16:23], [32:47] };
    endfunction
    initial begin
        for (integer i = 0; i < 64; ++i)
            $display("test2(%02d) = %b", i, test2(i));
    end

    function integer test3;
        input integer inp;
        case (1 + sideEffect(inp)) inside
            [16:23]: return 1;
            [32:47]: return 2;
            default: return 0;
            0, [60:61], 4: return 3;
        endcase
    endfunction
    initial begin
        for (integer i = 0; i < 64; ++i)
            $display("test3(%02d) = %b", i, test3(i));
    end

endmodule
