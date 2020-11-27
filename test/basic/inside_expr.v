module top;

    function integer sideEffect;
        input integer inp;
        begin
            $display("sideEffect(%b)", inp);
            sideEffect = inp;
        end
    endfunction

    initial begin : foo
        reg [1:0] a;
        for (a = 0; a < 3; a++) begin
            if (a == 2'b01 || a == 2'b00)
                $display("fizz");
            if (a == 2'b10)
                $display("buzz");
        end
    end

    initial $display("A", 1'b0);
    initial $display("B", 1'bx);
    initial $display("C", 1'bx);
    initial $display("D", 1'bx);
    initial $display("E", 1'bx);
    // TODO: Add support for inside expressions with side effects.
    // initial begin : bar
    //     integer tmp;
    //     tmp = sideEffect(3'bz11);
    //     $display("F", 1'b1);
    //     tmp = sideEffect(3'b?11);
    //     $display("G", 1'b1);
    // end

    function test1;
        input [2:0] inp;
        test1 = inp[0] == 1 && inp[2] == 1;
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

    wire [0:2][31:0] arr;
    assign arr = { 32'd60, 32'd61, 32'd63 };
    function test2;
        input integer inp;
        integer i;
        begin
            test2 = 0;
            for (i = 0; i < 3; ++i)
                test2 = test2 || (inp == arr[i]);
            test2 = test2 || (16 <= inp && inp <= 23) || (32 <= inp && inp <= 47);
        end
    endfunction
    initial begin : foobar
        integer i;
        for (i = 0; i < 64; ++i)
            $display("test2(%02d) = %b", i, test2(i));
    end

    function [0:31] test3;
        input integer inp;
        begin
            inp = 1 + sideEffect(inp);
            if (16 <= inp && inp <= 23) test3 = 1;
            else if (32 <= inp && inp <= 47) test3 = 2;
            else if (inp == 0 || (60 <= inp && inp <= 61) || inp == 4) test3 = 3;
            else test3 = 0;
        end
    endfunction
    initial begin : block3
        integer i;
        for (i = 0; i < 64; ++i)
            $display("test3(%02d) = %b", i, test3(i));
    end

endmodule
