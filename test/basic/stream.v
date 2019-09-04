module top;

    initial begin
        $display("packet addr = %b", {2'b01, 2'b10});
        $display("packet data = %b", {2'b00, 2'b11});
    end

    function automatic [23:0] pack_r_8_24;
        input [23:0] in;
        integer i;
        for (i = 0; i < 24; i = i + 8) begin
            pack_r_8_24[i+:8] = in[i+:8];
        end
    endfunction
    function automatic [23:0] pack_l_8_24;
        input [23:0] in;
        integer i;
        for (i = 0; i < 24; i = i + 8) begin
            pack_l_8_24[i+:8] = in[23-i-:8];
        end
    endfunction
    function automatic [23:0] pack_r_1_24;
        input [23:0] in;
        integer i;
        for (i = 0; i < 24; i = i + 1) begin
            pack_r_1_24[i] = in[i];
        end
    endfunction
    function automatic [23:0] pack_l_1_24;
        input [23:0] in;
        integer i;
        for (i = 0; i < 24; i = i + 1) begin
            pack_l_1_24[i] = in[23-i];
        end
    endfunction
    function automatic [23:0] pack_r_7_24;
        input [23:0] in;
        integer i;
        begin
        for (i = 0; i < 24; i = i + 7) begin
            pack_r_7_24[i+:7] = in[i+:7];
        end
        for (i = 0; i < 24; i = i + 7) begin
            pack_r_7_24[i+:7] = in[i+:7];
        end
        end
    endfunction
    function automatic [23:0] pack_l_7_24;
        input [23:0] in;
        integer i;
        integer e;
        begin
        for (i = 0; i < 24; i = i + 7) begin
            pack_l_7_24[23-i-:7] = in[i+:7];
        end
        e = i - 7;
        for (i = 0; i < 24-e; i = i + 1) begin
            pack_l_7_24[i] = in[i+e];
        end
        end
    endfunction
    initial begin
        $display("%h", pack_r_8_24(24'h060708));
        $display("%h", pack_l_8_24(24'h060708));

        $display("%h", pack_r_1_24(24'h060708));
        $display("%h", pack_l_1_24(24'h060708));

        $display("%h", pack_r_7_24(24'h060708));
        $display("%h", pack_l_7_24(24'h060708));

        $display("%h", pack_r_7_24(24'h607080));
        //$display("%h", pack_l_7_24(24'h0c0708));

        $display("%h", pack_r_7_24(24'h070800));
        $display("%h", pack_l_7_24(24'h000708));
    end

    task test_unpack;
        input [23:0] in;
        reg [0:0] i;
        reg [1:0] j;
        reg [2:0] k;
        reg [5:0] l;
        reg [11:0] m;
        begin
            {i, j, k, l, m} = in;
            $display("%b %b %b %b %b", i, j, k, l, m);
            {i, j, k, l, m} = pack_l_8_24(in);
            $display("%b %b %b %b %b", i, j, k, l, m);
            {i, j, k, l, m} = in;
            $display("%b %b %b %b %b", i, j, k, l, m);
            {i, j, k, l, m} = pack_l_1_24(in);
            $display("%b %b %b %b %b", i, j, k, l, m);
            {i, j, k, l, m} = in;
            $display("%b %b %b %b %b", i, j, k, l, m);
            {i, j, k, l, m} = pack_l_7_24(in);
            $display("%b %b %b %b %b", i, j, k, l, m);
        end
    endtask
    initial begin
        test_unpack(24'h060708);
        test_unpack(24'hC02375);
        test_unpack(24'h12E3B8);
    end

endmodule

// 060708
// 080706
// 060708
// 10e060
// 060708
// 1038c0
//

// 1 11 010 110001 100000000110
// 0 00 110 111000 010001100000
//
//  00 001 100000 011100001000
// 0 00 010 000000 011100000110
// 0 00 001 100000 011100001000
// 0 00 100 001110 000001100000
// 0 00 001 100000 011100001000
// 0 00 110 000100 000010000011
// 1 10 000 000010 001101110101
// 0 11 101 010010 001111000000
// 1 10 000 000010 001101110101
// 1 01 011 101100 010000000011
// 1 10 000 000010 001101110101
// 0 00 110 111000 010001100000
// 0 00 100 101110 001110111000
// 1 01 110 001110 001100010010
// 0 00 100 101110 001110111000
// 0 00 111 011100 011101001000
// 0 00 100 101110 001110111000
// 0 00 111 011101 110000001001


// 0 00 001 100000 011100001000
// 0 00 010 000000 011100000110
// 0 00 001 100000 011100001000
// 0 00 100 001110 000001100000
// 0 00 001 100000 011100001000
// 0 00 100 000011 100011000000
// 1 10 000 000010 001101110101
// 0 11 101 010010 001111000000
// 1 10 000 000010 001101110101
// 1 01 011 101100 010000000011
// 1 10 000 000010 001101110101
// 1 11 010 110001 100000000110
// 0 00 100 101110 001110111000
// 1 01 110 001110 001100010010
// 0 00 100 101110 001110111000
// 0 00 111 011100 011101001000
// 0 00 100 101110 001110111000
// 0 11 100 010001 111001011000
