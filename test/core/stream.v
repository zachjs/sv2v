module top;

    initial begin
        $display("packet addr = %b", {2'b01, 2'b10});
        $display("packet data = %b", {2'b00, 2'b11});
    end

    task printer;
        input [23:0] inpA;
        input [23:0] inpB;
        $display("printer(%h, %h)", inpA, inpB);
    endtask

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
        for (i = 0; i < 24; i = i + 7) begin
            pack_r_7_24[i+:7] = in[i+:7];
        end
    endfunction
    function automatic [23:0] pack_l_7_24;
        input [23:0] in;
        integer i;
        begin
        for (i = 0; i + 7 < 24; i = i + 7) begin
            pack_l_7_24[23-i-:7] = in[i+:7];
        end
        pack_l_7_24[0+:24%7] = in[i+:24%7];
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
        $display("%h", pack_l_7_24(24'h0c0708));

        $display("%h", pack_r_7_24(24'h070800));
        $display("%h", pack_l_7_24(24'h000708));

        $display("%h", pack_r_7_24(24'h070800));
        $display("%h", pack_l_7_24(24'h000708));

        printer(pack_r_7_24(24'h070800), pack_l_7_24(24'h000708));
    end

    reg [23:0] init_simple_stream_var = pack_l_7_24(24'h000718);
    wire [23:0] init_simple_stream_net = pack_l_7_24(24'h000728);
    reg [23:0] init_indirect_stream_var = pack_l_7_24(24'h000738);
    wire [23:0] init_indirect_stream_net = pack_l_7_24(24'h000748);

    reg [23:0] asgn_simple_stream_var = pack_l_7_24(24'h000758);
    wire [23:0] asgn_simple_stream_net = pack_l_7_24(24'h000768);
    reg [23:0] asgn_indirect_stream_var = pack_l_7_24(24'h000778);
    wire [23:0] asgn_indirect_stream_net = pack_l_7_24(24'h000788);

    initial begin
        #1;
        $display("%b", init_simple_stream_var);
        $display("%b", init_simple_stream_net);
        $display("%b", init_indirect_stream_var);
        $display("%b", init_indirect_stream_net);
        $display("%b", asgn_simple_stream_var);
        $display("%b", asgn_simple_stream_net);
        $display("%b", asgn_indirect_stream_var);
        $display("%b", asgn_indirect_stream_net);
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

    wire [0:0] i;
    wire [1:0] j;
    wire [2:0] k;
    wire [5:0] l;
    wire [11:0] m;
    reg [23:0] in;
    assign {i, j, k, l, m} = pack_l_7_24(in);
    initial begin
        #1 in = 24'h060708;
        #1 in = 24'hC02375;
        #1 in = 24'h12E3B8;
    end

endmodule
