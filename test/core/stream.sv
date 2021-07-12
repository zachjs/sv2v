module top;

    // Derived from: https://www.amiq.com/consulting/2017/05/29/how-to-pack-data-using-systemverilog-streaming-operators/
    typedef struct packed {
        logic [3:0] addr;
        logic [3:0] data;
    } packet_t;
    initial begin
        logic [1:0] array[] = '{ 2'b10, 2'b01, 2'b11, 2'b00 };
        packet_t packet = {<<4{ {<<2{array}} }};
        $display("packet addr = %b", packet.addr);
        $display("packet data = %b", packet.data);
    end

    function automatic [23:0] indirect;
        input [23:0] inp;
        indirect = inp;
    endfunction

    task printer;
        input [23:0] inpA;
        input [23:0] inpB;
        $display("printer(%h, %h)", inpA, inpB);
    endtask

    initial begin
        logic [23:0] temp;

        temp = {>>byte{24'h060708}};
        $display("%h", temp);
        temp = {<<byte{24'h060708}};
        $display("%h", temp);

        temp = {>>bit{24'h060708}};
        $display("%h", temp);
        temp = {<<bit{24'h060708}};
        $display("%h", temp);

        temp = {>>7{24'h060708}};
        $display("%h", temp);
        temp = {<<7{24'h060708}};
        $display("%h", temp);

        temp = {>>7{20'h60708}};
        $display("%h", temp);
        temp = {<<7{20'h60708}};
        $display("%h", temp);

        temp = {>>7{16'h0708}};
        $display("%h", temp);
        temp = {<<7{16'h0708}};
        $display("%h", temp);

        temp = indirect({>>7{16'h0708}});
        $display("%h", temp);
        temp = indirect({<<7{16'h0708}});
        $display("%h", temp);

        printer({>>7{16'h0708}}, {<<7{16'h0708}});
    end

    reg [23:0] init_simple_stream_var = {<<7{16'h0718}};
    wire [23:0] init_simple_stream_net = {<<7{16'h0728}};
    reg [23:0] init_indirect_stream_var = indirect({<<7{16'h0738}});
    wire [23:0] init_indirect_stream_net = indirect({<<7{16'h0748}});

    reg [23:0] asgn_simple_stream_var;
    wire [23:0] asgn_simple_stream_net;
    reg [23:0] asgn_indirect_stream_var;
    wire [23:0] asgn_indirect_stream_net;
    initial asgn_simple_stream_var = {<<7{16'h0758}};
    assign asgn_simple_stream_net = {<<7{16'h0768}};
    initial asgn_indirect_stream_var = indirect({<<7{16'h0778}});
    assign asgn_indirect_stream_net = indirect({<<7{16'h0788}});

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
        input logic [23:0] in;
        logic [0:0] i;
        logic [1:0] j;
        logic [2:0] k;
        logic [5:0] l;
        logic [11:0] m;
        {>>byte{i, j, k, l, m}} = in;
        $display("%b %b %b %b %b", i, j, k, l, m);
        {<<byte{i, j, k, l, m}} = in;
        $display("%b %b %b %b %b", i, j, k, l, m);
        {>>bit{i, j, k, l, m}} = in;
        $display("%b %b %b %b %b", i, j, k, l, m);
        {<<bit{i, j, k, l, m}} = in;
        $display("%b %b %b %b %b", i, j, k, l, m);
        {>>7{i, j, k, l, m}} = in;
        $display("%b %b %b %b %b", i, j, k, l, m);
        {<<7{i, j, k, l, m}} = in;
        $display("%b %b %b %b %b", i, j, k, l, m);
    endtask
    initial begin
        test_unpack(24'h060708);
        test_unpack(24'hC02375);
        test_unpack(24'h12E3B8);
    end

    logic [0:0] i;
    logic [1:0] j;
    logic [2:0] k;
    logic [5:0] l;
    logic [11:0] m;
    logic [23:0] in;
    assign {<<7{i, j, k, l, m}} = in;
    initial begin
        #1 in = 24'h060708;
        #1 in = 24'hC02375;
        #1 in = 24'h12E3B8;
    end

endmodule
