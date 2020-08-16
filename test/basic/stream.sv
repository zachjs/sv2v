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

endmodule
