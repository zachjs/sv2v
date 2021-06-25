interface Interface #(num_clients = 0);
    bit [num_clients-1:0] req;
    for (genvar i = 0; i < num_clients; ++i) begin : mps
        modport client_mp (output .client_req(req[i]));
    end
endinterface

module ClientAnd (client_ifc, bits);
    parameter WIDTH = 2;
    Interface client_ifc;
    input [WIDTH-1:0] bits;
    assign client_ifc.client_req = &bits;
endmodule

module ClientTick #(start = 0, period = 1) (Interface client_ifc, input clock);
    initial client_ifc.client_req = start;
    integer counter;
    initial counter = 0;
    always @(posedge clock) begin
        counter += 1;
        if (counter % period == 0)
            client_ifc.client_req = ~client_ifc.client_req;
    end
endmodule

module top;
    logic clock;
    initial begin
        clock = 1;
        forever #1 clock = ~clock;
    end

    parameter N = 8;

    Interface #(.num_clients(N)) intf();

    for (genvar j = 0; j < N - 1; j++) begin : clients
        ClientTick #(j, j + 1) client(
            .clock,
            .client_ifc(intf.mps[j + 1].client_mp)
        );
    end

    ClientAnd #(4) client(
        .bits(intf.req[4:1]),
        .client_ifc(intf.mps[0].client_mp)
    );

    initial begin
        $monitor("%0d %b %b", $time, clock, intf.req);
        #100 $finish;
    end
endmodule
