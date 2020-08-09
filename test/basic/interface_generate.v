module ClientAnd (client_req, bits);
    parameter WIDTH = 2;
    output client_req;
    input [WIDTH-1:0] bits;
    assign client_req = &bits;
endmodule

module ClientTick #(
    parameter start = 0,
    parameter period = 1
) (
    output reg client_req,
    input clock
);
    initial client_req = start;
    integer counter;
    initial counter = 0;
    always @(posedge clock) begin
        counter += 1;
        if (counter % period == 0)
            client_req = ~client_req;
    end
endmodule

module top;
    reg clock;
    initial begin
        clock = 1;
        forever #1 clock = ~clock;
    end

    parameter N = 8;


    generate
        begin : intf
            wire [N-1:0] req;
        end
        genvar j;
        for (j = 0; j < N - 1; j = j + 1) begin : clients
            ClientTick #(j, j + 1) client(
                .clock,
                .client_req(intf.req[j + 1])
            );
        end
    endgenerate

    ClientAnd #(4) client(
        .bits(intf.req[4:1]),
        .client_req(intf.req[0])
    );

    initial begin
        $monitor("%0d %b %b", $time, clock, intf.req);
        #100 $finish;
    end
endmodule
