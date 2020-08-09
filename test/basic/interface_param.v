module M(data);
    parameter A = 1;
    parameter WIDTH = 32;
    parameter B = 2;
    localparam OFFSET = 1;
    input wire [WIDTH-OFFSET:0] data;
    initial begin
        $display("A %b", A);
        $display("I.P %b", data);
        $display("B %b", B);
    end
endmodule

module top;
    generate
        begin : x
            wire [31:0] data = 0;
        end
        begin : y
            wire [9:0] data = 0;
        end
    endgenerate
    M #(.WIDTH(32)) a(x.data);
    M #(.WIDTH(10)) b(y.data);
    M #(3, 32, 4) c(x.data);
    M #(5, 10, 6) d(y.data);
endmodule
