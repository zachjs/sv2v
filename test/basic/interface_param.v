module M(data);
    parameter A = 1;
    parameter WIDTH = 32;
    parameter B = 2;
    input wire [WIDTH-1:0] data;
    initial begin
        $display("A %b", A);
        $display("I.P %b", data);
        $display("B %b", B);
    end
endmodule

module top;
    wire [31:0] x_data = 0;
    wire [9:0] y_data = 0;
    M #(.WIDTH(32)) a(x_data);
    M #(.WIDTH(10)) b(y_data);
    M #(3, 32, 4) c(x_data);
    M #(5, 10, 6) d(y_data);
endmodule
