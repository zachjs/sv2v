// pattern: expected typename, but found localparam identifier "L"
// location: typedef_not_type_localparam.sv:5:5
module top;
    localparam L = 0;
    L x;
endmodule
