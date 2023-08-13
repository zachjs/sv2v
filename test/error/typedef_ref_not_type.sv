// pattern: expected interface-based typename, but found net x.y
// location: typedef_ref_not_type.sv:7:5
module top;
    if (1) begin : x
        wire y;
    end
    typedef x.y T;
    T x;
endmodule
