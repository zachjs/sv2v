class P #(
    parameter WIDTH = 1,
    parameter type BASE = logic
);
    typedef BASE [WIDTH - 1:0] Unit;
endclass

`define DUMP \
    begin \
        a = '1; \
        b = '1; \
        c = '1; \
        d = '1; \
        e = '1; \
        $display("%b %b %b %b %b", a, b, c, d, e); \
    end

module top;
    localparam X = 2;
    localparam type T = logic [31:0];
    P#()::Unit a;
    P#(X)::Unit b;
    P#(X, T)::Unit c;
    P#(.WIDTH(X))::Unit d;
    P#(.BASE(T))::Unit e;
    initial `DUMP
    if (1) begin : blk
        localparam X = 3;
        localparam type T = logic [7:0];
        P#()::Unit a;
        P#(X)::Unit b;
        P#(X, T)::Unit c;
        P#(.WIDTH(X))::Unit d;
        P#(.BASE(T))::Unit e;
        initial `DUMP
    end
    if (1) begin : route
        localparam X = 3;
        localparam type T = logic [7:0];
        initial begin
            begin
                P#()::Unit a;
                P#(X)::Unit b;
                P#(X, T)::Unit c;
                P#(.WIDTH(X))::Unit d;
                P#(.BASE(T))::Unit e;
                `DUMP
            end
            begin
                P#()::Unit a;
                P#(X)::Unit b;
                P#(X, T)::Unit c;
                P#(.WIDTH(X))::Unit d;
                P#(.BASE(T))::Unit e;
                `DUMP
            end
            begin
                P#()::Unit a;
                P#(X)::Unit b;
                P#(X, T)::Unit c;
                P#(.WIDTH(X))::Unit d;
                P#(.BASE(T))::Unit e;
                `DUMP
            end
        end
    end
endmodule
