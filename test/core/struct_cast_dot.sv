module top;
    localparam type U = struct packed { integer s; };
    localparam type T = struct packed { U x, y, z; };
    localparam T L = '{x: '{s: 1}, y: '{s: 2}, z: '{s: 3}};
    logic [L.x.s-1:0] a;
    logic [T'(L).y.s-1:0] b;
    logic [U'(T'(L).z).s-1:0] c;
endmodule
