module top;
    wire (supply0, supply1) a = 1;
    wire (strong1, strong0) b = 1;
    wire (pull0, highz1) c = 1;
    wire (pull1, highz0) d = 1;
    wire (highz0, weak1) e = 1;
    wire (highz1, weak0) f = 1;

    wire u, v, w, x, y, z;
    assign (supply0, supply1) u = 1;
    assign (strong1, strong0) v = 1;
    assign (pull0, highz1) w = 1;
    assign (pull1, highz0) x = 1;
    assign (highz0, weak1) y = 1;
    assign (highz1, weak0) z = 1;
endmodule
