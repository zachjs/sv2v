function automatic [4:0] f;
    input signed [3:0] x;
    reg [3:0] x;
    var type(x) y;
    y = x;
    f = y;
endfunction
module top;
    task t;
        input signed [3:0] x;
        reg [3:0] x;
        var type(x) y;
        reg [4:0] z;
        y = x;
        z = y;
        $display("t(%b) -> %b", x, z);
    endtask
    localparam [3:0] X = 4'b1011;
    localparam [4:0] Y = f(X);
    initial $display("f(%b) -> %b", X, Y);
    initial t(X);
endmodule
