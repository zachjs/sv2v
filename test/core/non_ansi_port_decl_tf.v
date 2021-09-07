module top;
    function automatic [4:0] f;
        input reg signed [3:0] x;
        f = x;
    endfunction
    task t;
        input signed [3:0] x;
        reg [4:0] z;
        begin
            z = x;
            $display("t(%b) -> %b", x, z);
        end
    endtask
    localparam [3:0] X = 4'b1011;
    localparam [4:0] Y = f(X);
    initial $display("f(%b) -> %b", X, Y);
    initial t(X);
endmodule
