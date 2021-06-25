module top;
    function automatic [7:0] f;
        input [2:0] p;
        begin
            f = 7'b0;
            f[p+:2] = 2'b11;
        end
    endfunction
    reg [2:0] p;
    wire [7:0] q;
    assign q = f(p);
    initial begin
        $monitor("%0d, p=%b q=%b", $time, p, q);
        #1 p = 0;
        while (p != 7)
            #1 p = p + 1;
    end
endmodule
