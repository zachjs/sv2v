module top;
    function automatic logic [31:0] lcg(input logic [31:0] x);
        automatic logic [3:0] temp;
        lcg = x;
        for (temp = 0; temp < 3; temp++) begin
            lcg *= 1664525;
            lcg += 1013904223;
        end
    endfunction

    initial $display(lcg(0));
    initial $display(lcg(1));
    initial $display(lcg(2));
    initial $display(lcg(3));
    initial $display(lcg(4));
    initial $display(lcg(5));
endmodule
