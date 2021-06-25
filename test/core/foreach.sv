module top;
    logic [1:0] foo [4] = {2'b10,2'b01,2'b11,2'b00};
    initial
        foreach (foo [ x ])
            $display(x, foo[x]);

    // from the SystemVerilog-2017 specification
    int A [2][3][4];
    bit [3:0][2:1] B [5:1][4];
    initial begin
        A = 0;
        B = 0;
        foreach( A [ i, j, k ] )
            $display(i, j, k);
        foreach( B [ q, r, , s ] )
            $display(q, r, s);
    end
endmodule
