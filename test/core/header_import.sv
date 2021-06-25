package P;
    localparam FOO = 10;
endpackage
module top
    import P::FOO;
    ;
    initial begin
        $display(FOO);
    end
endmodule
