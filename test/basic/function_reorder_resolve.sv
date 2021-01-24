module top;
    parameter YES = 1;
    if (YES) begin
        function automatic [31:0] help;
            input [31:0] inp;
            help = inp + 1;
        endfunction
        initial $display("A %0d", help(0));
    end
    function automatic [31:0] help;
        input [31:0] inp;
        help = inp + 2;
    endfunction
    initial $display("B %0d", help(0));
endmodule
