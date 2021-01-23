package Pkg;
    localparam integer X = func(1);
    function automatic integer func;
        input integer inp;
        func = inp * 2;
    endfunction
endpackage

module top;
    initial $display(Pkg::X);
endmodule
