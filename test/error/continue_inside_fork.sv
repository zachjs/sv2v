// pattern: encountered continue inside fork-join
module top;
    initial
        while (1)
        fork
            continue;
        join
endmodule
