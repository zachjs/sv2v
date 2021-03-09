// pattern: encountered break inside fork-join
module top;
    final
        while (1)
        fork
            break;
        join
endmodule
