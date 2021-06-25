module top;
    `include "multipack_prec.vh"

    assign arr2[0][0] = arr1[0][0];
    assign arr2[0][1] = arr1[0][1];
    assign arr2[0][3:2] = arr1[0][3:2];
    assign arr2[1][0+:2] = arr1[1][0+:2];
    assign arr2[1][3-:2] = arr1[1][3-:2];
endmodule
