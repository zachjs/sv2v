`define PRINT(arr, a, b) \
    $display(arr[0+a][0+b]); \
    $display(arr[0+a][1+b]); \
    $display(arr[0+a][2+b]); \
    $display(arr[1+a][0+b]); \
    $display(arr[1+a][1+b]); \
    $display(arr[1+a][2+b]); \
    $display(arr[2+a][0+b]); \
    $display(arr[2+a][1+b]); \
    $display(arr[2+a][2+b]); \
    $display(arr[3+a][0+b]); \
    $display(arr[3+a][1+b]); \
    $display(arr[3+a][2+b]); \
    $display(arr[4+a][0+b]); \
    $display(arr[4+a][1+b]); \
    $display(arr[4+a][2+b]);

module Example;

    typedef logic [2:0] Pack;
    Pack [4:0] arr1;
    Pack [4:0] arr2;
    Pack [4:0] arr3;
    initial begin
        arr1 = 'b100101010100100;
        arr1[0][1] = ~arr1[0][1];
        arr1[4][2] = ~arr1[4][2];
        `PRINT(arr1, 0, 0)
        arr2 = 'b100101000110101;
        `PRINT(arr2, 0, 0)
        arr3 = 'b100100111101010;
        arr3[1] = arr3[2];
        `PRINT(arr3, 0, 0)
    end

    Pack [5:1] arr4;
    Pack [5:1] arr5;
    Pack [5:1] arr6;
    initial begin
        arr4 = 'b100101010100100;
        arr4[1][1] = ~arr4[1][1];
        arr4[5][2] = ~arr4[5][2];
        `PRINT(arr4, 1, 0)
        arr5 = 'b100101000110101;
        `PRINT(arr5, 1, 0)
        arr6 = 'b100100111101010;
        arr6[2] = arr6[3];
        `PRINT(arr6, 1, 0)
    end

    Pack [1:5] arr7;
    Pack [1:5] arr8;
    Pack [1:5] arr9;
    initial begin
        arr7 = 'b100101010100100;
        arr7[1][1] = ~arr7[1][1];
        arr7[5][2] = ~arr7[5][2];
        `PRINT(arr7, 1, 0)
        arr8 = 'b100101000110101;
        `PRINT(arr8, 1, 0)
        arr9 = 'b100100111101010;
        arr9[2] = arr9[3];
        `PRINT(arr9, 1, 0)
    end

endmodule
