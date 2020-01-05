module Example;

    initial
        $monitor("%b %b %b %b %b %b %b %b %b",
            arr1, arr2, arr3,
            arr4, arr5, arr6,
            arr7, arr8, arr9
        );

    typedef logic [2:0] Pack;

    Pack [4:0] arr1;
    Pack [4:0] arr2;
    Pack [4:0] arr3;
    initial begin
        #1; arr1 = 'b100101010100100;
        #1; arr1[0][1] = ~arr1[0][1];
        #1; arr1[4][2] = ~arr1[4][2];
        #1; arr2 = 'b100101000110101;
        #1; arr3 = 'b100100111101010;
        #1; arr3[1] = arr3[2];
    end

    Pack [5:1] arr4;
    Pack [5:1] arr5;
    Pack [5:1] arr6;
    initial begin
        #1; arr4 = 'b100101010100100;
        #1; arr4[1][1] = ~arr4[1][1];
        #1; arr4[5][2] = ~arr4[5][2];
        #1; arr5 = 'b100101000110101;
        #1; arr6 = 'b100100111101010;
        #1; arr6[2] = arr6[3];
    end

    Pack [1:5] arr7;
    Pack [1:5] arr8;
    Pack [1:5] arr9;
    initial begin
        #1; arr7 = 'b100101010100100;
        #1; arr7[1][1] = ~arr7[1][1];
        #1; arr7[5][2] = ~arr7[5][2];
        #1; arr8 = 'b100101000110101;
        #1; arr9 = 'b100100111101010;
        #1; arr9[2] = arr9[3];
    end

endmodule
