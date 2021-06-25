module Example;

    initial
        $monitor("%b %b %b %b %b %b %b %b %b",
            arr1, arr2, arr3,
            arr4, arr5, arr6,
            arr7, arr8, arr9
        );

    reg [14:0] arr1;
    reg [14:0] arr2;
    reg [14:0] arr3;
    initial begin
        #1; arr1 = 'b100101010100100;
        #1; arr1[0*3+1] = ~arr1[0*3+1];
        #1; arr1[4*3+2] = ~arr1[4*3+2];
        #1; arr2 = 'b100101000110101;
        #1; arr3 = 'b100100111101010;
        #1; arr3[1*3+:3] = arr3[2*3+:3];
    end

    reg [14:0] arr4;
    reg [14:0] arr5;
    reg [14:0] arr6;
    initial begin
        #1; arr4 = 'b100101010100100;
        #1; arr4[0*3+1] = ~arr4[0*3+1];
        #1; arr4[4*3+2] = ~arr4[4*3+2];
        #1; arr5 = 'b100101000110101;
        #1; arr6 = 'b100100111101010;
        #1; arr6[1*3+:3] = arr6[2*3+:3];
    end

    reg [14:0] arr7;
    reg [14:0] arr8;
    reg [14:0] arr9;
    initial begin
        #1; arr7 = 'b100101010100100;
        #1; arr7[(4-0)*3+1] = ~arr7[(4-0)*3+1];
        #1; arr7[(4-4)*3+2] = ~arr7[(4-4)*3+2];
        #1; arr8 = 'b100101000110101;
        #1; arr9 = 'b100100111101010;
        #1; arr9[(4-1)*3+:3] = arr9[(4-2)*3+:3];
    end

endmodule
