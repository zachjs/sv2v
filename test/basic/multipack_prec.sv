module top;
    wire [3:0][2:0] arr1 [0:1];
    wire [0:1][3:0][2:0] arr2;

    assign arr1[0][0] = 3'b001;
    assign arr1[0][1] = 3'b011;
    assign arr1[0][2] = 3'b100;
    assign arr1[0][3] = 3'b010;
    assign arr1[1][0] = 3'b110;
    assign arr1[1][1] = 3'b100;
    assign arr1[1][2] = 3'b010;
    assign arr1[1][3] = 3'b101;

    assign arr2[0][0] = arr1[0][0];
    assign arr2[0][1] = arr1[0][1];
    assign arr2[0][3:2] = arr1[0][3:2];
    assign arr2[1][0+:2] = arr1[1][0+:2];
    assign arr2[1][3-:2] = arr1[1][3-:2];
endmodule
