module top;
    function automatic [31:0] flatten;
        input [0:3][7:0] inp;
        flatten = {inp[0], inp[1], inp[2], inp[3]};
    endfunction
    task dump;
        input [0:3][7:0] inp;
        $display("t(%b)", {inp[0], inp[1], inp[2], inp[3]});
    endtask
    reg [0:3][7:0] arr1, arr2;
    wire [31:0] flat = flatten(arr1) | 1'b1;
    initial begin
        #1 arr1[0] = 1;
        #1 arr1[1] = 3;
        #1 arr1[2] = 9;
        #1 arr1[3] = 7;
        #1 arr2[0] = 1; dump(arr2);
        #1 arr2[1] = 3; dump(arr2);
        #1 arr2[2] = 9; dump(arr2);
        #1 arr2[3] = 7; dump(arr2);
    end
    reg [7:0] arr3 [0:3];
    wire [31:0] arr3_flat = {arr3[0], arr3[1], arr3[2], arr3[3]};
    initial $readmemh("tf_unpacked_input.mem", arr3);
endmodule
