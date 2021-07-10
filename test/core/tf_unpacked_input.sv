module top;
    function automatic [31:0] flatten;
        input byte inp [4];
        return {inp[0], inp[1], inp[2], inp[3]};
    endfunction
    task dump;
        input byte inp [4];
        $display("t(%b)", {inp[0], inp[1], inp[2], inp[3]});
    endtask
    byte arr1 [4];
    byte arr2 [4];
    wire integer unsigned flat = flatten(arr1) | 1'b1;
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
    byte arr3 [4];
    wire [31:0] arr3_flat = {arr3[0], arr3[1], arr3[2], arr3[3]};
    initial $readmemh("tf_unpacked_input.mem", arr3);
endmodule
