module top;
    reg [3:0] idx;
    reg [14:0] data;
    mod m(idx, data);
    initial begin
        #1 data = 0;
        #1 idx = 0;
        #1 data[0] = 1;
        #1 data[4] = 1;
        #1 data[5] = 1;
        #1 data[3] = 1;
        #1 data[8] = 1;
        #1 idx = 0;
        #1 idx = 1;
        #1 data[0] = 0;
        #1 data[0] = 1;
    end
endmodule
