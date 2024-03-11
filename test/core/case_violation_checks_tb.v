module top;
    reg [1:0] select;
    wire [3:0] data [2:0];
    UniqueCase case0(select, data[0]);
    Unique0Case case1(select, data[1]);
    PriorityCase case2(select, data[2]);
endmodule
