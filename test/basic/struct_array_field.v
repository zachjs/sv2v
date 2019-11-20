module main;
    reg [2:0] foo_x_1;
    reg [2:0] foo_x_0;
    reg [1:0] foo_y_2;
    reg [1:0] foo_y_1;
    reg [1:0] foo_y_0;
    wire [5:0] foo_x;
    wire [5:0] foo_y;
    assign foo_x = {foo_x_1, foo_x_0};
    assign foo_y = {foo_y_0, foo_y_1, foo_y_2};
    reg foo_z;
    wire [12:0] foo;
    assign foo = {foo_x, foo_y, foo_z};

    initial begin
        $monitor($time, " %b %b %b %b %b %b %b %b",
            foo, foo_x, foo_y, foo_z,
            foo_x_0, foo_x_0[0], foo_y_0, foo_y_0[0]);

        #1; foo_z = 0;

        #1; {foo_y_0, foo_y_1, foo_y_2} = 0;
        #1; foo_y_0 = 1'sb1;
        #1; foo_y_1 = 1'sb1;
        #1; foo_y_1[1] = 0;
        #1; foo_y_0[0] = 1;
        #1; foo_y_0[1] = 1;

        #1; {foo_x_1, foo_x_0} = 0;
        #1; foo_x_0 = 1'sb1;
        #1; foo_x_1 = 1'sb1;
        #1; foo_x_1[1] = 0;
        #1; foo_x_0[0] = 1;
        #1; foo_x_0[1] = 1;

    end
endmodule

module top;
endmodule
