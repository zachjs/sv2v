module test;
    reg [31:0] a_w, a_x;
    reg [7:0] a_y;
    reg a_z;
    reg [72:0] a;
    always @* a = {a_w, a_x, a_y, a_z};
    initial begin
        $monitor("%2d: %b %b %b %b %b", $time, a, a_w, a_x, a_y, a_z);

        #1 a_w = 0;
        #1 a_x = 0;
        #1 a_y = 0;
        #1 a_z = 0;

        #1 begin
            a_w = 1;
            a_x = 1;
            a_y = 1;
            a_z = 1;
        end
        #1 begin
            a_w = 2;
            a_x = 2;
            a_y = 2;
            a_z = 2;
        end
        #1 begin
            a_w = 3;
            a_x = 3;
            a_y = 3;
            a_z = 3;
        end
        #1 begin
            a_w = 0;
            a_x = 0;
            a_y = 0;
            a_z = 0;
        end
        #1 begin
            a_w = -1;
            a_x = -1;
            a_y = -1;
            a_z = -1;
        end
        #1 begin
            a_w = -2;
            a_x = -2;
            a_y = -2;
            a_z = -2;
        end

        #1 begin
            a_w = 0;
            a_x = 0;
            a_y = 1;
            a_z = 1;
        end
        #1 begin
            a_w = 1;
            a_x = 1;
            a_y = 0;
            a_z = 1;
        end
        #1 begin
            a_w = 1;
            a_x = 1;
            a_y = 1;
            a_z = 0;
        end
        #1 begin
            a_w = 2;
            a_x = 2;
            a_y = 3;
            a_z = 1;
        end
        #1;
        #1 begin
            a_w = 3;
            a_x = 3;
            a_y = 2;
            a_z = 0;
        end

        #1 begin
            a_w = 8;
            a_x = 0;
            a_y = 1;
            a_z = 1;
        end
        #1 begin
            a_w = 8;
            a_x = 1;
            a_y = 0;
            a_z = 1;
        end
        #1 begin
            a_w = 8;
            a_x = 1;
            a_y = 1;
            a_z = 0;
        end
        #1 begin
            a_w = 8;
            a_x = 2;
            a_y = 3;
            a_z = 1;
        end
        #1;
        #1 begin
            a_w = 8;
            a_x = 3;
            a_y = 2;
            a_z = 0;
        end

    end
    reg [31:0] b_x;
    wire [72:0] b_y;
    reg b_z;
    reg [105:0] b;
    assign b_y = a;
    always @* b = {b_x, b_y, b_z};
    initial begin
        #100;
        a_w = 'bx;
        a_x = 'bx;
        a_y = 'bx;
        a_z = 'bx;
        $monitor("%2d: %b %b %b %b", $time, b, b_x, b_y, b_z);

        #1 b_x = 0;
        #1 begin
            a_w = 0;
            a_x = 0;
            a_y = 0;
            a_z = 0;
        end
        #1 b_z = 0;

        #1 begin
            a_w = 1;
            a_x = 1;
            a_y = 1;
            a_z = 1;
            b_x = 1;
            b_z = 1;
        end
        #1 begin
            a_w = 2;
            a_x = 2;
            a_y = 2;
            a_z = 2;
            b_x = 2;
            b_z = 2;
        end
        #1 begin
            a_w = 3;
            a_x = 3;
            a_y = 3;
            a_z = 3;
            b_x = 3;
            b_z = 3;
        end
        #1 begin
            a_w = 0;
            a_x = 0;
            a_y = 0;
            a_z = 0;
            b_x = 0;
            b_z = 0;
        end
        #1 begin
            a_w = -1;
            a_x = -1;
            a_y = -1;
            a_z = -1;
            b_x = -1;
            b_z = -1;
        end
        #1 begin
            a_w = -2;
            a_x = -2;
            a_y = -2;
            a_z = -2;
            b_x = -2;
            b_z = -2;
        end

        #1 begin
            a_w = 0;
            a_x = 0;
            a_y = 1;
            a_z = 1;
            b_x = 0;
            b_z = 1;
        end
        #1 begin
            a_w = 1;
            a_x = 1;
            a_y = 0;
            a_z = 1;
            b_x = 1;
            b_z = 1;
        end
        #1 begin
            a_w = 1;
            a_x = 1;
            a_y = 1;
            a_z = 0;
            b_x = 1;
            b_z = 0;
        end
        #1 begin
            a_w = 2;
            a_x = 2;
            a_y = 3;
            a_z = 1;
            b_x = 2;
            b_z = 1;
        end
        #1;
        #1 begin
            a_w = 3;
            a_x = 3;
            a_y = 2;
            a_z = 0;
            b_x = 3;
            b_z = 0;
        end

    end
endmodule

module top; endmodule
