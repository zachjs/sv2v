module test;
    typedef struct packed {
        int w, x;
        byte y;
        logic z;
    } struct_a;
    struct_a a;
    wire signed [31:0] a_w, a_x;
    wire signed [7:0] a_y;
    assign a_w = a.w;
    assign a_x = a.x;
    assign a_y = a.y;
    initial begin
        // TODO: The signed fields should not have to be indirected here, but
        // iverilog does not currently support complex arguments to $monitor.
        $monitor("%2d: %b %b %b %b %b", $time, a, a_w, a_x, a_y, a.z);

        #1 a.w = 0;
        #1 a.x = 0;
        #1 a.y = 0;
        #1 a.z = 0;

        #1 a = '{default: 1};
        #1 a = '{default: 2};
        #1 a = '{default: 3};
        #1 a = '{default: 0};
        #1 a = '{default: -1};
        #1 a = '{default: -2};

        #1 a = '{int: 0, default: 1};
        #1 a = '{byte: 0, default: 1};
        #1 a = '{logic: 0, default: 1};
        #1 a = '{logic: 1, int: 2, byte: 3};
        #1 a = '{logic: 1, int: 2, byte: 3, default: -1};
        #1 a = '{int: 3, byte: 2, default: 0};

        #1 a = '{w: 8, int: 0, default: 1};
        #1 a = '{w: 8, byte: 0, default: 1};
        #1 a = '{w: 8, logic: 0, default: 1};
        #1 a = '{w: 8, logic: 1, int: 2, byte: 3};
        #1 a = '{w: 8, logic: 1, int: 2, byte: 3, default: -1};
        #1 a = '{w: 8, int: 3, byte: 2, default: 0};

    end
    typedef struct packed {
        int x;
        struct_a y;
        logic z;
    } struct_b;
    struct_b b;
    wire signed [31:0] b_x;
    assign b_x = b.x;
    initial begin
        #100;
        // TODO: The signed fields should not have to be indirected here, but
        // iverilog does not currently support complex arguments to $monitor.
        $monitor("%2d: %b %b %b %b", $time, b, b_x, b.y, b.z);

        #1 b.x = 0;
        #1 b.y = 0;
        #1 b.z = 0;

        #1 b = '{default: 1};
        #1 b = '{default: 2};
        #1 b = '{default: 3};
        #1 b = '{default: 0};
        #1 b = '{default: -1};
        #1 b = '{default: -2};

        #1 b = '{int: 0, default: 1};
        #1 b = '{byte: 0, default: 1};
        #1 b = '{logic: 0, default: 1};
        #1 b = '{logic: 1, int: 2, byte: 3};
        #1 b = '{logic: 1, int: 2, byte: 3, default: -1};
        #1 b = '{int: 3, byte: 2, default: 0};

    end
endmodule

module top; endmodule
