module example(sel, out);
    parameter W = 8;

    typedef struct packed {
        logic [W/2-1:0] x;
        logic [W/2-1:0] y;
    } line_t;

    line_t [3:0] arr;
    initial begin
        arr[0] = 8'b01000011;
        arr[1] = 8'b00010110;
        arr[2] = 8'b10001111;
        arr[3] = 8'b01100110;
    end

    input logic [1:0] sel;
    output line_t out;

    assign out.x = sel ? arr[sel].x : '0;
    always @* out.y = sel ? arr[sel].y : '0;
endmodule
