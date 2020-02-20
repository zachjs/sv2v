module top;
    reg [31:0] data;
    reg p1, p2;
    wire [3:0] out_x;
    wire [3:0] out_y;

    Example example(data, p1, p2, out_x, out_y);

    task exhaust;
        begin
            #1 p1 = 0;
            #1 p2 = 0;
            #1 p1 = 0;
            #1 p2 = 1;
            #1 p1 = 1;
            #1 p2 = 0;
            #1 p1 = 1;
            #1 p2 = 1;
        end
    endtask

    initial begin
        $monitor("%2d %b %b %b %b %b", $time,
            data, p1, p2, out_x, out_y);
        #1 data = 32'ha7107338;
        exhaust;
        #1 data = 32'h8f8259e4;
        exhaust;
        #1 data = 32'h80ad046a;
        exhaust;
        #1 data = 32'hbf93017e;
        exhaust;
        #1 data = 32'he6458a2d;
        exhaust;
    end
endmodule
