module check;
    wire y;
    if (1) begin : over
        wire [3:0] x;
        assign x[0] = 0;
        initial $display("intf x %b", x);
    end
    if (1) begin : m
        assign over.x[1] = 1;
        initial $display("mod i.x %b", over.x);
    end
    assign over.x[2] = 1'bz;
    initial $display("check over.x %b", over.x);
    initial $display("check y %b", y);
endmodule

module top;
    check c();
    if (1) begin : over
        wire [3:0] x;
        assign x[0] = 0;
        initial $display("intf x %b", x);
    end
    if (1) begin : m
        assign over.x[1] = 1;
        initial $display("mod i.x %b", over.x);
    end
    assign over.x[2] = 1'bz;
    initial $display("top over.x %b", over.x);
endmodule
