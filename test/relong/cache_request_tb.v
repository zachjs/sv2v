`default_nettype none

module top;

    wire [5:0] index;
    wire [3:0] offset;
    wire [103:0] flatRequest;
    reg [10:0] dataIn;

    CacheHelper dut(
        .index(index),
        .offset(offset),
        .flatRequest(flatRequest)
    );

    assign {index, offset} = dataIn[9:0];

    initial begin
        $monitor($time, " %h %h = %h", index, offset, flatRequest);
        for (dataIn = 11'h0; dataIn <= 10'h3ff; dataIn = dataIn + 11'h1) begin
            #10;
            if (94'h277bad0badf00d0000000080 != flatRequest[93:0])
                $error("Bad fixed values");
            if (dataIn[9:0] != flatRequest[103:94])
                $error("Bad index/offset");
        end
    end
endmodule

