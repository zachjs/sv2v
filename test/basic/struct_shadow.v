// While this might look silly, you'll notice that the sections are actually
// different. We are ensuring that the correct struct definitions are being used
// in each scope.
module top;
    reg [2:0] a = 3'b111;
    reg [2:0] b = 3'b111;
    reg [2:0] c = 3'b111;
    reg [2:0] d = 3'b111;
    reg [2:0] e = 3'b111;
    reg [2:0] f = 3'b111;
    integer i = 2;
    integer j = 2;
    integer k = 2;
    initial begin
        $display("A: 000 -> 000000");
        $display("A: 001 -> 121424");
        $display("A: 010 -> 214142");
        $display("A: 011 -> 335566");
        $display("A: 100 -> 442211");
        $display("A: 101 -> 563635");
        $display("A: 110 -> 656353");
        $display("A: 111 -> 777777");
        $display("B: 000 -> 000000");
        $display("B: 001 -> 214241");
        $display("B: 010 -> 141422");
        $display("B: 011 -> 355663");
        $display("B: 100 -> 422114");
        $display("B: 101 -> 636355");
        $display("B: 110 -> 563536");
        $display("B: 111 -> 777777");
        $display("C: 000 -> 000000");
        $display("C: 001 -> 142412");
        $display("C: 010 -> 414221");
        $display("C: 011 -> 556633");
        $display("C: 100 -> 221144");
        $display("C: 101 -> 363556");
        $display("C: 110 -> 635365");
        $display("C: 111 -> 777777");
        $display("D: 000 -> 000000");
        $display("D: 001 -> 424121");
        $display("D: 010 -> 142214");
        $display("D: 011 -> 566335");
        $display("D: 100 -> 211442");
        $display("D: 101 -> 635563");
        $display("D: 110 -> 353656");
        $display("D: 111 -> 777777");
        $display("E: 000 -> 000000");
        $display("E: 001 -> 241214");
        $display("E: 010 -> 422141");
        $display("E: 011 -> 663355");
        $display("E: 100 -> 114422");
        $display("E: 101 -> 355636");
        $display("E: 110 -> 536563");
        $display("E: 111 -> 777777");
        $display("F: 000 -> 000000");
        $display("F: 001 -> 412142");
        $display("F: 010 -> 221414");
        $display("F: 011 -> 633556");
        $display("F: 100 -> 144221");
        $display("F: 101 -> 556363");
        $display("F: 110 -> 365635");
        $display("F: 111 -> 777777");
    end
endmodule
