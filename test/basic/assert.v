module Module(input clock, input clear, input data);
    wire x, y;
    assign y = data;
    assign x = y;
    task hello;
        $display("Hello!");
    endtask
endmodule
