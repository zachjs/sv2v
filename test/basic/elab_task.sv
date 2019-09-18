module top;
    if (1 == 0)
        wire foo;
    else
        $info("foo");
endmodule
