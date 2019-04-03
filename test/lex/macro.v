module top;
initial begin
    repeat (13) $display(42);
    repeat (1) $display("foo\nbar");
    repeat (1) $display(" foo \n bar ");
    repeat (5) $display("test");
    repeat (35) $display("bar", "bar");
    repeat (35) $display("foo", "foo");
    $display("foo", 1);
    $display("foo", 1);
    $display("foo", 2);
    $display("display");
    $display("s", "foo", "ss");
    $display("s", "display", "ss");
    $display("s foo = \"foo\"");
    $display("s display = \"display\"");
end
endmodule
