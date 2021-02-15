`define TEST(x, y) $display("{%b, %b} => %b", x, y, {x, y});

module top;
    initial begin
        `TEST(1'sbz, 1'sbx);
        `TEST(1'sb1, 1'sb0);
        `TEST(2'sh3, 32'd0);
        `TEST(3'sh4, 32'd0);
        `TEST(3'sb101, 32'd0);
        `TEST(32'sh3, 32'd0);
        `TEST(32'sh4, 32'd0);
        `TEST(32'sb0101, 32'd0);
        `TEST(32'sh3, 32'd0);
        `TEST(32'sh4, 32'd0);
        `TEST(32'sb0101, 32'd0);
        `TEST(17'hz, 1'b0);
        `TEST(17'hzzzzz, 1'b0);
        `TEST(17'hzzzzz, 1'bz);
        `TEST(17'hzzzzz, 1'h0);
        `TEST(17'hzzzzz, 1'h1);
        `TEST(17'hzzzzz, 1'hx);
        `TEST(17'hzzzzz, 1'hz);
        `TEST(2'hx, 1'h0);
        `TEST(2'hx, 1'h1);
        `TEST(2'hx, 1'hx);
        `TEST(2'hx, 1'hz);
    end
endmodule
