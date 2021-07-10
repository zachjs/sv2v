`define TEST(size, literal) \
    tmp``size = literal; \
    $display(`"size'(literal) = %b`", tmp``size);
module top;
    initial begin : blk
        reg [0:0] tmp1; reg [1:0] tmp2; reg [2:0] tmp3; reg [3:0] tmp4; reg [4:0] tmp5;
        reg [5:0] tmp6; reg [6:0] tmp7; reg [7:0] tmp8; reg [8:0] tmp9; reg [9:0] tmp10;
        reg [10:0] tmp11; reg [11:0] tmp12; reg [12:0] tmp13; reg [13:0] tmp14; reg [14:0] tmp15;
        reg [15:0] tmp16; reg [16:0] tmp17; reg [17:0] tmp18; reg [18:0] tmp19; reg [19:0] tmp20;
        reg [20:0] tmp21; reg [21:0] tmp22; reg [22:0] tmp23; reg [23:0] tmp24; reg [24:0] tmp25;
        reg [25:0] tmp26; reg [26:0] tmp27; reg [27:0] tmp28; reg [28:0] tmp29; reg [29:0] tmp30;
        reg [30:0] tmp31; reg [31:0] tmp32; reg [32:0] tmp33; reg [33:0] tmp34; reg [34:0] tmp35;
        `include "cast_literal.vh"
    end
endmodule
