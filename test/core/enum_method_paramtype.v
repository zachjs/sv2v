module inner_alias_B8D29;
	wire [1:0] v;
	integer next_val;
	integer prev_val;
	function automatic [1:0] sv2v_cast_2;
		input reg [1:0] inp;
		sv2v_cast_2 = inp;
	endfunction
	initial begin
		next_val = sv2v_cast_2(v[1-:2] + 1);
		prev_val = sv2v_cast_2(v[1-:2] - 1);
		$display("alias next=%0d prev=%0d", next_val, prev_val);
	end
endmodule
module inner_inline_D2FEB;
	wire [1:0] v;
	integer next_val;
	integer prev_val;
	function automatic [1:0] sv2v_cast_2;
		input reg [1:0] inp;
		sv2v_cast_2 = inp;
	endfunction
	initial begin
		next_val = sv2v_cast_2(v[1-:2] + 1);
		prev_val = sv2v_cast_2(v[1-:2] - 1);
		$display("inline next=%0d prev=%0d", next_val, prev_val);
	end
endmodule
module inner_bare_59EC8;
	wire [1:0] x;
	integer next_val;
	function automatic [1:0] sv2v_cast_2;
		input reg [1:0] inp;
		sv2v_cast_2 = inp;
	endfunction
	initial begin
		next_val = sv2v_cast_2(x + 1);
		$display("bare next=%0d", next_val);
	end
endmodule
module top;
	inner_alias_B8D29 i();
	inner_inline_D2FEB j();
	inner_bare_59EC8 k();
endmodule
