module top;
	wire [1:0] select;
	wire [11:0] data;
	UniqueCase case0(
		.select(select),
		.data(data[0+:4])
	);
	Unique0Case case1(
		.select(select),
		.data(data[4+:4])
	);
	PriorityCase case2(
		.select(select),
		.data(data[8+:4])
	);
endmodule
module UniqueCase (
	select,
	data
);
	reg _sv2v_0;
	input wire [1:0] select;
	output reg [3:0] data;
	always @(*) begin
		if (_sv2v_0)
			;
		data = 4'b0000;
		(* synthesis, parallel_case *)
		case (select)
			2'd0: data = 4'ha;
			2'd1: data = 4'h6;
			2'd2: data = 4'h3;
		endcase
	end
	initial _sv2v_0 = 0;
endmodule
module Unique0Case (
	select,
	data
);
	reg _sv2v_0;
	input wire [1:0] select;
	output reg [3:0] data;
	always @(*) begin
		if (_sv2v_0)
			;
		data = 4'b0000;
		(* synthesis, parallel_case *)
		case (select)
			2'd0: data = 4'ha;
			2'd1: data = 4'h6;
			2'd2: data = 4'h3;
		endcase
	end
	initial _sv2v_0 = 0;
endmodule
module PriorityCase (
	select,
	data
);
	reg _sv2v_0;
	input wire [1:0] select;
	output reg [3:0] data;
	always @(*) begin
		if (_sv2v_0)
			;
		data = 4'b0000;
		(* synthesis, full_case *)
		case (select)
			2'd0: data = 4'ha;
			2'd1: data = 4'h6;
			2'd2: data = 4'h3;
		endcase
	end
	initial _sv2v_0 = 0;
endmodule
