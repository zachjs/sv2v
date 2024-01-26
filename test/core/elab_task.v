module top;
	initial begin
		$write("Elaboration Info: ");
		$display;
	end
	initial begin
		$write("Elaboration Info: ");
		$display("%b", 1);
	end
	initial begin
		$write("Elaboration Warning: ");
		$display;
	end
	initial begin
		$write("Elaboration Warning: ");
		$display("%b", 2);
	end
	initial begin
		$write("Elaboration Error: ");
		$display;
	end
	initial begin
		$write("Elaboration Error: ");
		$display("%b", 3);
	end
	initial begin
		$display("Elaboration Fatal:");
		$finish;
	end
	initial begin
		$write("Elaboration Fatal: ");
		$display;
		$finish(0);
	end
	initial begin
		$write("Elaboration Fatal: ");
		$display("%b", 4);
		$finish(0);
	end
endmodule
