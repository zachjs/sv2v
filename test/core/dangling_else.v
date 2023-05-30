module top;
	wire [15:0] a;
	wire [15:0] b;
	reg [15:0] c;
	integer i;
	always @(*)
		if (a[0]) begin
			if (b[0])
				c[0] = 1;
			else
				c[0] = 0;
		end
	always @(*)
		if (a[1]) begin
			if (b[1])
				c[1] = 1;
		end
		else
			c[1] = 0;
	always @(*)
		if (a[2]) begin
			(* test *)
			if (b[2])
				c[2] = 1;
			else
				c[2] = 0;
		end
	always @(*)
		if (a[3]) begin
			(* test *)
			if (b[3])
				c[3] = 1;
		end
		else
			c[3] = 0;
	always @(*)
		if (a[4]) begin
			for (i = 0; i < 1; i = i + 1)
				if (b[4])
					c[4] = 1;
				else
					c[4] = 0;
		end
	always @(*)
		if (a[5]) begin
			for (i = 0; i < 1; i = i + 1)
				if (b[5])
					c[5] = 1;
		end
		else
			c[5] = 0;
	always @(*)
		if (a[6]) begin
			while (i < 1) if (b[6])
				c[6] = 1;
			else
				c[6] = 0;
		end
	always @(*)
		if (a[7]) begin
			while (i < 1) if (b[7])
				c[7] = 1;
		end
		else
			c[7] = 0;
	always @(*)
		if (a[8]) begin
			repeat (i) if (b[8])
				c[8] = 1;
			else
				c[8] = 0;
		end
	always @(*)
		if (a[9]) begin
			repeat (i) if (b[9])
				c[9] = 1;
		end
		else
			c[9] = 0;
	always @(*)
		if (a[10]) begin : sv2v_autoblock_1
			integer j;
			for (j = 15; j >= 0; j = j - 1)
				if (b[10])
					c[10] = 1;
				else
					c[10] = 0;
		end
	always @(*)
		if (a[11]) begin : sv2v_autoblock_2
			integer j;
			for (j = 15; j >= 0; j = j - 1)
				if (b[11])
					c[11] = 1;
		end
		else
			c[11] = 0;
	always @(*)
		if (a[12]) begin
			forever #(10)
				if (b[12])
					c[12] = 1;
				else
					c[12] = 0;
		end
	always @(*)
		if (a[13]) begin
			forever #(10)
				if (b[13])
					c[13] = 1;
		end
		else
			c[13] = 0;
	always @(*)
		if (a[14]) begin
			#(10)
				if (b[14])
					c[14] = 1;
				else
					c[14] = 0;
		end
	always @(*)
		if (a[15]) begin
			#(10)
				if (b[15])
					c[15] = 1;
		end
		else
			c[15] = 0;
endmodule