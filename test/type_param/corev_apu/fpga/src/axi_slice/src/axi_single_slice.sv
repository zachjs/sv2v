// Copyright 2018 ETH Zurich and University of Bologna.
// Copyright and related rights are licensed under the Solderpad Hardware
// License, Version 0.51 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://solderpad.org/licenses/SHL-0.51. Unless required by applicable law
// or agreed to in writing, software, hardware and materials distributed under
// this License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.

/// Wrapper for a generic fifo
module axi_single_slice #(
    parameter int BUFFER_DEPTH = -1,
    parameter int DATA_WIDTH   = -1
) (
    input  logic                  clk_i,    // Clock
    input  logic                  rst_ni,  // Asynchronous reset active low
    input  logic                  testmode_i,
    input  logic                  valid_i,
    output logic                  ready_o,
    input  logic [DATA_WIDTH-1:0] data_i,

    input  logic                  ready_i,
    output logic                  valid_o,
    output logic [DATA_WIDTH-1:0] data_o
);

    logic full, empty;

    assign ready_o = ~full;
    assign valid_o = ~empty;

    fifo #(
        .FALL_THROUGH ( 1'b0         ),
        .DATA_WIDTH   ( DATA_WIDTH   ),
        .DEPTH        ( BUFFER_DEPTH )
    ) i_fifo (
        .clk_i      ( clk_i             ),
        .rst_ni     ( rst_ni            ),
        .flush_i    ( 1'b0              ),
        .threshold_o (), // NC
        .testmode_i ( testmode_i        ),
        .full_o     ( full              ),
        .empty_o    ( empty             ),
        .data_i     ( data_i            ),
        .push_i     ( valid_i & ready_o ),
        .data_o     ( data_o            ),
        .pop_i      ( ready_i & valid_o )
    );

endmodule
