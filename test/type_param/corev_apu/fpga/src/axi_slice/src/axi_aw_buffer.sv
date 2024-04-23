// Copyright 2018 ETH Zurich and University of Bologna.
// Copyright and related rights are licensed under the Solderpad Hardware
// License, Version 0.51 (the "License"); you may not use this file except in
// compliance with the License.  You may obtain a copy of the License at
// http://solderpad.org/licenses/SHL-0.51. Unless required by applicable law
// or agreed to in writing, software, hardware and materials distributed under
// this License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
// CONDITIONS OF ANY KIND, either express or implied. See the License for the
// specific language governing permissions and limitations under the License.

// Davide Rossi <davide.rossi@unibo.it>

module axi_aw_buffer #(
    parameter int ID_WIDTH     = -1,
    parameter int ADDR_WIDTH   = -1,
    parameter int USER_WIDTH   = -1,
    parameter int BUFFER_DEPTH = -1
)(

    input logic                   clk_i,
    input logic                   rst_ni,
    input logic                   test_en_i,

    input  logic                  slave_valid_i,
    input  logic [ADDR_WIDTH-1:0] slave_addr_i,
    input  logic [2:0]            slave_prot_i,
    input  logic [3:0]            slave_region_i,
    input  logic [7:0]            slave_len_i,
    input  logic [2:0]            slave_size_i,
    input  logic [1:0]            slave_burst_i,
    input  logic                  slave_lock_i,
    input  logic [3:0]            slave_cache_i,
    input  logic [3:0]            slave_qos_i,
    input  logic [ID_WIDTH-1:0]   slave_id_i,
    input  logic [USER_WIDTH-1:0] slave_user_i,
    output logic                  slave_ready_o,

    output logic                  master_valid_o,
    output logic [ADDR_WIDTH-1:0] master_addr_o,
    output logic [2:0]            master_prot_o,
    output logic [3:0]            master_region_o,
    output logic [7:0]            master_len_o,
    output logic [2:0]            master_size_o,
    output logic [1:0]            master_burst_o,
    output logic                  master_lock_o,
    output logic [3:0]            master_cache_o,
    output logic [3:0]            master_qos_o,
    output logic [ID_WIDTH-1:0]   master_id_o,
    output logic [USER_WIDTH-1:0] master_user_o,
    input  logic                  master_ready_i
);

   logic [29+ADDR_WIDTH+USER_WIDTH+ID_WIDTH-1:0] s_data_in;
   logic [29+ADDR_WIDTH+USER_WIDTH+ID_WIDTH-1:0] s_data_out;



   assign s_data_in = {slave_cache_i,  slave_prot_i,  slave_lock_i,  slave_burst_i,  slave_size_i,  slave_len_i,  slave_qos_i,  slave_region_i,  slave_addr_i,  slave_user_i,  slave_id_i};
   assign             {master_cache_o, master_prot_o, master_lock_o, master_burst_o, master_size_o, master_len_o, master_qos_o, master_region_o, master_addr_o, master_user_o, master_id_o} = s_data_out;


    axi_single_slice #(.BUFFER_DEPTH(BUFFER_DEPTH), .DATA_WIDTH(29+ADDR_WIDTH+USER_WIDTH+ID_WIDTH)) i_axi_single_slice (
      .clk_i      ( clk_i          ),
      .rst_ni     ( rst_ni         ),
      .testmode_i ( test_en_i      ),
      .valid_i    ( slave_valid_i  ),
      .ready_o    ( slave_ready_o  ),
      .data_i     ( s_data_in      ),
      .ready_i    ( master_ready_i ),
      .valid_o    ( master_valid_o ),
      .data_o     ( s_data_out     )
    );

endmodule
