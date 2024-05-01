/* Copyright 2018 ETH Zurich and University of Bologna.
 * Copyright and related rights are licensed under the Solderpad Hardware
 * License, Version 0.51 (the “License”); you may not use this file except in
 * compliance with the License.  You may obtain a copy of the License at
 * http://solderpad.org/licenses/SHL-0.51. Unless required by applicable law
 * or agreed to in writing, software, hardware and materials distributed under
 * this License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * File:  axi_shim.sv
 * Author: Michael Schaffner <schaffner@iis.ee.ethz.ch>
 *         Florian Zaruba <zarubaf@iis.ee.ethz.ch>
 * Date:   1.8.2018
 *
 * Description: Manages communication with the AXI Bus. Note that this unit does not
 *              buffer requests and register the signals.
 *
 */


module axi_shim #(
    parameter config_pkg::cva6_cfg_t CVA6Cfg = config_pkg::cva6_cfg_empty,
    parameter int unsigned AxiNumWords = 4, // data width in dwords, this is also the maximum burst length, must be >=2
    parameter type axi_req_t = logic,
    parameter type axi_rsp_t = logic
) (
    input logic clk_i,  // Clock
    input logic rst_ni,  // Asynchronous reset active low
    // read channel
    // request
    input logic rd_req_i,
    output logic rd_gnt_o,
    input logic [CVA6Cfg.AxiAddrWidth-1:0] rd_addr_i,
    input logic [$clog2(AxiNumWords)-1:0] rd_blen_i,  // axi convention: LEN-1
    input logic [2:0] rd_size_i,
    input  logic [CVA6Cfg.AxiIdWidth-1:0]   rd_id_i,   // use same ID for reads, or make sure you only have one outstanding read tx
    input logic rd_lock_i,
    // read response (we have to unconditionally sink the response)
    input logic rd_rdy_i,
    output logic rd_last_o,
    output logic rd_valid_o,
    output logic [CVA6Cfg.AxiDataWidth-1:0] rd_data_o,
    output logic [CVA6Cfg.AxiUserWidth-1:0] rd_user_o,
    output logic [CVA6Cfg.AxiIdWidth-1:0] rd_id_o,
    output logic rd_exokay_o,  // indicates whether exclusive tx succeeded
    // write channel
    input logic wr_req_i,
    output logic wr_gnt_o,
    input logic [CVA6Cfg.AxiAddrWidth-1:0] wr_addr_i,
    input logic [AxiNumWords-1:0][CVA6Cfg.AxiDataWidth-1:0] wr_data_i,
    input logic [AxiNumWords-1:0][CVA6Cfg.AxiUserWidth-1:0] wr_user_i,
    input logic [AxiNumWords-1:0][(CVA6Cfg.AxiDataWidth/8)-1:0] wr_be_i,
    input logic [$clog2(AxiNumWords)-1:0] wr_blen_i,  // axi convention: LEN-1
    input logic [2:0] wr_size_i,
    input logic [CVA6Cfg.AxiIdWidth-1:0] wr_id_i,
    input logic wr_lock_i,
    input logic [5:0] wr_atop_i,
    // write response
    input logic wr_rdy_i,
    output logic wr_valid_o,
    output logic [CVA6Cfg.AxiIdWidth-1:0] wr_id_o,
    output logic wr_exokay_o,  // indicates whether exclusive tx succeeded
    // AXI port
    output axi_req_t axi_req_o,
    input axi_rsp_t axi_resp_i
);
  localparam AddrIndex = ($clog2(AxiNumWords) > 0) ? $clog2(AxiNumWords) : 1;

  ///////////////////////////////////////////////////////
  // write channel
  ///////////////////////////////////////////////////////

  enum logic [3:0] {
    IDLE,
    WAIT_AW_READY,
    WAIT_LAST_W_READY,
    WAIT_LAST_W_READY_AW_READY,
    WAIT_AW_READY_BURST
  }
      wr_state_q, wr_state_d;

  // AXI tx counter
  logic [AddrIndex-1:0] wr_cnt_d, wr_cnt_q;
  logic wr_single_req, wr_cnt_done, wr_cnt_clr, wr_cnt_en;

  assign wr_single_req = (wr_blen_i == 0);

  // address
  assign axi_req_o.aw.burst = axi_pkg::BURST_INCR;  // Use BURST_INCR for AXI regular transaction
  assign axi_req_o.aw.addr = wr_addr_i[CVA6Cfg.AxiAddrWidth-1:0];
  assign axi_req_o.aw.size = wr_size_i;
  assign axi_req_o.aw.len = wr_blen_i;
  assign axi_req_o.aw.id = wr_id_i;
  assign axi_req_o.aw.prot = 3'b0;
  assign axi_req_o.aw.region = 4'b0;
  assign axi_req_o.aw.lock = wr_lock_i;
  assign axi_req_o.aw.cache = axi_pkg::CACHE_MODIFIABLE;
  assign axi_req_o.aw.qos = 4'b0;
  assign axi_req_o.aw.atop = wr_atop_i;
  assign axi_req_o.aw.user = '0;

  // data
  assign axi_req_o.w.data = wr_data_i[wr_cnt_q];
  assign axi_req_o.w.user = wr_user_i[wr_cnt_q];
  assign axi_req_o.w.strb = wr_be_i[wr_cnt_q];
  assign axi_req_o.w.last = wr_cnt_done;

  // write response
  assign wr_exokay_o = (axi_resp_i.b.resp == axi_pkg::RESP_EXOKAY);
  assign axi_req_o.b_ready = wr_rdy_i;
  assign wr_valid_o = axi_resp_i.b_valid;
  assign wr_id_o = axi_resp_i.b.id;

  // tx counter
  assign wr_cnt_done = (wr_cnt_q == wr_blen_i);
  assign wr_cnt_d            = (wr_cnt_clr) ? '0 : (wr_cnt_en && CVA6Cfg.AxiBurstWriteEn) ? wr_cnt_q + 1 : wr_cnt_q;

  always_comb begin : p_axi_write_fsm
    // default
    wr_state_d         = wr_state_q;

    axi_req_o.aw_valid = 1'b0;
    axi_req_o.w_valid  = 1'b0;
    wr_gnt_o           = 1'b0;

    wr_cnt_en          = 1'b0;
    wr_cnt_clr         = 1'b0;

    case (wr_state_q)
      ///////////////////////////////////
      IDLE: begin
        // we have an incoming request
        if (wr_req_i) begin
          // is this a read or write?
          axi_req_o.aw_valid = 1'b1;
          axi_req_o.w_valid  = 1'b1;

          if (CVA6Cfg.AxiBurstWriteEn && !wr_single_req) begin
            wr_cnt_en = axi_resp_i.w_ready;

            case ({
              axi_resp_i.aw_ready, axi_resp_i.w_ready
            })
              2'b11:   wr_state_d = WAIT_LAST_W_READY;
              2'b01:   wr_state_d = WAIT_LAST_W_READY_AW_READY;
              2'b10:   wr_state_d = WAIT_LAST_W_READY;
              default: ;
            endcase
          end else if (wr_single_req) begin  // its a single write
            wr_cnt_clr = 1'b1;
            // single req can be granted here
            wr_gnt_o   = axi_resp_i.aw_ready & axi_resp_i.w_ready;
            case ({
              axi_resp_i.aw_ready, axi_resp_i.w_ready
            })
              2'b01:   wr_state_d = WAIT_AW_READY;
              2'b10:   wr_state_d = WAIT_LAST_W_READY;
              default: wr_state_d = IDLE;
            endcase
            // its a request for the whole cache line
          end
        end
      end
      ///////////////////////////////////
      // ~> from single write
      WAIT_AW_READY: begin
        axi_req_o.aw_valid = 1'b1;

        if (axi_resp_i.aw_ready) begin
          wr_state_d = IDLE;
          wr_gnt_o   = 1'b1;
        end
      end
      ///////////////////////////////////
      // ~> from write, there is an outstanding write
      WAIT_LAST_W_READY: begin
        axi_req_o.w_valid = 1'b1;

        if (CVA6Cfg.AxiBurstWriteEn && axi_resp_i.w_ready && !wr_cnt_done) begin
          wr_cnt_en = 1'b1;
        end else if (wr_cnt_done) begin  // this is the last write
          if (axi_resp_i.w_ready) begin
            wr_state_d = IDLE;
            wr_cnt_clr = 1'b1;
            wr_gnt_o   = 1'b1;
          end
        end
      end
      ///////////////////////////////////
      default: begin
        ///////////////////////////////////
        // ~> we need to wait for an aw_ready and there is at least one outstanding write
        if (CVA6Cfg.AxiBurstWriteEn) begin
          if (wr_state_q == WAIT_LAST_W_READY_AW_READY) begin
            axi_req_o.w_valid  = 1'b1;
            axi_req_o.aw_valid = 1'b1;
            // we got an aw_ready
            case ({
              axi_resp_i.aw_ready, axi_resp_i.w_ready
            })
              // we got an aw ready
              2'b01: begin
                // are there any outstanding transactions?
                if (wr_cnt_done) begin
                  wr_state_d = WAIT_AW_READY_BURST;
                  wr_cnt_clr = 1'b1;
                end else begin
                  // yes, so reduce the count and stay here
                  wr_cnt_en = 1'b1;
                end
              end
              2'b10:   wr_state_d = WAIT_LAST_W_READY;
              2'b11: begin
                // we are finished
                if (wr_cnt_done) begin
                  wr_state_d = IDLE;
                  wr_gnt_o   = 1'b1;
                  wr_cnt_clr = 1'b1;
                  // there are outstanding transactions
                end else begin
                  wr_state_d = WAIT_LAST_W_READY;
                  wr_cnt_en  = 1'b1;
                end
              end
              default: ;
            endcase
          end  ///////////////////////////////////
               // ~> all data has already been sent, we are only waiting for the aw_ready
          else if (wr_state_q == WAIT_AW_READY_BURST) begin
            axi_req_o.aw_valid = 1'b1;

            if (axi_resp_i.aw_ready) begin
              wr_state_d = IDLE;
              wr_gnt_o   = 1'b1;
            end
          end
        end else begin
          wr_state_d = IDLE;
        end
      end
    endcase
  end


  ///////////////////////////////////////////////////////
  // read channel
  ///////////////////////////////////////////////////////

  // address
  // in case of a wrapping transfer we can simply begin at the address, if we want to request a cache-line
  // with an incremental transfer we need to output the corresponding base address of the cache line
  assign axi_req_o.ar.burst  = axi_pkg::BURST_INCR;  // Use BURST_INCR for AXI regular transaction
  assign axi_req_o.ar.addr   = rd_addr_i[CVA6Cfg.AxiAddrWidth-1:0];
  assign axi_req_o.ar.size   = rd_size_i;
  assign axi_req_o.ar.len    = rd_blen_i;
  assign axi_req_o.ar.id     = rd_id_i;
  assign axi_req_o.ar.prot   = 3'b0;
  assign axi_req_o.ar.region = 4'b0;
  assign axi_req_o.ar.lock   = rd_lock_i;
  assign axi_req_o.ar.cache  = axi_pkg::CACHE_MODIFIABLE;
  assign axi_req_o.ar.qos    = 4'b0;
  assign axi_req_o.ar.user   = '0;

  // make the read request
  assign axi_req_o.ar_valid  = rd_req_i;
  assign rd_gnt_o            = rd_req_i & axi_resp_i.ar_ready;

  // return path
  assign axi_req_o.r_ready   = rd_rdy_i;
  assign rd_data_o           = axi_resp_i.r.data;
  if (ariane_pkg::AXI_USER_EN) begin
    assign rd_user_o = axi_resp_i.r.user;
  end else begin
    assign rd_user_o = '0;
  end
  assign rd_last_o   = axi_resp_i.r.last;
  assign rd_valid_o  = axi_resp_i.r_valid;
  assign rd_id_o     = axi_resp_i.r.id;
  assign rd_exokay_o = (axi_resp_i.r.resp == axi_pkg::RESP_EXOKAY);


  // ----------------
  // Registers
  // ----------------
  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (~rst_ni) begin
      // start in flushing state and initialize the memory
      wr_state_q <= IDLE;
      wr_cnt_q   <= '0;
    end else begin
      wr_state_q <= wr_state_d;
      wr_cnt_q   <= wr_cnt_d;
    end
  end

  // ----------------
  // Assertions
  // ----------------

  //pragma translate_off
  initial begin
    assert (AxiNumWords >= 1)
    else $fatal(1, "[axi adapter] AxiNumWords must be >= 1");
    assert (CVA6Cfg.AxiIdWidth >= 2)
    else $fatal(1, "[axi adapter] AXI id width must be at least 2 bit wide");
  end
  //pragma translate_on

endmodule  // axi_adapter2
