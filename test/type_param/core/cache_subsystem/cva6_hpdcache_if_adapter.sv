// Copyright 2023 Commissariat a l'Energie Atomique et aux Energies
//                Alternatives (CEA)
//
// Licensed under the Solderpad Hardware License, Version 2.1 (the “License”);
// you may not use this file except in compliance with the License.
// SPDX-License-Identifier: Apache-2.0 WITH SHL-2.1
// You may obtain a copy of the License at https://solderpad.org/licenses/
//
// Authors: Cesar Fuguet
// Date: February, 2023
// Description: Interface adapter for the CVA6 core
module cva6_hpdcache_if_adapter
  import hpdcache_pkg::*;

//  Parameters
//  {{{
#(
    parameter config_pkg::cva6_cfg_t CVA6Cfg      = config_pkg::cva6_cfg_empty,
    parameter bit                    is_load_port = 1'b1
)
//  }}}

//  Ports
//  {{{
(
    //  Clock and active-low reset pins
    input logic clk_i,
    input logic rst_ni,

    //  Port ID
    input hpdcache_pkg::hpdcache_req_sid_t hpdcache_req_sid_i,

    //  Request/response ports from/to the CVA6 core
    input  ariane_pkg::dcache_req_i_t cva6_req_i,
    output ariane_pkg::dcache_req_o_t cva6_req_o,
    input  ariane_pkg::amo_req_t      cva6_amo_req_i,
    output ariane_pkg::amo_resp_t     cva6_amo_resp_o,

    //  Request port to the L1 Dcache
    output logic                        hpdcache_req_valid_o,
    input  logic                        hpdcache_req_ready_i,
    output hpdcache_pkg::hpdcache_req_t hpdcache_req_o,
    output logic                        hpdcache_req_abort_o,
    output hpdcache_pkg::hpdcache_tag_t hpdcache_req_tag_o,
    output hpdcache_pkg::hpdcache_pma_t hpdcache_req_pma_o,

    //  Response port from the L1 Dcache
    input logic                        hpdcache_rsp_valid_i,
    input hpdcache_pkg::hpdcache_rsp_t hpdcache_rsp_i
);
  //  }}}

  //  Internal nets and registers
  //  {{{
  logic forward_store, forward_amo;
  logic hpdcache_req_is_uncacheable;
  //  }}}

  //  Request forwarding
  //  {{{
  generate
    //  LOAD request
    //  {{{
    if (is_load_port == 1'b1) begin : load_port_gen
      assign hpdcache_req_is_uncacheable = !config_pkg::is_inside_cacheable_regions(
          CVA6Cfg,
          {
            {64 - ariane_pkg::DCACHE_TAG_WIDTH{1'b0}}
            , cva6_req_i.address_tag
            , {ariane_pkg::DCACHE_INDEX_WIDTH{1'b0}}
          }
      );

      //    Request forwarding
      assign hpdcache_req_valid_o = cva6_req_i.data_req,
          hpdcache_req_o.addr_offset = cva6_req_i.address_index,
          hpdcache_req_o.wdata = '0,
          hpdcache_req_o.op = hpdcache_pkg::HPDCACHE_REQ_LOAD,
          hpdcache_req_o.be = cva6_req_i.data_be,
          hpdcache_req_o.size = cva6_req_i.data_size,
          hpdcache_req_o.sid = hpdcache_req_sid_i,
          hpdcache_req_o.tid = cva6_req_i.data_id,
          hpdcache_req_o.need_rsp = 1'b1,
          hpdcache_req_o.phys_indexed = 1'b0,
          hpdcache_req_o.addr_tag = '0,  // unused on virtually indexed request
          hpdcache_req_o.pma = '0;  // unused on virtually indexed request

      assign hpdcache_req_abort_o = cva6_req_i.kill_req,
          hpdcache_req_tag_o = cva6_req_i.address_tag,
          hpdcache_req_pma_o.uncacheable = hpdcache_req_is_uncacheable,
          hpdcache_req_pma_o.io = 1'b0;

      //    Response forwarding
      assign cva6_req_o.data_rvalid = hpdcache_rsp_valid_i,
          cva6_req_o.data_rdata = hpdcache_rsp_i.rdata,
          cva6_req_o.data_rid = hpdcache_rsp_i.tid,
          cva6_req_o.data_gnt = hpdcache_req_ready_i;
    end  //  }}}

         //  {{{
    else begin : store_amo_gen
      //  STORE/AMO request
      hpdcache_req_addr_t   amo_addr;
      hpdcache_req_offset_t amo_addr_offset;
      hpdcache_tag_t        amo_tag;
      logic amo_is_word, amo_is_word_hi;
      hpdcache_req_data_t        amo_data;
      hpdcache_req_be_t          amo_data_be;
      hpdcache_req_op_t          amo_op;
      logic               [31:0] amo_resp_word;

      //  AMO logic
      //  {{{
      always_comb begin : amo_op_comb
        amo_addr = cva6_amo_req_i.operand_a;
        amo_addr_offset = amo_addr[0+:HPDCACHE_REQ_OFFSET_WIDTH];
        amo_tag = amo_addr[HPDCACHE_REQ_OFFSET_WIDTH+:HPDCACHE_TAG_WIDTH];
        amo_is_word = (cva6_amo_req_i.size == 2'b10);
        amo_is_word_hi = cva6_amo_req_i.operand_a[2];

        amo_data = amo_is_word ? {2{cva6_amo_req_i.operand_b[0+:32]}} : cva6_amo_req_i.operand_b;

        amo_data_be = amo_is_word_hi ? 8'hf0 : amo_is_word ? 8'h0f : 8'hff;

        unique case (cva6_amo_req_i.amo_op)
          ariane_pkg::AMO_LR:   amo_op = HPDCACHE_REQ_AMO_LR;
          ariane_pkg::AMO_SC:   amo_op = HPDCACHE_REQ_AMO_SC;
          ariane_pkg::AMO_SWAP: amo_op = HPDCACHE_REQ_AMO_SWAP;
          ariane_pkg::AMO_ADD:  amo_op = HPDCACHE_REQ_AMO_ADD;
          ariane_pkg::AMO_AND:  amo_op = HPDCACHE_REQ_AMO_AND;
          ariane_pkg::AMO_OR:   amo_op = HPDCACHE_REQ_AMO_OR;
          ariane_pkg::AMO_XOR:  amo_op = HPDCACHE_REQ_AMO_XOR;
          ariane_pkg::AMO_MAX:  amo_op = HPDCACHE_REQ_AMO_MAX;
          ariane_pkg::AMO_MAXU: amo_op = HPDCACHE_REQ_AMO_MAXU;
          ariane_pkg::AMO_MIN:  amo_op = HPDCACHE_REQ_AMO_MIN;
          ariane_pkg::AMO_MINU: amo_op = HPDCACHE_REQ_AMO_MINU;
          default:              amo_op = HPDCACHE_REQ_LOAD;
        endcase
      end

      assign amo_resp_word  = amo_is_word_hi ? hpdcache_rsp_i.rdata[0][32 +: 32]
                                                   : hpdcache_rsp_i.rdata[0][0  +: 32];
      //  }}}

      //  Request forwarding
      //  {{{
      assign hpdcache_req_is_uncacheable = !config_pkg::is_inside_cacheable_regions(
          CVA6Cfg,
          {
            {64 - ariane_pkg::DCACHE_TAG_WIDTH{1'b0}}
            , hpdcache_req_o.addr_tag,
            {ariane_pkg::DCACHE_INDEX_WIDTH{1'b0}}
          }
      );

      assign forward_store = cva6_req_i.data_req, forward_amo = cva6_amo_req_i.req;

      assign hpdcache_req_valid_o = forward_store | forward_amo,
          hpdcache_req_o.addr_offset = forward_amo ? amo_addr_offset : cva6_req_i.address_index,
          hpdcache_req_o.wdata = forward_amo ? amo_data : cva6_req_i.data_wdata,
          hpdcache_req_o.op = forward_amo ? amo_op : hpdcache_pkg::HPDCACHE_REQ_STORE,
          hpdcache_req_o.be = forward_amo ? amo_data_be : cva6_req_i.data_be,
          hpdcache_req_o.size = forward_amo ? cva6_amo_req_i.size : cva6_req_i.data_size,
          hpdcache_req_o.sid = hpdcache_req_sid_i,
          hpdcache_req_o.tid = forward_amo ? '1 : '0,
          hpdcache_req_o.need_rsp = forward_amo,
          hpdcache_req_o.phys_indexed = 1'b1,
          hpdcache_req_o.addr_tag = forward_amo ? amo_tag : cva6_req_i.address_tag,
          hpdcache_req_o.pma.uncacheable = hpdcache_req_is_uncacheable,
          hpdcache_req_o.pma.io = 1'b0,
          hpdcache_req_abort_o = 1'b0,  // unused on physically indexed requests
          hpdcache_req_tag_o = '0,  // unused on physically indexed requests
          hpdcache_req_pma_o = '0;  // unused on physically indexed requests
      //  }}}

      //  Response forwarding
      //  {{{
      assign cva6_req_o.data_rvalid = hpdcache_rsp_valid_i && (hpdcache_rsp_i.tid != '1),
          cva6_req_o.data_rdata = hpdcache_rsp_i.rdata,
          cva6_req_o.data_rid = hpdcache_rsp_i.tid,
          cva6_req_o.data_gnt = hpdcache_req_ready_i;

      assign cva6_amo_resp_o.ack = hpdcache_rsp_valid_i && (hpdcache_rsp_i.tid == '1),
          cva6_amo_resp_o.result = amo_is_word ? {{32{amo_resp_word[31]}}, amo_resp_word}
                                                        : hpdcache_rsp_i.rdata[0][63:0];
      //  }}}
    end
    //  }}}
  endgenerate
  //  }}}

  //  Assertions
  //  {{{
  //    pragma translate_off
  forward_one_request_assert :
  assert property (@(posedge clk_i) ($onehot0({forward_store, forward_amo})))
  else $error("Only one request shall be forwarded");
  //    pragma translate_on
  //  }}}
endmodule
