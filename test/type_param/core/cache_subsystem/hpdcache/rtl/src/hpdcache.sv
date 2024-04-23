/*
 *  Copyright 2023 CEA*
 *  *Commissariat a l'Energie Atomique et aux Energies Alternatives (CEA)
 *
 *  SPDX-License-Identifier: Apache-2.0 WITH SHL-2.1
 *
 *  Licensed under the Solderpad Hardware License v 2.1 (the “License”); you
 *  may not use this file except in compliance with the License, or, at your
 *  option, the Apache License version 2.0. You may obtain a copy of the
 *  License at
 *
 *  https://solderpad.org/licenses/SHL-2.1/
 *
 *  Unless required by applicable law or agreed to in writing, any work
 *  distributed under the License is distributed on an “AS IS” BASIS, WITHOUT
 *  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 *  License for the specific language governing permissions and limitations
 *  under the License.
 */
/*
 *  Authors       : Cesar Fuguet
 *  Creation Date : April, 2021
 *  Description   : HPDcache top
 *  History       :
 */
module hpdcache
import hpdcache_pkg::*;
    //  Parameters
    //  {{{
#(
    parameter int  NREQUESTERS           = 1,
    parameter int  HPDcacheMemIdWidth    = 8,
    parameter int  HPDcacheMemDataWidth  = 512,
    parameter type hpdcache_mem_req_t    = logic,
    parameter type hpdcache_mem_req_w_t  = logic,
    parameter type hpdcache_mem_resp_r_t = logic,
    parameter type hpdcache_mem_resp_w_t = logic
)
    //  }}}

    //  Ports
    //  {{{
(
    //      Clock and reset signals
    input  logic                          clk_i,
    input  logic                          rst_ni,

    //      Force the write buffer to send all pending writes
    input  logic                          wbuf_flush_i,

    //      Core request interface
    //         1st cycle
    input  logic                          core_req_valid_i [NREQUESTERS-1:0],
    output logic                          core_req_ready_o [NREQUESTERS-1:0],
    input  hpdcache_req_t                 core_req_i       [NREQUESTERS-1:0],
    //         2nd cycle
    input  logic                          core_req_abort_i [NREQUESTERS-1:0],
    input  hpdcache_tag_t                 core_req_tag_i   [NREQUESTERS-1:0],
    input  hpdcache_pma_t                 core_req_pma_i   [NREQUESTERS-1:0],

    //      Core response interface
    output logic                          core_rsp_valid_o [NREQUESTERS-1:0],
    output hpdcache_rsp_t                 core_rsp_o       [NREQUESTERS-1:0],

    //      Miss read interface
    input  logic                          mem_req_miss_read_ready_i,
    output logic                          mem_req_miss_read_valid_o,
    output hpdcache_mem_req_t             mem_req_miss_read_o,

    output logic                          mem_resp_miss_read_ready_o,
    input  logic                          mem_resp_miss_read_valid_i,
    input  hpdcache_mem_resp_r_t          mem_resp_miss_read_i,

    //      Write-buffer write interface
    input  logic                          mem_req_wbuf_write_ready_i,
    output logic                          mem_req_wbuf_write_valid_o,
    output hpdcache_mem_req_t             mem_req_wbuf_write_o,

    input  logic                          mem_req_wbuf_write_data_ready_i,
    output logic                          mem_req_wbuf_write_data_valid_o,
    output hpdcache_mem_req_w_t           mem_req_wbuf_write_data_o,

    output logic                          mem_resp_wbuf_write_ready_o,
    input  logic                          mem_resp_wbuf_write_valid_i,
    input  hpdcache_mem_resp_w_t          mem_resp_wbuf_write_i,

    //      Uncached read interface
    input  logic                          mem_req_uc_read_ready_i,
    output logic                          mem_req_uc_read_valid_o,
    output hpdcache_mem_req_t             mem_req_uc_read_o,

    output logic                          mem_resp_uc_read_ready_o,
    input  logic                          mem_resp_uc_read_valid_i,
    input  hpdcache_mem_resp_r_t          mem_resp_uc_read_i,

    //      Uncached write interface
    input  logic                          mem_req_uc_write_ready_i,
    output logic                          mem_req_uc_write_valid_o,
    output hpdcache_mem_req_t             mem_req_uc_write_o,

    input  logic                          mem_req_uc_write_data_ready_i,
    output logic                          mem_req_uc_write_data_valid_o,
    output hpdcache_mem_req_w_t           mem_req_uc_write_data_o,

    output logic                          mem_resp_uc_write_ready_o,
    input  logic                          mem_resp_uc_write_valid_i,
    input  hpdcache_mem_resp_w_t          mem_resp_uc_write_i,

    //      Performance events
    output logic                          evt_cache_write_miss_o,
    output logic                          evt_cache_read_miss_o,
    output logic                          evt_uncached_req_o,
    output logic                          evt_cmo_req_o,
    output logic                          evt_write_req_o,
    output logic                          evt_read_req_o,
    output logic                          evt_prefetch_req_o,
    output logic                          evt_req_on_hold_o,
    output logic                          evt_rtab_rollback_o,
    output logic                          evt_stall_refill_o,
    output logic                          evt_stall_o,

    //      Status interface
    output logic                          wbuf_empty_o,

    //      Configuration interface
    input  logic                          cfg_enable_i,
    input  wbuf_timecnt_t                 cfg_wbuf_threshold_i,
    input  logic                          cfg_wbuf_reset_timecnt_on_write_i,
    input  logic                          cfg_wbuf_sequential_waw_i,
    input  logic                          cfg_wbuf_inhibit_write_coalescing_i,
    input  logic                          cfg_prefetch_updt_plru_i,
    input  logic                          cfg_error_on_cacheable_amo_i,
    input  logic                          cfg_rtab_single_entry_i
);

    //  }}}

    //  Declaration of internal signals
    //  {{{
    logic                  refill_req_valid;
    logic                  refill_req_ready;
    logic                  refill_busy;
    logic                  refill_updt_plru;
    hpdcache_set_t         refill_set;
    hpdcache_dir_entry_t   refill_dir_entry;
    hpdcache_way_vector_t  refill_read_victim_way;
    hpdcache_way_vector_t  refill_write_victim_way;
    logic                  refill_write_dir;
    logic                  refill_write_data;
    hpdcache_word_t        refill_word;
    hpdcache_refill_data_t refill_data;
    logic                  refill_core_rsp_valid;
    hpdcache_rsp_t         refill_core_rsp;
    hpdcache_nline_t       refill_nline;
    logic                  refill_updt_rtab;

    logic                  miss_mshr_empty;
    logic                  miss_mshr_check;
    mshr_set_t             miss_mshr_check_set;
    mshr_tag_t             miss_mshr_check_tag;
    logic                  miss_mshr_hit;
    logic                  miss_mshr_alloc_cs;
    logic                  miss_mshr_alloc;
    logic                  miss_mshr_alloc_ready;
    logic                  miss_mshr_alloc_full;
    hpdcache_nline_t       miss_mshr_alloc_nline;
    hpdcache_req_tid_t     miss_mshr_alloc_tid;
    hpdcache_req_sid_t     miss_mshr_alloc_sid;
    hpdcache_word_t        miss_mshr_alloc_word;
    logic                  miss_mshr_alloc_need_rsp;
    logic                  miss_mshr_alloc_is_prefetch;

    logic                  wbuf_flush_all;
    logic                  wbuf_write;
    logic                  wbuf_write_ready;
    wbuf_addr_t            wbuf_write_addr;
    wbuf_data_t            wbuf_write_data;
    wbuf_be_t              wbuf_write_be;
    logic                  wbuf_write_uncacheable;
    logic                  wbuf_read_hit;
    logic                  wbuf_read_flush_hit;
    hpdcache_req_addr_t    wbuf_rtab_addr;
    logic                  wbuf_rtab_is_read;
    logic                  wbuf_rtab_hit_open;
    logic                  wbuf_rtab_hit_pend;
    logic                  wbuf_rtab_hit_sent;
    logic                  wbuf_rtab_not_ready;

    logic                  uc_ready;
    logic                  uc_req_valid;
    hpdcache_uc_op_t       uc_req_op;
    hpdcache_req_addr_t    uc_req_addr;
    hpdcache_req_size_t    uc_req_size;
    hpdcache_req_data_t    uc_req_data;
    hpdcache_req_be_t      uc_req_be;
    logic                  uc_req_uncacheable;
    hpdcache_req_sid_t     uc_req_sid;
    hpdcache_req_tid_t     uc_req_tid;
    logic                  uc_req_need_rsp;
    logic                  uc_wbuf_flush_all;
    logic                  uc_dir_amo_match;
    hpdcache_set_t         uc_dir_amo_match_set;
    hpdcache_tag_t         uc_dir_amo_match_tag;
    logic                  uc_dir_amo_update_plru;
    hpdcache_way_vector_t  uc_dir_amo_hit_way;
    logic                  uc_data_amo_write;
    logic                  uc_data_amo_write_enable;
    hpdcache_set_t         uc_data_amo_write_set;
    hpdcache_req_size_t    uc_data_amo_write_size;
    hpdcache_word_t        uc_data_amo_write_word;
    logic [63:0]           uc_data_amo_write_data;
    logic  [7:0]           uc_data_amo_write_be;
    logic                  uc_lrsc_snoop;
    hpdcache_req_addr_t    uc_lrsc_snoop_addr;
    hpdcache_req_size_t    uc_lrsc_snoop_size;
    logic                  uc_core_rsp_ready;
    logic                  uc_core_rsp_valid;
    hpdcache_rsp_t         uc_core_rsp;

    logic                  cmo_req_valid;
    logic                  cmo_ready;
    hpdcache_cmoh_op_t     cmo_req_op;
    hpdcache_req_addr_t    cmo_req_addr;
    hpdcache_req_data_t    cmo_req_wdata;
    logic                  cmo_wbuf_flush_all;
    logic                  cmo_dir_check;
    hpdcache_set_t         cmo_dir_check_set;
    hpdcache_tag_t         cmo_dir_check_tag;
    hpdcache_way_vector_t  cmo_dir_check_hit_way;
    logic                  cmo_dir_inval;
    hpdcache_set_t         cmo_dir_inval_set;
    hpdcache_way_vector_t  cmo_dir_inval_way;

    logic                  rtab_empty;
    logic                  ctrl_empty;

    logic                  core_rsp_valid;
    hpdcache_rsp_t         core_rsp;

    logic                  arb_req_valid;
    logic                  arb_req_ready;
    hpdcache_req_t         arb_req;
    logic                  arb_abort;
    hpdcache_tag_t         arb_tag;
    hpdcache_pma_t         arb_pma;

    localparam logic [HPDcacheMemIdWidth-1:0] HPDCACHE_UC_READ_ID  = {HPDcacheMemIdWidth{1'b1}};
    localparam logic [HPDcacheMemIdWidth-1:0] HPDCACHE_UC_WRITE_ID = {HPDcacheMemIdWidth{1'b1}};
    //  }}}

    //  Requesters arbiter
    //  {{{
    hpdcache_core_arbiter #(
        .NREQUESTERS                        (NREQUESTERS)
    ) core_req_arbiter_i (
        .clk_i,
        .rst_ni,

        .core_req_valid_i,
        .core_req_ready_o,
        .core_req_i,
        .core_req_abort_i,
        .core_req_tag_i,
        .core_req_pma_i,

        .core_rsp_valid_i                   (core_rsp_valid),
        .core_rsp_i                         (core_rsp),
        .core_rsp_valid_o,
        .core_rsp_o,

        .arb_req_valid_o                    (arb_req_valid),
        .arb_req_ready_i                    (arb_req_ready),
        .arb_req_o                          (arb_req),
        .arb_abort_o                        (arb_abort),
        .arb_tag_o                          (arb_tag),
        .arb_pma_o                          (arb_pma)
    );
    //  }}}

    //  HPDcache controller
    //  {{{
    hpdcache_ctrl hpdcache_ctrl_i(
        .clk_i,
        .rst_ni,

        .core_req_valid_i                   (arb_req_valid),
        .core_req_ready_o                   (arb_req_ready),
        .core_req_i                         (arb_req),
        .core_req_abort_i                   (arb_abort),
        .core_req_tag_i                     (arb_tag),
        .core_req_pma_i                     (arb_pma),

        .core_rsp_valid_o                   (core_rsp_valid),
        .core_rsp_o                         (core_rsp),

        .wbuf_flush_i,

        .cachedir_hit_o                     (/* unused */),

        .miss_mshr_check_o                  (miss_mshr_check),
        .miss_mshr_check_set_o              (miss_mshr_check_set),
        .miss_mshr_check_tag_o              (miss_mshr_check_tag),
        .miss_mshr_alloc_o                  (miss_mshr_alloc),
        .miss_mshr_alloc_cs_o               (miss_mshr_alloc_cs),
        .miss_mshr_alloc_ready_i            (miss_mshr_alloc_ready),
        .miss_mshr_alloc_full_i             (miss_mshr_alloc_full),
        .miss_mshr_alloc_nline_o            (miss_mshr_alloc_nline),
        .miss_mshr_alloc_tid_o              (miss_mshr_alloc_tid),
        .miss_mshr_alloc_sid_o              (miss_mshr_alloc_sid),
        .miss_mshr_alloc_word_o             (miss_mshr_alloc_word),
        .miss_mshr_alloc_need_rsp_o         (miss_mshr_alloc_need_rsp),
        .miss_mshr_alloc_is_prefetch_o      (miss_mshr_alloc_is_prefetch),
        .miss_mshr_hit_i                    (miss_mshr_hit),

        .refill_req_valid_i                 (refill_req_valid),
        .refill_req_ready_o                 (refill_req_ready),
        .refill_busy_i                      (refill_busy),
        .refill_updt_plru_i                 (refill_updt_plru),
        .refill_set_i                       (refill_set),
        .refill_dir_entry_i                 (refill_dir_entry),
        .refill_victim_way_o                (refill_read_victim_way),
        .refill_victim_way_i                (refill_write_victim_way),
        .refill_write_dir_i                 (refill_write_dir),
        .refill_write_data_i                (refill_write_data),
        .refill_word_i                      (refill_word),
        .refill_data_i                      (refill_data),
        .refill_core_rsp_valid_i            (refill_core_rsp_valid),
        .refill_core_rsp_i                  (refill_core_rsp),
        .refill_nline_i                     (refill_nline),
        .refill_updt_rtab_i                 (refill_updt_rtab),

        .wbuf_empty_i                       (wbuf_empty_o),
        .wbuf_flush_all_o                   (wbuf_flush_all),
        .wbuf_write_o                       (wbuf_write),
        .wbuf_write_ready_i                 (wbuf_write_ready),
        .wbuf_write_addr_o                  (wbuf_write_addr),
        .wbuf_write_data_o                  (wbuf_write_data),
        .wbuf_write_be_o                    (wbuf_write_be),
        .wbuf_write_uncacheable_o           (wbuf_write_uncacheable),
        .wbuf_read_hit_i                    (wbuf_read_hit),
        .wbuf_read_flush_hit_o              (wbuf_read_flush_hit),
        .wbuf_rtab_addr_o                   (wbuf_rtab_addr),
        .wbuf_rtab_is_read_o                (wbuf_rtab_is_read),
        .wbuf_rtab_hit_open_i               (wbuf_rtab_hit_open),
        .wbuf_rtab_hit_pend_i               (wbuf_rtab_hit_pend),
        .wbuf_rtab_hit_sent_i               (wbuf_rtab_hit_sent),
        .wbuf_rtab_not_ready_i              (wbuf_rtab_not_ready),

        .uc_busy_i                          (~uc_ready),
        .uc_lrsc_snoop_o                    (uc_lrsc_snoop),
        .uc_lrsc_snoop_addr_o               (uc_lrsc_snoop_addr),
        .uc_lrsc_snoop_size_o               (uc_lrsc_snoop_size),
        .uc_req_valid_o                     (uc_req_valid),
        .uc_req_op_o                        (uc_req_op),
        .uc_req_addr_o                      (uc_req_addr),
        .uc_req_size_o                      (uc_req_size),
        .uc_req_data_o                      (uc_req_data),
        .uc_req_be_o                        (uc_req_be),
        .uc_req_uc_o                        (uc_req_uncacheable),
        .uc_req_sid_o                       (uc_req_sid),
        .uc_req_tid_o                       (uc_req_tid),
        .uc_req_need_rsp_o                  (uc_req_need_rsp),
        .uc_wbuf_flush_all_i                (uc_wbuf_flush_all),
        .uc_dir_amo_match_i                 (uc_dir_amo_match),
        .uc_dir_amo_match_set_i             (uc_dir_amo_match_set),
        .uc_dir_amo_match_tag_i             (uc_dir_amo_match_tag),
        .uc_dir_amo_update_plru_i           (uc_dir_amo_update_plru),
        .uc_dir_amo_hit_way_o               (uc_dir_amo_hit_way),
        .uc_data_amo_write_i                (uc_data_amo_write),
        .uc_data_amo_write_enable_i         (uc_data_amo_write_enable),
        .uc_data_amo_write_set_i            (uc_data_amo_write_set),
        .uc_data_amo_write_size_i           (uc_data_amo_write_size),
        .uc_data_amo_write_word_i           (uc_data_amo_write_word),
        .uc_data_amo_write_data_i           (uc_data_amo_write_data),
        .uc_data_amo_write_be_i             (uc_data_amo_write_be),
        .uc_core_rsp_ready_o                (uc_core_rsp_ready),
        .uc_core_rsp_valid_i                (uc_core_rsp_valid),
        .uc_core_rsp_i                      (uc_core_rsp),

        .cmo_busy_i                         (~cmo_ready),
        .cmo_req_valid_o                    (cmo_req_valid),
        .cmo_req_op_o                       (cmo_req_op),
        .cmo_req_addr_o                     (cmo_req_addr),
        .cmo_req_wdata_o                    (cmo_req_wdata),
        .cmo_wbuf_flush_all_i               (cmo_wbuf_flush_all),
        .cmo_dir_check_i                    (cmo_dir_check),
        .cmo_dir_check_set_i                (cmo_dir_check_set),
        .cmo_dir_check_tag_i                (cmo_dir_check_tag),
        .cmo_dir_check_hit_way_o            (cmo_dir_check_hit_way),
        .cmo_dir_inval_i                    (cmo_dir_inval),
        .cmo_dir_inval_set_i                (cmo_dir_inval_set),
        .cmo_dir_inval_way_i                (cmo_dir_inval_way),

        .rtab_empty_o                       (rtab_empty),
        .ctrl_empty_o                       (ctrl_empty),

        .cfg_enable_i,
        .cfg_rtab_single_entry_i,

        .evt_cache_write_miss_o,
        .evt_cache_read_miss_o,
        .evt_uncached_req_o,
        .evt_cmo_req_o,
        .evt_write_req_o,
        .evt_read_req_o,
        .evt_prefetch_req_o,
        .evt_req_on_hold_o,
        .evt_rtab_rollback_o,
        .evt_stall_refill_o,
        .evt_stall_o
    );
    //  }}}

    //  HPDcache write-buffer
    //  {{{
    hpdcache_wbuf_wrapper #(
        .HPDcacheMemIdWidth                 (HPDcacheMemIdWidth),
        .HPDcacheMemDataWidth               (HPDcacheMemDataWidth),
        .hpdcache_mem_req_t                 (hpdcache_mem_req_t),
        .hpdcache_mem_req_w_t               (hpdcache_mem_req_w_t),
        .hpdcache_mem_resp_w_t              (hpdcache_mem_resp_w_t)
    ) hpdcache_wbuf_i(
        .clk_i,
        .rst_ni,

        .empty_o                            (wbuf_empty_o),
        .full_o                             (/* unused */),
        .flush_all_i                        (wbuf_flush_all),

        .cfg_threshold_i                    (cfg_wbuf_threshold_i),
        .cfg_reset_timecnt_on_write_i       (cfg_wbuf_reset_timecnt_on_write_i),
        .cfg_sequential_waw_i               (cfg_wbuf_sequential_waw_i),
        .cfg_inhibit_write_coalescing_i     (cfg_wbuf_inhibit_write_coalescing_i),

        .write_i                            (wbuf_write),
        .write_ready_o                      (wbuf_write_ready),
        .write_addr_i                       (wbuf_write_addr),
        .write_data_i                       (wbuf_write_data),
        .write_be_i                         (wbuf_write_be),
        .write_uc_i                         (wbuf_write_uncacheable),

        .read_addr_i                        (wbuf_write_addr),
        .read_hit_o                         (wbuf_read_hit),
        .read_flush_hit_i                   (wbuf_read_flush_hit),

        .replay_addr_i                      (wbuf_rtab_addr),
        .replay_is_read_i                   (wbuf_rtab_is_read),
        .replay_open_hit_o                  (wbuf_rtab_hit_open),
        .replay_pend_hit_o                  (wbuf_rtab_hit_pend),
        .replay_sent_hit_o                  (wbuf_rtab_hit_sent),
        .replay_not_ready_o                 (wbuf_rtab_not_ready),

        .mem_req_write_ready_i              (mem_req_wbuf_write_ready_i),
        .mem_req_write_valid_o              (mem_req_wbuf_write_valid_o),
        .mem_req_write_o                    (mem_req_wbuf_write_o),

        .mem_req_write_data_ready_i         (mem_req_wbuf_write_data_ready_i),
        .mem_req_write_data_valid_o         (mem_req_wbuf_write_data_valid_o),
        .mem_req_write_data_o               (mem_req_wbuf_write_data_o),

        .mem_resp_write_ready_o             (mem_resp_wbuf_write_ready_o),
        .mem_resp_write_valid_i             (mem_resp_wbuf_write_valid_i),
        .mem_resp_write_i                   (mem_resp_wbuf_write_i)
    );
    //  }}}

    //  Miss handler
    //  {{{
    hpdcache_miss_handler #(
        .HPDcacheMemIdWidth                 (HPDcacheMemIdWidth),
        .HPDcacheMemDataWidth               (HPDcacheMemDataWidth),
        .hpdcache_mem_req_t                 (hpdcache_mem_req_t),
        .hpdcache_mem_resp_r_t              (hpdcache_mem_resp_r_t)
    ) hpdcache_miss_handler_i(
        .clk_i,
        .rst_ni,

        .mshr_empty_o                       (miss_mshr_empty),
        .mshr_full_o                        (/* unused */),

        .cfg_prefetch_updt_plru_i,

        .mshr_check_i                       (miss_mshr_check),
        .mshr_check_set_i                   (miss_mshr_check_set),
        .mshr_check_tag_i                   (miss_mshr_check_tag),
        .mshr_check_hit_o                   (miss_mshr_hit),

        .mshr_alloc_ready_o                 (miss_mshr_alloc_ready),
        .mshr_alloc_i                       (miss_mshr_alloc),
        .mshr_alloc_cs_i                    (miss_mshr_alloc_cs),
        .mshr_alloc_full_o                  (miss_mshr_alloc_full),
        .mshr_alloc_nline_i                 (miss_mshr_alloc_nline),
        .mshr_alloc_tid_i                   (miss_mshr_alloc_tid),
        .mshr_alloc_sid_i                   (miss_mshr_alloc_sid),
        .mshr_alloc_word_i                  (miss_mshr_alloc_word),
        .mshr_alloc_need_rsp_i              (miss_mshr_alloc_need_rsp),
        .mshr_alloc_is_prefetch_i           (miss_mshr_alloc_is_prefetch),

        .refill_req_ready_i                 (refill_req_ready),
        .refill_req_valid_o                 (refill_req_valid),
        .refill_busy_o                      (refill_busy),
        .refill_updt_plru_o                 (refill_updt_plru),
        .refill_set_o                       (refill_set),
        .refill_dir_entry_o                 (refill_dir_entry),
        .refill_victim_way_i                (refill_read_victim_way),
        .refill_write_dir_o                 (refill_write_dir),
        .refill_write_data_o                (refill_write_data),
        .refill_victim_way_o                (refill_write_victim_way),
        .refill_data_o                      (refill_data),
        .refill_word_o                      (refill_word),
        .refill_nline_o                     (refill_nline),
        .refill_updt_rtab_o                 (refill_updt_rtab),

        .refill_core_rsp_valid_o            (refill_core_rsp_valid),
        .refill_core_rsp_o                  (refill_core_rsp),

        .mem_req_ready_i                    (mem_req_miss_read_ready_i),
        .mem_req_valid_o                    (mem_req_miss_read_valid_o),
        .mem_req_o                          (mem_req_miss_read_o),

        .mem_resp_ready_o                   (mem_resp_miss_read_ready_o),
        .mem_resp_valid_i                   (mem_resp_miss_read_valid_i),
        .mem_resp_i                         (mem_resp_miss_read_i)
    );
    //  }}}

    //  Uncacheable request handler
    //  {{{
    hpdcache_uncached #(
        .HPDcacheMemIdWidth            (HPDcacheMemIdWidth),
        .HPDcacheMemDataWidth          (HPDcacheMemDataWidth),
        .hpdcache_mem_req_t            (hpdcache_mem_req_t),
        .hpdcache_mem_req_w_t          (hpdcache_mem_req_w_t),
        .hpdcache_mem_resp_r_t         (hpdcache_mem_resp_r_t),
        .hpdcache_mem_resp_w_t         (hpdcache_mem_resp_w_t)
    ) hpdcache_uc_i(
        .clk_i,
        .rst_ni,

        .wbuf_empty_i                  (wbuf_empty_o),
        .mshr_empty_i                  (miss_mshr_empty),
        .rtab_empty_i                  (rtab_empty),
        .ctrl_empty_i                  (ctrl_empty),

        .req_valid_i                   (uc_req_valid),
        .req_ready_o                   (uc_ready),
        .req_op_i                      (uc_req_op),
        .req_addr_i                    (uc_req_addr),
        .req_size_i                    (uc_req_size),
        .req_data_i                    (uc_req_data),
        .req_be_i                      (uc_req_be),
        .req_uc_i                      (uc_req_uncacheable),
        .req_sid_i                     (uc_req_sid),
        .req_tid_i                     (uc_req_tid),
        .req_need_rsp_i                (uc_req_need_rsp),

        .wbuf_flush_all_o              (uc_wbuf_flush_all),

        .dir_amo_match_o               (uc_dir_amo_match),
        .dir_amo_match_set_o           (uc_dir_amo_match_set),
        .dir_amo_match_tag_o           (uc_dir_amo_match_tag),
        .dir_amo_update_plru_o         (uc_dir_amo_update_plru),
        .dir_amo_hit_way_i             (uc_dir_amo_hit_way),

        .data_amo_write_o              (uc_data_amo_write),
        .data_amo_write_enable_o       (uc_data_amo_write_enable),
        .data_amo_write_set_o          (uc_data_amo_write_set),
        .data_amo_write_size_o         (uc_data_amo_write_size),
        .data_amo_write_word_o         (uc_data_amo_write_word),
        .data_amo_write_data_o         (uc_data_amo_write_data),
        .data_amo_write_be_o           (uc_data_amo_write_be),

        .lrsc_snoop_i                  (uc_lrsc_snoop),
        .lrsc_snoop_addr_i             (uc_lrsc_snoop_addr),
        .lrsc_snoop_size_i             (uc_lrsc_snoop_size),

        .core_rsp_ready_i              (uc_core_rsp_ready),
        .core_rsp_valid_o              (uc_core_rsp_valid),
        .core_rsp_o                    (uc_core_rsp),

        .mem_read_id_i                 (HPDCACHE_UC_READ_ID),
        .mem_write_id_i                (HPDCACHE_UC_WRITE_ID),

        .mem_req_read_ready_i          (mem_req_uc_read_ready_i),
        .mem_req_read_valid_o          (mem_req_uc_read_valid_o),
        .mem_req_read_o                (mem_req_uc_read_o),

        .mem_resp_read_ready_o         (mem_resp_uc_read_ready_o),
        .mem_resp_read_valid_i         (mem_resp_uc_read_valid_i),
        .mem_resp_read_i               (mem_resp_uc_read_i),

        .mem_req_write_ready_i         (mem_req_uc_write_ready_i),
        .mem_req_write_valid_o         (mem_req_uc_write_valid_o),
        .mem_req_write_o               (mem_req_uc_write_o),

        .mem_req_write_data_ready_i    (mem_req_uc_write_data_ready_i),
        .mem_req_write_data_valid_o    (mem_req_uc_write_data_valid_o),
        .mem_req_write_data_o          (mem_req_uc_write_data_o),

        .mem_resp_write_ready_o        (mem_resp_uc_write_ready_o),
        .mem_resp_write_valid_i        (mem_resp_uc_write_valid_i),
        .mem_resp_write_i              (mem_resp_uc_write_i),

        .cfg_error_on_cacheable_amo_i
    );

    //  CMO Request Handler
    //  {{{
    hpdcache_cmo hpdcache_cmo_i(
        .clk_i,
        .rst_ni,

        .wbuf_empty_i           (wbuf_empty_o),
        .mshr_empty_i           (miss_mshr_empty),
        .rtab_empty_i           (rtab_empty),
        .ctrl_empty_i           (ctrl_empty),

        .req_valid_i            (cmo_req_valid),
        .req_ready_o            (cmo_ready),
        .req_op_i               (cmo_req_op),
        .req_addr_i             (cmo_req_addr),
        .req_wdata_i            (cmo_req_wdata),

        .wbuf_flush_all_o       (cmo_wbuf_flush_all),

        .dir_check_o            (cmo_dir_check),
        .dir_check_set_o        (cmo_dir_check_set),
        .dir_check_tag_o        (cmo_dir_check_tag),
        .dir_check_hit_way_i    (cmo_dir_check_hit_way),

        .dir_inval_o            (cmo_dir_inval),
        .dir_inval_set_o        (cmo_dir_inval_set),
        .dir_inval_way_o        (cmo_dir_inval_way)
    );
    //  }}}

    //  Assertions
    //  {{{
    // pragma translate_off
    initial begin
        req_access_width_assert:
            assert (HPDCACHE_REQ_WORDS <= HPDCACHE_ACCESS_WORDS) else
                $error("req data width shall be l.e. to cache access width");
        refill_access_width_assert:
            assert (HPDCACHE_CL_WORDS >= HPDCACHE_ACCESS_WORDS) else
                $error("cache access width shall be l.e. to cache-line width");
        miss_mem_id_width_assert:
            assert (HPDcacheMemIdWidth >= (HPDCACHE_MSHR_WAY_WIDTH + HPDCACHE_MSHR_SET_WIDTH)) else
                $error("insufficient ID bits on the mem interface to transport misses");
        wbuf_mem_id_width_assert:
            assert (HPDcacheMemIdWidth >= HPDCACHE_WBUF_DIR_PTR_WIDTH) else
                $error("insufficient ID bits on the mem interface to transport writes");

    end
    // pragma translate_on
    // }}}

endmodule
