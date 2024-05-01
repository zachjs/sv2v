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
 *  Description   : HPDcache controller
 *  History       :
 */
module hpdcache_ctrl
    // Package imports
    // {{{
import hpdcache_pkg::*;
    // }}}

    // Ports
    // {{{
(
    input  logic                  clk_i,
    input  logic                  rst_ni,

    //      Core request interface
    input  logic                  core_req_valid_i,
    output logic                  core_req_ready_o,
    input  hpdcache_req_t         core_req_i,
    input  logic                  core_req_abort_i,
    input  hpdcache_tag_t         core_req_tag_i,
    input  hpdcache_pma_t         core_req_pma_i,

    //      Core response interface
    output logic                  core_rsp_valid_o,
    output hpdcache_rsp_t         core_rsp_o,

    //      Force the write buffer to send all pending writes
    input  logic                  wbuf_flush_i,

    //      Global control signals
    output logic                  cachedir_hit_o,

    //      Miss handler interface
    output logic                  miss_mshr_check_o,
    output mshr_set_t             miss_mshr_check_set_o,
    output mshr_tag_t             miss_mshr_check_tag_o,
    output logic                  miss_mshr_alloc_o,
    output logic                  miss_mshr_alloc_cs_o,
    input  logic                  miss_mshr_alloc_ready_i,
    input  logic                  miss_mshr_alloc_full_i,
    output hpdcache_nline_t       miss_mshr_alloc_nline_o,
    output hpdcache_req_tid_t     miss_mshr_alloc_tid_o,
    output hpdcache_req_sid_t     miss_mshr_alloc_sid_o,
    output hpdcache_word_t        miss_mshr_alloc_word_o,
    output logic                  miss_mshr_alloc_need_rsp_o,
    output logic                  miss_mshr_alloc_is_prefetch_o,
    input  logic                  miss_mshr_hit_i,

    //      Refill interface
    input  logic                  refill_req_valid_i,
    output logic                  refill_req_ready_o,
    input  logic                  refill_busy_i,
    input  logic                  refill_updt_plru_i,
    input  hpdcache_set_t         refill_set_i,
    input  hpdcache_dir_entry_t   refill_dir_entry_i,
    output hpdcache_way_vector_t  refill_victim_way_o,
    input  hpdcache_way_vector_t  refill_victim_way_i,
    input  logic                  refill_write_dir_i,
    input  logic                  refill_write_data_i,
    input  hpdcache_word_t        refill_word_i,
    input  hpdcache_refill_data_t refill_data_i,
    input  logic                  refill_core_rsp_valid_i,
    input  hpdcache_rsp_t         refill_core_rsp_i,
    input  hpdcache_nline_t       refill_nline_i,
    input  logic                  refill_updt_rtab_i,

    //      Write buffer interface
    input  logic                  wbuf_empty_i,
    output logic                  wbuf_flush_all_o,
    output logic                  wbuf_write_o,
    input  logic                  wbuf_write_ready_i,
    output wbuf_addr_t            wbuf_write_addr_o,
    output wbuf_data_t            wbuf_write_data_o,
    output wbuf_be_t              wbuf_write_be_o,
    output logic                  wbuf_write_uncacheable_o,
    input  logic                  wbuf_read_hit_i,
    output logic                  wbuf_read_flush_hit_o,
    output hpdcache_req_addr_t    wbuf_rtab_addr_o,
    output logic                  wbuf_rtab_is_read_o,
    input  logic                  wbuf_rtab_hit_open_i,
    input  logic                  wbuf_rtab_hit_pend_i,
    input  logic                  wbuf_rtab_hit_sent_i,
    input  logic                  wbuf_rtab_not_ready_i,

    //      Uncacheable request handler
    input  logic                  uc_busy_i,
    output logic                  uc_lrsc_snoop_o,
    output hpdcache_req_addr_t    uc_lrsc_snoop_addr_o,
    output hpdcache_req_size_t    uc_lrsc_snoop_size_o,
    output logic                  uc_req_valid_o,
    output hpdcache_uc_op_t       uc_req_op_o,
    output hpdcache_req_addr_t    uc_req_addr_o,
    output hpdcache_req_size_t    uc_req_size_o,
    output hpdcache_req_data_t    uc_req_data_o,
    output hpdcache_req_be_t      uc_req_be_o,
    output logic                  uc_req_uc_o,
    output hpdcache_req_sid_t     uc_req_sid_o,
    output hpdcache_req_tid_t     uc_req_tid_o,
    output logic                  uc_req_need_rsp_o,
    input  logic                  uc_wbuf_flush_all_i,
    input  logic                  uc_dir_amo_match_i,
    input  hpdcache_set_t         uc_dir_amo_match_set_i,
    input  hpdcache_tag_t         uc_dir_amo_match_tag_i,
    input  logic                  uc_dir_amo_update_plru_i,
    output hpdcache_way_vector_t  uc_dir_amo_hit_way_o,
    input  logic                  uc_data_amo_write_i,
    input  logic                  uc_data_amo_write_enable_i,
    input  hpdcache_set_t         uc_data_amo_write_set_i,
    input  hpdcache_req_size_t    uc_data_amo_write_size_i,
    input  hpdcache_word_t        uc_data_amo_write_word_i,
    input  logic [63:0]           uc_data_amo_write_data_i,
    input  logic  [7:0]           uc_data_amo_write_be_i,
    output logic                  uc_core_rsp_ready_o,
    input  logic                  uc_core_rsp_valid_i,
    input  hpdcache_rsp_t         uc_core_rsp_i,

    //      Cache Management Operation (CMO)
    input  logic                  cmo_busy_i,
    output logic                  cmo_req_valid_o,
    output hpdcache_cmoh_op_t     cmo_req_op_o,
    output hpdcache_req_addr_t    cmo_req_addr_o,
    output hpdcache_req_data_t    cmo_req_wdata_o,
    input  logic                  cmo_wbuf_flush_all_i,
    input  logic                  cmo_dir_check_i,
    input  hpdcache_set_t         cmo_dir_check_set_i,
    input  hpdcache_tag_t         cmo_dir_check_tag_i,
    output hpdcache_way_vector_t  cmo_dir_check_hit_way_o,
    input  logic                  cmo_dir_inval_i,
    input  hpdcache_set_t         cmo_dir_inval_set_i,
    input  hpdcache_way_vector_t  cmo_dir_inval_way_i,

    output logic                  rtab_empty_o,
    output logic                  ctrl_empty_o,

    //   Configuration signals
    input  logic                  cfg_enable_i,
    input  logic                  cfg_rtab_single_entry_i,

    //   Performance events
    output logic                  evt_cache_write_miss_o,
    output logic                  evt_cache_read_miss_o,
    output logic                  evt_uncached_req_o,
    output logic                  evt_cmo_req_o,
    output logic                  evt_write_req_o,
    output logic                  evt_read_req_o,
    output logic                  evt_prefetch_req_o,
    output logic                  evt_req_on_hold_o,
    output logic                  evt_rtab_rollback_o,
    output logic                  evt_stall_refill_o,
    output logic                  evt_stall_o
);
    // }}}

    //  Definition of internal registers
    //  {{{
    logic                    st1_req_valid_q, st1_req_valid_d;
    hpdcache_req_t           st1_req_q;
    logic                    st1_req_rtab_q;
    rtab_ptr_t               st1_rtab_pop_try_ptr_q;

    logic                    st2_req_valid_q, st2_req_valid_d;
    logic                    st2_req_is_prefetch_q, st2_req_is_prefetch_d;
    logic                    st2_req_need_rsp_q;
    hpdcache_req_addr_t      st2_req_addr_q;
    hpdcache_req_sid_t       st2_req_sid_q;
    hpdcache_req_tid_t       st2_req_tid_q;
    //  }}}

    //  Definition of internal signals
    //  {{{
    logic [1:0]              st0_arb_req;
    logic [1:0]              st0_arb_req_grant;
    logic                    st0_arb_ready;

    logic                    st0_req_ready;

    logic                    st0_req_valid;
    hpdcache_req_t           st0_req;
    logic                    st0_req_is_uncacheable;
    logic                    st0_req_is_load;
    logic                    st0_req_is_store;
    logic                    st0_req_is_amo;
    logic                    st0_req_is_cmo_fence;
    logic                    st0_req_is_cmo_inval;
    logic                    st0_req_is_cmo_prefetch;
    logic                    st0_req_cachedir_read;
    logic                    st0_req_cachedata_read;
    hpdcache_set_t           st0_req_set;
    hpdcache_word_t          st0_req_word;
    logic                    st0_rtab_pop_try_valid;
    logic                    st0_rtab_pop_try_ready;
    hpdcache_req_t           st0_rtab_pop_try_req;
    logic                    st0_rtab_pop_try_sel;
    rtab_ptr_t               st0_rtab_pop_try_ptr;

    logic                    st1_rsp_valid;
    logic                    st1_rsp_aborted;
    hpdcache_req_t           st1_req;
    logic                    st1_req_abort;
    logic                    st1_req_cachedata_write;
    logic                    st1_req_cachedata_write_enable;
    hpdcache_pma_t           st1_req_pma;
    hpdcache_tag_t           st1_req_tag;
    hpdcache_set_t           st1_req_set;
    hpdcache_word_t          st1_req_word;
    hpdcache_nline_t         st1_req_nline;
    hpdcache_req_addr_t      st1_req_addr;
    logic                    st1_req_updt_lru;
    logic                    st1_req_is_uncacheable;
    logic                    st1_req_is_load;
    logic                    st1_req_is_store;
    logic                    st1_req_is_amo;
    logic                    st1_req_is_amo_lr;
    logic                    st1_req_is_amo_sc;
    logic                    st1_req_is_amo_swap;
    logic                    st1_req_is_amo_add;
    logic                    st1_req_is_amo_and;
    logic                    st1_req_is_amo_or;
    logic                    st1_req_is_amo_xor;
    logic                    st1_req_is_amo_max;
    logic                    st1_req_is_amo_maxu;
    logic                    st1_req_is_amo_min;
    logic                    st1_req_is_amo_minu;
    logic                    st1_req_is_cmo_inval;
    logic                    st1_req_is_cmo_fence;
    logic                    st1_req_is_cmo_prefetch;
    hpdcache_way_vector_t    st1_dir_hit;
    hpdcache_req_data_t      st1_read_data;
    logic                    st1_rtab_alloc;
    logic                    st1_rtab_alloc_and_link;
    logic                    st1_rtab_pop_try_commit;
    logic                    st1_rtab_pop_try_rback;
    logic                    st1_rtab_mshr_hit;
    logic                    st1_rtab_mshr_full;
    logic                    st1_rtab_mshr_ready;
    logic                    st1_rtab_wbuf_hit;
    logic                    st1_rtab_wbuf_not_ready;
    logic                    st1_rtab_check;
    logic                    st1_rtab_check_hit;

    logic                    st2_req_we;
    hpdcache_word_t          st2_req_word;

    logic                    rtab_full;

    logic                    hpdcache_init_ready;
    //  }}}

    //  Decoding of the request
    //  {{{
    //     Select between request in the replay table or a new core requests
    assign st0_req_valid           = st0_rtab_pop_try_sel ? st0_rtab_pop_try_valid
                                                          : core_req_valid_i,
           st0_req.addr_offset     = st0_rtab_pop_try_sel ? st0_rtab_pop_try_req.addr_offset
                                                          : core_req_i.addr_offset,
           st0_req.addr_tag        = st0_rtab_pop_try_sel ? st0_rtab_pop_try_req.addr_tag
                                                          : core_req_i.addr_tag,
           st0_req.wdata           = st0_rtab_pop_try_sel ? st0_rtab_pop_try_req.wdata
                                                          : core_req_i.wdata,
           st0_req.op              = st0_rtab_pop_try_sel ? st0_rtab_pop_try_req.op
                                                          : core_req_i.op,
           st0_req.be              = st0_rtab_pop_try_sel ? st0_rtab_pop_try_req.be
                                                          : core_req_i.be,
           st0_req.size            = st0_rtab_pop_try_sel ? st0_rtab_pop_try_req.size
                                                          : core_req_i.size,
           st0_req.sid             = st0_rtab_pop_try_sel ? st0_rtab_pop_try_req.sid
                                                          : core_req_i.sid,
           st0_req.tid             = st0_rtab_pop_try_sel ? st0_rtab_pop_try_req.tid
                                                          : core_req_i.tid,
           st0_req.need_rsp        = st0_rtab_pop_try_sel ? st0_rtab_pop_try_req.need_rsp
                                                          : core_req_i.need_rsp,
           st0_req.phys_indexed    = st0_rtab_pop_try_sel ? 1'b1
                                                          : core_req_i.phys_indexed,
           st0_req.pma             = st0_rtab_pop_try_sel ? st0_rtab_pop_try_req.pma
                                                          : core_req_i.pma;

    //     Decode operation in stage 0
    assign st0_req_is_uncacheable  = ~cfg_enable_i | ( st0_req.phys_indexed
                                                     & st0_req.pma.uncacheable),
           st0_req_is_load         =         is_load(st0_req.op),
           st0_req_is_store        =        is_store(st0_req.op),
           st0_req_is_amo          =          is_amo(st0_req.op),
           st0_req_is_cmo_fence    =    is_cmo_fence(st0_req.op, st0_req.size),
           st0_req_is_cmo_inval    =    is_cmo_inval(st0_req.op, st0_req.size),
           st0_req_is_cmo_prefetch = is_cmo_prefetch(st0_req.op, st0_req.size);

    //     Decode operation in stage 1

    //         In case of replay or physically-indexed cache, the tag and PMA come
    //         from stage 0. Otherwise, this information come directly from the
    //         requester in stage 1
    assign st1_req_tag             = st1_req_q.phys_indexed ? st1_req_q.addr_tag : core_req_tag_i,
           st1_req_pma             = st1_req_q.phys_indexed ? st1_req_q.pma      : core_req_pma_i;

    assign st1_req.addr_offset     = st1_req_q.addr_offset,
           st1_req.addr_tag        = st1_req_rtab_q ? st1_req_q.addr_tag : st1_req_tag,
           st1_req.wdata           = st1_req_q.wdata,
           st1_req.op              = st1_req_q.op,
           st1_req.be              = st1_req_q.be,
           st1_req.size            = st1_req_q.size,
           st1_req.sid             = st1_req_q.sid,
           st1_req.tid             = st1_req_q.tid,
           st1_req.need_rsp        = st1_req_q.need_rsp,
           st1_req.phys_indexed    = st1_req_q.phys_indexed,
           st1_req.pma             = st1_req_rtab_q ? st1_req_q.pma : st1_req_pma;

    //         A requester can ask to abort a request it initiated on the
    //         previous cycle (stage 0). Useful in case of TLB miss for example
    assign st1_req_abort           = core_req_abort_i & ~st1_req.phys_indexed;

    assign st1_req_is_uncacheable  = ~cfg_enable_i | st1_req.pma.uncacheable,
           st1_req_is_load         =         is_load(st1_req.op),
           st1_req_is_store        =        is_store(st1_req.op),
           st1_req_is_amo          =          is_amo(st1_req.op),
           st1_req_is_amo_lr       =       is_amo_lr(st1_req.op),
           st1_req_is_amo_sc       =       is_amo_sc(st1_req.op),
           st1_req_is_amo_swap     =     is_amo_swap(st1_req.op),
           st1_req_is_amo_add      =      is_amo_add(st1_req.op),
           st1_req_is_amo_and      =      is_amo_and(st1_req.op),
           st1_req_is_amo_or       =       is_amo_or(st1_req.op),
           st1_req_is_amo_xor      =      is_amo_xor(st1_req.op),
           st1_req_is_amo_max      =      is_amo_max(st1_req.op),
           st1_req_is_amo_maxu     =     is_amo_maxu(st1_req.op),
           st1_req_is_amo_min      =      is_amo_min(st1_req.op),
           st1_req_is_amo_minu     =     is_amo_minu(st1_req.op),
           st1_req_is_cmo_inval    =    is_cmo_inval(st1_req.op, st1_req.size),
           st1_req_is_cmo_fence    =    is_cmo_fence(st1_req.op, st1_req.size),
           st1_req_is_cmo_prefetch = is_cmo_prefetch(st1_req.op, st1_req.size);
    //  }}}

    //  Refill arbiter: it arbitrates between normal requests (from the core,
    //  coprocessor, prefetch) and refill requests (from the miss handler).
    //
    //  TODO This arbiter could be replaced by a weighted-round-robin arbiter.
    //  This way we could distribute asymetrically the bandwidth to the core
    //  and the refill interfaces.
    //  {{{
    hpdcache_rrarb #(.N(2)) st0_arb_i
    (
        .clk_i,
        .rst_ni,
        .req_i                              (st0_arb_req),
        .gnt_o                              (st0_arb_req_grant),
        .ready_i                            (st0_arb_ready)
    );

    //      The arbiter can cycle the priority token when:
    //      - The granted request is consumed (req_grant &  req_valid & req_ready)
    //      - The granted request is aborted  (req_grant & ~req_valid)
    assign st0_arb_ready  = ((st0_arb_req_grant[0] &     st0_req_valid   &    st0_req_ready  ) |
                             (st0_arb_req_grant[1] &  refill_req_valid_i & refill_req_ready_o) |
                             (st0_arb_req_grant[0] &    ~st0_req_valid  ) |
                             (st0_arb_req_grant[1] & ~refill_req_valid_i));

    assign st0_arb_req[0] = st0_req_valid,
           st0_arb_req[1] = refill_req_valid_i;

    assign core_req_ready_o       = st0_req_ready & ~st0_rtab_pop_try_sel,
           st0_rtab_pop_try_ready = st0_req_ready &  st0_rtab_pop_try_sel;

    //      Trigger an event signal when the pipeline is stalled (new request is not consumed)
    assign evt_stall_o        = core_req_valid_i & ~core_req_ready_o;
    //  }}}

    //  Cache controller protocol engine
    //  {{{
    hpdcache_ctrl_pe hpdcache_ctrl_pe_i(
        .arb_st0_req_valid_i                (st0_req_valid & st0_arb_req_grant[0]),
        .arb_st0_req_ready_o                (st0_req_ready),
        .arb_refill_valid_i                 (refill_req_valid_i & st0_arb_req_grant[1]),
        .arb_refill_ready_o                 (refill_req_ready_o),
        .st0_req_is_uncacheable_i           (st0_req_is_uncacheable),
        .st0_req_need_rsp_i                 (st0_req.need_rsp),
        .st0_req_is_load_i                  (st0_req_is_load),
        .st0_req_is_store_i                 (st0_req_is_store),
        .st0_req_is_amo_i                   (st0_req_is_amo),
        .st0_req_is_cmo_fence_i             (st0_req_is_cmo_fence),
        .st0_req_is_cmo_inval_i             (st0_req_is_cmo_inval),
        .st0_req_is_cmo_prefetch_i          (st0_req_is_cmo_prefetch),
        .st0_req_mshr_check_o               (miss_mshr_check_o),
        .st0_req_cachedir_read_o            (st0_req_cachedir_read),
        .st0_req_cachedata_read_o           (st0_req_cachedata_read),

        .st1_req_valid_i                    (st1_req_valid_q),
        .st1_req_abort_i                    (st1_req_abort),
        .st1_req_rtab_i                     (st1_req_rtab_q),
        .st1_req_is_uncacheable_i           (st1_req_is_uncacheable),
        .st1_req_need_rsp_i                 (st1_req.need_rsp),
        .st1_req_is_load_i                  (st1_req_is_load),
        .st1_req_is_store_i                 (st1_req_is_store),
        .st1_req_is_amo_i                   (st1_req_is_amo),
        .st1_req_is_cmo_inval_i             (st1_req_is_cmo_inval),
        .st1_req_is_cmo_fence_i             (st1_req_is_cmo_fence),
        .st1_req_is_cmo_prefetch_i          (st1_req_is_cmo_prefetch),
        .st1_req_valid_o                    (st1_req_valid_d),
        .st1_rsp_valid_o                    (st1_rsp_valid),
        .st1_rsp_aborted_o                  (st1_rsp_aborted),
        .st1_req_cachedir_updt_lru_o        (st1_req_updt_lru),
        .st1_req_cachedata_write_o          (st1_req_cachedata_write),
        .st1_req_cachedata_write_enable_o   (st1_req_cachedata_write_enable),

        .st2_req_valid_i                    (st2_req_valid_q),
        .st2_req_is_prefetch_i              (st2_req_is_prefetch_q),
        .st2_req_valid_o                    (st2_req_valid_d),
        .st2_req_we_o                       (st2_req_we),
        .st2_req_is_prefetch_o              (st2_req_is_prefetch_d),
        .st2_req_mshr_alloc_o               (miss_mshr_alloc_o),
        .st2_req_mshr_alloc_cs_o            (miss_mshr_alloc_cs_o),

        .rtab_full_i                        (rtab_full),
        .rtab_req_valid_i                   (st0_rtab_pop_try_valid),
        .rtab_sel_o                         (st0_rtab_pop_try_sel),
        .rtab_check_o                       (st1_rtab_check),
        .rtab_check_hit_i                   (st1_rtab_check_hit),
        .st1_rtab_alloc_o                   (st1_rtab_alloc),
        .st1_rtab_alloc_and_link_o          (st1_rtab_alloc_and_link),
        .st1_rtab_commit_o                  (st1_rtab_pop_try_commit),
        .st1_rtab_rback_o                   (st1_rtab_pop_try_rback),
        .st1_rtab_mshr_hit_o                (st1_rtab_mshr_hit),
        .st1_rtab_mshr_full_o               (st1_rtab_mshr_full),
        .st1_rtab_mshr_ready_o              (st1_rtab_mshr_ready),
        .st1_rtab_wbuf_hit_o                (st1_rtab_wbuf_hit),
        .st1_rtab_wbuf_not_ready_o          (st1_rtab_wbuf_not_ready),

        .cachedir_hit_i                     (cachedir_hit_o),
        .cachedir_init_ready_i              (hpdcache_init_ready),

        .mshr_alloc_ready_i                 (miss_mshr_alloc_ready_i),
        .mshr_hit_i                         (miss_mshr_hit_i),
        .mshr_full_i                        (miss_mshr_alloc_full_i),

        .refill_busy_i,
        .refill_core_rsp_valid_i,

        .wbuf_write_valid_o                 (wbuf_write_o),
        .wbuf_write_ready_i,
        .wbuf_read_hit_i,
        .wbuf_write_uncacheable_o,
        .wbuf_read_flush_hit_o,

        .uc_busy_i,
        .uc_req_valid_o,
        .uc_core_rsp_ready_o,

        .cmo_busy_i,
        .cmo_req_valid_o,

        .evt_cache_write_miss_o,
        .evt_cache_read_miss_o,
        .evt_uncached_req_o,
        .evt_cmo_req_o,
        .evt_write_req_o,
        .evt_read_req_o,
        .evt_prefetch_req_o,
        .evt_req_on_hold_o,
        .evt_rtab_rollback_o,
        .evt_stall_refill_o
    );

    assign ctrl_empty_o = ~(st1_req_valid_q | st2_req_valid_q);
    //  }}}

    //  Replay table
    //  {{{
    hpdcache_rtab #(
        .rtab_entry_t                       (hpdcache_req_t)
    ) hpdcache_rtab_i(
        .clk_i,
        .rst_ni,

        .empty_o                            (rtab_empty_o),
        .full_o                             (rtab_full),

        .check_i                            (st1_rtab_check),
        .check_nline_i                      (st1_req_nline),
        .check_hit_o                        (st1_rtab_check_hit),

        .alloc_i                            (st1_rtab_alloc),
        .alloc_and_link_i                   (st1_rtab_alloc_and_link),
        .alloc_req_i                        (st1_req),
        .alloc_mshr_hit_i                   (st1_rtab_mshr_hit),
        .alloc_mshr_full_i                  (st1_rtab_mshr_full),
        .alloc_mshr_ready_i                 (st1_rtab_mshr_ready),
        .alloc_wbuf_hit_i                   (st1_rtab_wbuf_hit),
        .alloc_wbuf_not_ready_i             (st1_rtab_wbuf_not_ready),

        .pop_try_valid_o                    (st0_rtab_pop_try_valid),
        .pop_try_i                          (st0_rtab_pop_try_ready),
        .pop_try_req_o                      (st0_rtab_pop_try_req),
        .pop_try_ptr_o                      (st0_rtab_pop_try_ptr),

        .pop_commit_i                       (st1_rtab_pop_try_commit),
        .pop_commit_ptr_i                   (st1_rtab_pop_try_ptr_q),

        .pop_rback_i                        (st1_rtab_pop_try_rback),
        .pop_rback_ptr_i                    (st1_rtab_pop_try_ptr_q),
        .pop_rback_mshr_hit_i               (st1_rtab_mshr_hit),
        .pop_rback_mshr_full_i              (st1_rtab_mshr_full),
        .pop_rback_mshr_ready_i             (st1_rtab_mshr_ready),
        .pop_rback_wbuf_hit_i               (st1_rtab_wbuf_hit),
        .pop_rback_wbuf_not_ready_i         (st1_rtab_wbuf_not_ready),

        .wbuf_addr_o                        (wbuf_rtab_addr_o),
        .wbuf_is_read_o                     (wbuf_rtab_is_read_o),
        .wbuf_hit_open_i                    (wbuf_rtab_hit_open_i),
        .wbuf_hit_pend_i                    (wbuf_rtab_hit_pend_i),
        .wbuf_hit_sent_i                    (wbuf_rtab_hit_sent_i),
        .wbuf_not_ready_i                   (wbuf_rtab_not_ready_i),

        .miss_ready_i                       (miss_mshr_alloc_ready_i),

        .refill_i                           (refill_updt_rtab_i),
        .refill_nline_i,

        .cfg_single_entry_i                 (cfg_rtab_single_entry_i)
    );
    //  }}}

    //  Pipeline stage 1 registers
    //  {{{
    always_ff @(posedge clk_i)
    begin : st1_req_payload_ff
        if (st0_req_ready) begin
            st1_req_q <= st0_req;
        end
    end

    always_ff @(posedge clk_i or negedge rst_ni)
    begin : st1_req_valid_ff
        if (!rst_ni) begin
            st1_req_valid_q        <= 1'b0;
            st1_req_rtab_q         <= 1'b0;
            st1_rtab_pop_try_ptr_q <= '0;
        end else begin
            st1_req_valid_q <= st1_req_valid_d;
            if (st0_req_ready) begin
                st1_req_rtab_q <= st0_rtab_pop_try_sel;
                if (st0_rtab_pop_try_sel) begin
                    st1_rtab_pop_try_ptr_q <= st0_rtab_pop_try_ptr;
                end
            end
        end
    end
    //  }}}

    //  Pipeline stage 2 registers
    //  {{{
    always_ff @(posedge clk_i)
    begin : st2_req_payload_ff
        if (st2_req_we) begin
            st2_req_need_rsp_q <= st1_req.need_rsp;
            st2_req_addr_q     <= st1_req_addr;
            st2_req_sid_q      <= st1_req.sid;
            st2_req_tid_q      <= st1_req.tid;
        end
    end

    always_ff @(posedge clk_i or negedge rst_ni)
    begin : st2_req_valid_ff
        if (!rst_ni) begin
            st2_req_valid_q       <= 1'b0;
            st2_req_is_prefetch_q <= 1'b0;
        end else begin
            st2_req_valid_q       <= st2_req_valid_d;
            st2_req_is_prefetch_q <= st2_req_is_prefetch_d;
        end
    end
    //  }}}

    //  Controller for the HPDcache directory and data memory arrays
    //  {{{
    assign st0_req_set   = hpdcache_get_req_offset_set(st0_req.addr_offset),
           st0_req_word  = hpdcache_get_req_offset_word(st0_req.addr_offset),
           st1_req_set   = hpdcache_get_req_offset_set(st1_req.addr_offset),
           st1_req_word  = hpdcache_get_req_offset_word(st1_req.addr_offset),
           st1_req_addr  = {st1_req.addr_tag, st1_req.addr_offset},
           st1_req_nline = hpdcache_get_req_addr_nline(st1_req_addr),
           st2_req_word  = hpdcache_get_req_addr_word(st2_req_addr_q);

    hpdcache_memctrl hpdcache_memctrl_i (
        .clk_i,
        .rst_ni,

        .ready_o                       (hpdcache_init_ready),

        .dir_match_i                   (st0_req_cachedir_read),
        .dir_match_set_i               (st0_req_set),
        .dir_match_tag_i               (st1_req.addr_tag),
        .dir_update_lru_i              (st1_req_updt_lru),
        .dir_hit_way_o                 (st1_dir_hit),

        .dir_amo_match_i               (uc_dir_amo_match_i),
        .dir_amo_match_set_i           (uc_dir_amo_match_set_i),
        .dir_amo_match_tag_i           (uc_dir_amo_match_tag_i),
        .dir_amo_update_plru_i         (uc_dir_amo_update_plru_i),
        .dir_amo_hit_way_o             (uc_dir_amo_hit_way_o),

        .dir_refill_i                  (refill_write_dir_i),
        .dir_refill_set_i              (refill_set_i),
        .dir_refill_entry_i            (refill_dir_entry_i),
        .dir_refill_updt_plru_i        (refill_updt_plru_i),
        .dir_victim_way_o              (refill_victim_way_o),

        .dir_cmo_check_i               (cmo_dir_check_i),
        .dir_cmo_check_set_i           (cmo_dir_check_set_i),
        .dir_cmo_check_tag_i           (cmo_dir_check_tag_i),
        .dir_cmo_check_hit_way_o       (cmo_dir_check_hit_way_o),

        .dir_cmo_inval_i               (cmo_dir_inval_i),
        .dir_cmo_inval_set_i           (cmo_dir_inval_set_i),
        .dir_cmo_inval_way_i           (cmo_dir_inval_way_i),

        .data_req_read_i               (st0_req_cachedata_read),
        .data_req_read_set_i           (st0_req_set),
        .data_req_read_size_i          (st0_req.size),
        .data_req_read_word_i          (st0_req_word),
        .data_req_read_data_o          (st1_read_data),

        .data_req_write_i              (st1_req_cachedata_write),
        .data_req_write_enable_i       (st1_req_cachedata_write_enable),
        .data_req_write_set_i          (st1_req_set),
        .data_req_write_size_i         (st1_req.size),
        .data_req_write_word_i         (st1_req_word),
        .data_req_write_data_i         (st1_req.wdata),
        .data_req_write_be_i           (st1_req.be),

        .data_amo_write_i              (uc_data_amo_write_i),
        .data_amo_write_enable_i       (uc_data_amo_write_enable_i),
        .data_amo_write_set_i          (uc_data_amo_write_set_i),
        .data_amo_write_size_i         (uc_data_amo_write_size_i),
        .data_amo_write_word_i         (uc_data_amo_write_word_i),
        .data_amo_write_data_i         (uc_data_amo_write_data_i),
        .data_amo_write_be_i           (uc_data_amo_write_be_i),

        .data_refill_i                 (refill_write_data_i),
        .data_refill_way_i             (refill_victim_way_i),
        .data_refill_set_i             (refill_set_i),
        .data_refill_word_i            (refill_word_i),
        .data_refill_data_i            (refill_data_i)
    );

    assign cachedir_hit_o = |st1_dir_hit;
    //  }}}

    //  Write buffer outputs
    //  {{{
    assign wbuf_write_addr_o = st1_req_addr,
           wbuf_write_data_o = st1_req.wdata,
           wbuf_write_be_o   = st1_req.be,
           wbuf_flush_all_o  = cmo_wbuf_flush_all_i | uc_wbuf_flush_all_i | wbuf_flush_i;
    //  }}}

    //  Miss handler outputs
    //  {{{
    assign miss_mshr_check_set_o =
            st0_req.addr_offset[HPDCACHE_OFFSET_WIDTH +: HPDCACHE_MSHR_SET_WIDTH];
    assign miss_mshr_check_tag_o =
            st1_req_nline[HPDCACHE_MSHR_SET_WIDTH +: HPDCACHE_MSHR_TAG_WIDTH];

    assign miss_mshr_alloc_nline_o       = hpdcache_get_req_addr_nline(st2_req_addr_q),
           miss_mshr_alloc_tid_o         = st2_req_tid_q,
           miss_mshr_alloc_sid_o         = st2_req_sid_q,
           miss_mshr_alloc_word_o        = st2_req_word,
           miss_mshr_alloc_need_rsp_o    = st2_req_need_rsp_q,
           miss_mshr_alloc_is_prefetch_o = st2_req_is_prefetch_q;
    //  }}}

    //  Uncacheable request handler outputs
    //  {{{
    assign uc_lrsc_snoop_o           = st1_req_valid_q & st1_req_is_store,
           uc_lrsc_snoop_addr_o      = st1_req_addr,
           uc_lrsc_snoop_size_o      = st1_req.size,
           uc_req_addr_o             = st1_req_addr,
           uc_req_size_o             = st1_req.size,
           uc_req_data_o             = st1_req.wdata,
           uc_req_be_o               = st1_req.be,
           uc_req_uc_o               = st1_req_is_uncacheable,
           uc_req_sid_o              = st1_req.sid,
           uc_req_tid_o              = st1_req.tid,
           uc_req_need_rsp_o         = st1_req.need_rsp,
           uc_req_op_o.is_ld         = st1_req_is_load,
           uc_req_op_o.is_st         = st1_req_is_store,
           uc_req_op_o.is_amo_lr     = st1_req_is_amo_lr,
           uc_req_op_o.is_amo_sc     = st1_req_is_amo_sc,
           uc_req_op_o.is_amo_swap   = st1_req_is_amo_swap,
           uc_req_op_o.is_amo_add    = st1_req_is_amo_add,
           uc_req_op_o.is_amo_and    = st1_req_is_amo_and,
           uc_req_op_o.is_amo_or     = st1_req_is_amo_or,
           uc_req_op_o.is_amo_xor    = st1_req_is_amo_xor,
           uc_req_op_o.is_amo_max    = st1_req_is_amo_max,
           uc_req_op_o.is_amo_maxu   = st1_req_is_amo_maxu,
           uc_req_op_o.is_amo_min    = st1_req_is_amo_min,
           uc_req_op_o.is_amo_minu   = st1_req_is_amo_minu;
    //  }}}

    //  CMO request handler outputs
    //  {{{
    assign cmo_req_addr_o                 = st1_req_addr,
           cmo_req_wdata_o                = st1_req.wdata,
           cmo_req_op_o.is_fence          = st1_req_is_cmo_fence,
           cmo_req_op_o.is_inval_by_nline = st1_req_is_cmo_inval &
                                            is_cmo_inval_by_nline(st1_req.size),
           cmo_req_op_o.is_inval_by_set   = st1_req_is_cmo_inval &
                                            is_cmo_inval_by_set(st1_req.size),
           cmo_req_op_o.is_inval_all      = st1_req_is_cmo_inval &
                                            is_cmo_inval_all(st1_req.size);
    //  }}}

    //  Control of the response to the core
    //  {{{
    assign core_rsp_valid_o   = refill_core_rsp_valid_i                     |
                                (uc_core_rsp_valid_i & uc_core_rsp_ready_o) |
                                st1_rsp_valid,
           core_rsp_o.rdata   = (refill_core_rsp_valid_i ? refill_core_rsp_i.rdata :
                                (uc_core_rsp_valid_i     ? uc_core_rsp_i.rdata     :
                                st1_read_data)),
           core_rsp_o.sid     = (refill_core_rsp_valid_i ? refill_core_rsp_i.sid   :
                                (uc_core_rsp_valid_i     ? uc_core_rsp_i.sid       :
                                st1_req.sid)),
           core_rsp_o.tid     = (refill_core_rsp_valid_i ? refill_core_rsp_i.tid   :
                                (uc_core_rsp_valid_i     ? uc_core_rsp_i.tid       :
                                st1_req.tid)),
           core_rsp_o.error   = (refill_core_rsp_valid_i ? refill_core_rsp_i.error :
                                (uc_core_rsp_valid_i     ? uc_core_rsp_i.error     :
                                /* FIXME */1'b0)),
           core_rsp_o.aborted = st1_rsp_aborted;
    //  }}}

    //  Assertions
    //  pragma translate_off
    //  {{{
    assert property (@(posedge clk_i) disable iff (!rst_ni)
            $onehot0({core_req_ready_o, st0_rtab_pop_try_ready, refill_req_ready_o})) else
                    $error("ctrl: only one request can be served per cycle");
    //  }}}
    //  pragma translate_on
endmodule
