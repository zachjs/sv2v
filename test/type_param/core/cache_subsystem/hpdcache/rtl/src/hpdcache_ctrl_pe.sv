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
 *  Description   : HPDcache Control Protocol Engine
 *  History       :
 */
module hpdcache_ctrl_pe
    // Ports
    // {{{
(
    //   Refill arbiter
    //   {{{
    input  logic                   arb_st0_req_valid_i,
    output logic                   arb_st0_req_ready_o,
    input  logic                   arb_refill_valid_i,
    output logic                   arb_refill_ready_o,
    //   }}}

    //   Pipeline stage 0
    //   {{{
    input  logic                   st0_req_is_uncacheable_i,
    input  logic                   st0_req_need_rsp_i,
    input  logic                   st0_req_is_load_i,
    input  logic                   st0_req_is_store_i,
    input  logic                   st0_req_is_amo_i,
    input  logic                   st0_req_is_cmo_fence_i,
    input  logic                   st0_req_is_cmo_inval_i,
    input  logic                   st0_req_is_cmo_prefetch_i,
    output logic                   st0_req_mshr_check_o,
    output logic                   st0_req_cachedir_read_o,
    output logic                   st0_req_cachedata_read_o,
    //   }}}

    //   Pipeline stage 1
    //   {{{
    input  logic                   st1_req_valid_i,
    input  logic                   st1_req_abort_i,
    input  logic                   st1_req_rtab_i,
    input  logic                   st1_req_is_uncacheable_i,
    input  logic                   st1_req_need_rsp_i,
    input  logic                   st1_req_is_load_i,
    input  logic                   st1_req_is_store_i,
    input  logic                   st1_req_is_amo_i,
    input  logic                   st1_req_is_cmo_inval_i,
    input  logic                   st1_req_is_cmo_fence_i,
    input  logic                   st1_req_is_cmo_prefetch_i,
    output logic                   st1_req_valid_o,
    output logic                   st1_rsp_valid_o,
    output logic                   st1_rsp_aborted_o,
    output logic                   st1_req_cachedir_updt_lru_o,
    output logic                   st1_req_cachedata_write_o,
    output logic                   st1_req_cachedata_write_enable_o,
    //   }}}

    //   Pipeline stage 2
    //   {{{
    input  logic                   st2_req_valid_i,
    input  logic                   st2_req_is_prefetch_i,
    output logic                   st2_req_valid_o,
    output logic                   st2_req_we_o,
    output logic                   st2_req_is_prefetch_o,
    output logic                   st2_req_mshr_alloc_o,
    output logic                   st2_req_mshr_alloc_cs_o,
    //   }}}

    //   Replay
    //   {{{
    input  logic                   rtab_full_i,
    input  logic                   rtab_req_valid_i,
    output logic                   rtab_sel_o,
    output logic                   rtab_check_o,
    input  logic                   rtab_check_hit_i,
    output logic                   st1_rtab_alloc_o,
    output logic                   st1_rtab_alloc_and_link_o,
    output logic                   st1_rtab_commit_o,
    output logic                   st1_rtab_rback_o,
    output logic                   st1_rtab_mshr_hit_o,
    output logic                   st1_rtab_mshr_full_o,
    output logic                   st1_rtab_mshr_ready_o,
    output logic                   st1_rtab_wbuf_hit_o,
    output logic                   st1_rtab_wbuf_not_ready_o,
    //   }}}

    //   Cache directory
    //   {{{
    input  logic                   cachedir_hit_i,
    input  logic                   cachedir_init_ready_i,
    //   }}}

    //   Miss Status Holding Register (MSHR)
    //   {{{
    input  logic                   mshr_alloc_ready_i,
    input  logic                   mshr_hit_i,
    input  logic                   mshr_full_i,
    //   }}}

    //   Refill interface
    //   {{{
    input  logic                   refill_busy_i,
    input  logic                   refill_core_rsp_valid_i,
    //   }}}

    //   Write buffer
    //   {{{
    input  logic                   wbuf_write_ready_i,
    input  logic                   wbuf_read_hit_i,
    output logic                   wbuf_write_valid_o,
    output logic                   wbuf_write_uncacheable_o,
    output logic                   wbuf_read_flush_hit_o,
    //   }}}

    //   Uncacheable request handler
    //   {{{
    input  logic                   uc_busy_i,
    output logic                   uc_req_valid_o,
    output logic                   uc_core_rsp_ready_o,
    //   }}}

    //   Cache Management Operation (CMO)
    //   {{{
    input  logic                   cmo_busy_i,
    output logic                   cmo_req_valid_o,
    //   }}}

    //   Performance events
    //   {{{
    output logic                   evt_cache_write_miss_o,
    output logic                   evt_cache_read_miss_o,
    output logic                   evt_uncached_req_o,
    output logic                   evt_cmo_req_o,
    output logic                   evt_write_req_o,
    output logic                   evt_read_req_o,
    output logic                   evt_prefetch_req_o,
    output logic                   evt_req_on_hold_o,
    output logic                   evt_rtab_rollback_o,
    output logic                   evt_stall_refill_o
    //   }}}
);
    // }}}

    //  Definition of internal signals
    //  {{{
    logic  st1_fence;
    logic  st1_rtab_alloc, st1_rtab_alloc_and_link;
    //  }}}

    //  Global control signals
    //  {{{

    //  Determine if the new request is a "fence". Here, fence instructions are
    //  considered those that need to be executed in program order
    //  (irrespectively of addresses). This means that all memory operations
    //  arrived before the "fence" instruction need to be finished, and only
    //  then the "fence" instruction is executed. In the same manner, all
    //  instructions following the "fence" need to wait the completion of this
    //  last before being executed.
    assign st1_fence = st1_req_is_uncacheable_i |
                       st1_req_is_cmo_fence_i   |
                       st1_req_is_cmo_inval_i   |
                       st1_req_is_amo_i;
    //  }}}

    //  Arbitration of responses to the core
    //  {{{
    assign uc_core_rsp_ready_o = ~refill_core_rsp_valid_i;
    //  }}}

    //  Arbiter between core or replay request.
    //  {{{
    //      Take the replay request when:
    //      - The replay table is full.
    //      - The replay table has a ready request (request with all dependencies solved)
    //      - There is an outstanding CMO or uncached/AMO request
    //
    //      IMPORTANT: When the replay table is full, the cache cannot accept new core
    //      requests because this can introduce a dead-lock : If the core request needs to
    //      be put on hold, as there is no place the replay table, the pipeline needs to
    //      stall. If the pipeline is stalled, dependencies of on-hold requests cannot be
    //      solved, and the system is locked.
    assign rtab_sel_o = rtab_full_i                   |
                        rtab_req_valid_i              |
                        (st1_req_valid_i & st1_fence) |
                        cmo_busy_i                    |
                        uc_busy_i;
    //  }}}

    //  Replay logic
    //  {{{
    //      Replay table allocation
    assign st1_rtab_alloc_o          = st1_rtab_alloc          & ~st1_req_rtab_i,
           st1_rtab_alloc_and_link_o = st1_rtab_alloc_and_link,
           st1_rtab_rback_o          = st1_rtab_alloc          &  st1_req_rtab_i;

    //      Performance event
    assign evt_req_on_hold_o   = st1_rtab_alloc | st1_rtab_alloc_and_link,
           evt_rtab_rollback_o = st1_rtab_rback_o;
    //  }}}

    //  Data-cache control lines
    //  {{{
    always_comb
    begin : hpdcache_ctrl_comb
        automatic logic nop, st1_nop, st2_nop;

        uc_req_valid_o                      = 1'b0;

        cmo_req_valid_o                     = 1'b0;

        wbuf_write_valid_o                  = 1'b0;
        wbuf_read_flush_hit_o               = 1'b0;
        wbuf_write_uncacheable_o            = 1'b0; // unused

        arb_st0_req_ready_o                 = 1'b0;
        arb_refill_ready_o                  = 1'b0;

        st0_req_mshr_check_o                = 1'b0;
        st0_req_cachedir_read_o             = 1'b0;
        st0_req_cachedata_read_o            = 1'b0;

        st1_req_valid_o                     = st1_req_valid_i;
        st1_nop                             = 1'b0;
        st1_req_cachedata_write_o           = 1'b0;
        st1_req_cachedata_write_enable_o    = 1'b0;
        st1_req_cachedir_updt_lru_o         = 1'b0;
        st1_rsp_valid_o                     = 1'b0;
        st1_rsp_aborted_o                   = 1'b0;

        st2_req_valid_o                     = st2_req_valid_i;
        st2_req_we_o                        = 1'b0;
        st2_req_is_prefetch_o               = 1'b0;
        st2_req_mshr_alloc_cs_o             = 1'b0;
        st2_req_mshr_alloc_o                = 1'b0;
        st2_nop                             = 1'b0;

        nop                                 = 1'b0;

        rtab_check_o                        = 1'b0;
        st1_rtab_alloc                      = 1'b0;
        st1_rtab_alloc_and_link             = 1'b0;
        st1_rtab_commit_o                   = 1'b0;
        st1_rtab_mshr_hit_o                 = 1'b0;
        st1_rtab_mshr_full_o                = 1'b0;
        st1_rtab_mshr_ready_o               = 1'b0;
        st1_rtab_wbuf_hit_o                 = 1'b0;
        st1_rtab_wbuf_not_ready_o           = 1'b0;

        evt_cache_write_miss_o              = 1'b0;
        evt_cache_read_miss_o               = 1'b0;
        evt_uncached_req_o                  = 1'b0;
        evt_cmo_req_o                       = 1'b0;
        evt_write_req_o                     = 1'b0;
        evt_read_req_o                      = 1'b0;
        evt_prefetch_req_o                  = 1'b0;
        evt_stall_refill_o                  = 1'b0;

        //  Wait for the cache to be initialized
        //  {{{
        if (!cachedir_init_ready_i) begin
            //  initialization of the cache RAMs
        end
        //  }}}

        //  Refilling the cache
        //  {{{
        else if (refill_busy_i) begin
            //  miss handler has the control of the cache
            evt_stall_refill_o = arb_st0_req_valid_i;
        end
        //  }}}

        //  Normal pipeline operation
        //  {{{
        else begin
            //  Stage 2 request pending
            //  {{{
            if (st2_req_valid_i) begin
                st2_req_valid_o         = 1'b0;

                //  Allocate an entry in the MSHR
                st2_req_mshr_alloc_cs_o = 1'b1;
                st2_req_mshr_alloc_o    = 1'b1;

                //  Introduce a NOP in the next cycle to prevent a hazard on the MSHR
                st2_nop                 = 1'b1;

                //  Performance event
                evt_cache_read_miss_o   = ~st2_req_is_prefetch_i;
                evt_read_req_o          = ~st2_req_is_prefetch_i;
                evt_prefetch_req_o      =  st2_req_is_prefetch_i;
            end
            //  }}}

            //  Stage 1 request pending
            //  {{{
            if (st1_req_valid_i) begin
                //  Check if the request in stage 1 has a conflict with one of the
                //  request in the replay table.
                rtab_check_o = ~st1_req_rtab_i & ~st1_fence;

                //  Check if the current request is aborted. If so, respond to the
                //  core (when need_rsp is set) and set the aborted flag
                if (st1_req_abort_i && !st1_req_rtab_i) begin
                    st1_rsp_valid_o = st1_req_need_rsp_i;
                    st1_rsp_aborted_o = 1'b1;
                end

                //  Allocate a new entry in the replay table in case of conflict with
                //  an on-hold request
                else if (rtab_check_o && rtab_check_hit_i) begin
                    st1_rtab_alloc_and_link = 1'b1;

                    //  Do not consume a request in this cycle in stage 0
                    st1_nop = 1'b1;
                end

                //  CMO fence or invalidate
                //  {{{
                else if (st1_req_is_cmo_fence_i || st1_req_is_cmo_inval_i) begin
                    cmo_req_valid_o = 1'b1;
                    st1_nop         = 1'b1;

                    //  Performance event
                    evt_cmo_req_o = 1'b1;
                end
                //  }}}

                //  Uncacheable load, store or AMO request
                //  {{{
                else if (st1_req_is_uncacheable_i) begin
                    uc_req_valid_o = 1'b1;
                    st1_nop        = 1'b1;

                    //  Performance event
                    evt_uncached_req_o = 1'b1;
                end
                //  }}}

                //  Cacheable request
                //  {{{
                else begin
                    //  AMO cacheable request
                    //  {{{
                    if (st1_req_is_amo_i) begin
                        uc_req_valid_o = 1'b1;
                        st1_nop        = 1'b1;

                        //  Performance event
                        evt_uncached_req_o = 1'b1;
                    end
                    //  }}}

                    //  Load cacheable request
                    //  {{{
                    if (|{st1_req_is_load_i,
                          st1_req_is_cmo_prefetch_i})
                    begin
                        //  Cache miss
                        //  {{{
                        if (!cachedir_hit_i) begin
                            //  If there is a match in the write buffer, lets send the
                            //  entry right away
                            wbuf_read_flush_hit_o = 1'b1;

                            //  Do not consume a request in this cycle in stage 0
                            st1_nop = 1'b1;

                            //  Pending miss on the same line
                            if (mshr_hit_i) begin
                                //  Put the request in the replay table
                                st1_rtab_alloc = 1'b1;

                                st1_rtab_mshr_hit_o = 1'b1;
                            end

                            //  No available slot in the MSHR
                            else if (mshr_full_i) begin
                                //  Put the request in the replay table
                                st1_rtab_alloc = 1'b1;

                                st1_rtab_mshr_full_o = 1'b1;
                            end

                            //  Hit on an open entry of the write buffer:
                            //    wait for the entry to be acknowledged
                            else if (wbuf_read_hit_i) begin
                                //  Put the request in the replay table
                                st1_rtab_alloc = 1'b1;

                                st1_rtab_wbuf_hit_o = 1'b1;
                            end

                            //  Miss Handler is not ready to send
                            else if (!mshr_alloc_ready_i) begin
                                //  Put the request on hold if the MISS HANDLER is not
                                //  ready to send a new miss request. This is to prevent
                                //  a deadlock between the read request channel and the
                                //  read response channel.
                                //
                                //  The request channel may be stalled by targets if they
                                //  are not able to send a response (response is
                                //  prioritary). Therefore, we need to put the request on
                                //  hold to allow a possible refill read response to be
                                //  accomplished.
                                st1_rtab_alloc = 1'b1;

                                st1_rtab_mshr_ready_o = 1'b1;
                            end

                            //  Forward the request to the next stage to allocate the
                            //  entry in the MSHR and send the refill request
                            else begin
                                //  If the request comes from the replay table, free the
                                //  corresponding RTAB entry
                                st1_rtab_commit_o = st1_req_rtab_i;

                                st2_req_valid_o       = 1'b1;
                                st2_req_we_o          = 1'b1;
                                st2_req_is_prefetch_o = st1_req_is_cmo_prefetch_i;
                            end
                        end
                        //  }}}

                        //  Cache hit
                        //  {{{
                        else begin
                            //  If the request comes from the replay table, free the
                            //  corresponding RTAB entry
                            st1_rtab_commit_o = st1_req_rtab_i;

                            //  Add a NOP when replaying a request, and there is no available
                            //  request from the replay table.
                            st1_nop = st1_req_rtab_i & ~rtab_sel_o;

                            //  Update the PLRU bit for the accessed set
                            st1_req_cachedir_updt_lru_o = st1_req_is_load_i;

                            //  Respond to the core (if needed)
                            st1_rsp_valid_o = st1_req_need_rsp_i;

                            //  Performance event
                            evt_read_req_o     = ~st1_req_is_cmo_prefetch_i;
                            evt_prefetch_req_o =  st1_req_is_cmo_prefetch_i;
                        end
                        //  }}}
                    end
                    //  }}}

                    //  Store cacheable request
                    //  {{{
                    if (st1_req_is_store_i) begin
                        //  Write in the write buffer if there is no pending miss in the same line.
                        //
                        //  We assume here that the NoC that transports read and write transactions does
                        //  not guaranty the order between transactions on those channels.
                        //  Therefore, the cache must hold a write if there is a pending read on the
                        //  same address.
                        wbuf_write_valid_o = ~mshr_hit_i;

                        //  Add a NOP in the pipeline when:
                        //  - Structural hazard on the cache data if the st0 request is a load
                        //    operation.
                        //  - Replaying a request, the cache cannot accept a request from the
                        //    core the next cycle. It can however accept a new request from the
                        //    replay table
                        //
                        //  IMPORTANT: we could remove the NOP in the first scenario if the
                        //  controller checks for the hit of this write. However, this adds
                        //  a DIR_RAM -> DATA_RAM timing path.
                        st1_nop = (arb_st0_req_valid_i &  st0_req_is_load_i) |
                                  (st1_req_rtab_i      & ~rtab_sel_o);

                        //  Enable the data RAM in case of write. However, the actual write
                        //  depends on the hit signal from the cache directory.
                        //
                        //  IMPORTANT: this produces unnecessary power consumption in case of
                        //  write misses, but removes timing paths between the cache directory
                        //  RAM and the data RAM chip-select.
                        st1_req_cachedata_write_o = 1'b1;

                        //  Cache miss
                        if (!cachedir_hit_i) begin
                            //  Pending miss on the same line
                            if (mshr_hit_i) begin
                                //  Put the request in the replay table
                                st1_rtab_alloc = 1'b1;

                                st1_rtab_mshr_hit_o = 1'b1;

                                //  Do not consume a request in this cycle in stage 0
                                st1_nop = 1'b1;
                            end

                            //  No available entry in the write buffer (or conflict on pending entry)
                            else if (!wbuf_write_ready_i) begin
                                //  Put the request in the replay table
                                st1_rtab_alloc = 1'b1;

                                st1_rtab_wbuf_not_ready_o = 1'b1;

                                //  Do not consume a request in this cycle in stage 0
                                st1_nop = 1'b1;
                            end

                            else begin
                                //  If the request comes from the replay table, free the
                                //  corresponding RTAB entry
                                st1_rtab_commit_o = st1_req_rtab_i;

                                //  Respond to the core (if needed)
                                st1_rsp_valid_o = st1_req_need_rsp_i;

                                //  Performance event
                                evt_cache_write_miss_o = 1'b1;
                                evt_write_req_o        = 1'b1;
                            end
                        end

                        //  Cache hit
                        else begin
                            //  No available entry in the write buffer (or conflict on pending entry)
                            if (!wbuf_write_ready_i) begin
                                //  Put the request in the replay table
                                st1_rtab_alloc = 1'b1;

                                st1_rtab_wbuf_not_ready_o = 1'b1;

                                //  Do not consume a request in this cycle in stage 0
                                st1_nop = 1'b1;
                            end

                            //  The store can be performed in the write buffer and in the cache
                            else begin
                                //  If the request comes from the replay table, free the
                                //  corresponding RTAB entry
                                st1_rtab_commit_o = st1_req_rtab_i;

                                //  Respond to the core
                                st1_rsp_valid_o = st1_req_need_rsp_i;

                                //  Update the PLRU bit for the accessed set
                                st1_req_cachedir_updt_lru_o = 1'b1;

                                //  Write in the data RAM
                                st1_req_cachedata_write_enable_o = 1'b1;

                                //  Performance event
                                evt_write_req_o = 1'b1;
                            end
                        end
                    end
                    //  }}}
                end
                // }}}
            end
            //  }}}

            //  New request
            //  {{{
            nop = st1_nop | st2_nop;

            //      The cache controller accepts a core request when:
            //      -  The req-refill arbiter grants the request
            //      -  The pipeline is not being flushed
            arb_st0_req_ready_o = arb_st0_req_valid_i & ~nop;

            //      The cache controller accepts a refill when:
            //      -  The req-refill arbiter grants the refill
            //      -  The pipeline is empty
            arb_refill_ready_o = arb_refill_valid_i & ~(st1_req_valid_i | st2_req_valid_i);

            //      Forward the request to stage 1
            //      - There is a valid request in stage 0
            st1_req_valid_o = arb_st0_req_ready_o;

            //      New cacheable stage 0 request granted
            //      {{{
            //          IMPORTANT: here the RAM is enabled independently if the
            //          request needs to be put on-hold.
            //          This increases the power consumption in that cases, but
            //          removes the timing paths RAM-to-RAM between the cache
            //          directory and the data array.
            if (arb_st0_req_valid_i && !st0_req_is_uncacheable_i) begin
                st0_req_cachedata_read_o =
                          st0_req_is_load_i &
                        ~(st1_req_valid_i   & st1_req_is_store_i & ~st1_req_is_uncacheable_i);
                if (st0_req_is_load_i         |
                    st0_req_is_cmo_prefetch_i |
                    st0_req_is_store_i        |
                    st0_req_is_amo_i          )
                begin
                    st0_req_mshr_check_o    = 1'b1;
                    st0_req_cachedir_read_o = ~st0_req_is_amo_i;
                end
            end
            //      }}}
            //  }}}
        end
        //  }}} end of normal pipeline operation
    end
    //  }}}
endmodule
