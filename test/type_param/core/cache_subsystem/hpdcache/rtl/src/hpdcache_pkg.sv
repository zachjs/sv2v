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
 *  Description   : Write-Through (WT), High-Throughput (HTPUT) HPDcache Package
 *  History       :
 */
package hpdcache_pkg;
    //  Definition of global constants for the HPDcache data and directory
    //  {{{

    //  HPDcache physical address width (bits)
    localparam int unsigned HPDCACHE_PA_WIDTH = hpdcache_params_pkg::PARAM_PA_WIDTH;

    //  HPDcache number of sets
    localparam int unsigned HPDCACHE_SETS = hpdcache_params_pkg::PARAM_SETS;

    //  HPDcache number of ways
    localparam int unsigned HPDCACHE_WAYS = hpdcache_params_pkg::PARAM_WAYS;

    //  HPDcache word width (bits)
    localparam int unsigned HPDCACHE_WORD_WIDTH = hpdcache_params_pkg::PARAM_WORD_WIDTH;

    //  HPDcache cache-line width (bits)
    localparam int unsigned HPDCACHE_CL_WORDS = hpdcache_params_pkg::PARAM_CL_WORDS;

    //  HPDcache number of words in the request data channels (request and response)
    localparam int unsigned HPDCACHE_REQ_WORDS = hpdcache_params_pkg::PARAM_REQ_WORDS;

    //  HPDcache request transaction ID width (bits)
    localparam int unsigned HPDCACHE_REQ_TRANS_ID_WIDTH = hpdcache_params_pkg::PARAM_REQ_TRANS_ID_WIDTH;

    //  HPDcache request source ID width (bits)
    localparam int unsigned HPDCACHE_REQ_SRC_ID_WIDTH = hpdcache_params_pkg::PARAM_REQ_SRC_ID_WIDTH;
    //  }}}

    //  Utility definitions
    //  {{{
    typedef logic unsigned [31:0] hpdcache_uint;
    typedef logic signed   [31:0] hpdcache_int;
    typedef logic unsigned [31:0] hpdcache_uint32;
    typedef logic signed   [31:0] hpdcache_int32;
    typedef logic unsigned [63:0] hpdcache_uint64;
    typedef logic signed   [63:0] hpdcache_int64;
    //  }}}

    //  Definition of constants and types for HPDcache directory memory
    //  {{{
    localparam int unsigned HPDCACHE_CL_WIDTH       = HPDCACHE_CL_WORDS*HPDCACHE_WORD_WIDTH;
    localparam int unsigned HPDCACHE_OFFSET_WIDTH   = $clog2(HPDCACHE_CL_WIDTH/8);
    localparam int unsigned HPDCACHE_NLINE_WIDTH    = HPDCACHE_PA_WIDTH - HPDCACHE_OFFSET_WIDTH;
    localparam int unsigned HPDCACHE_SET_WIDTH      = $clog2(HPDCACHE_SETS);
    localparam int unsigned HPDCACHE_TAG_WIDTH      = HPDCACHE_NLINE_WIDTH - HPDCACHE_SET_WIDTH;
    localparam int unsigned HPDCACHE_WORD_IDX_WIDTH = $clog2(HPDCACHE_CL_WORDS);

    typedef logic unsigned [  HPDCACHE_OFFSET_WIDTH-1:0] hpdcache_offset_t;
    typedef logic unsigned [   HPDCACHE_NLINE_WIDTH-1:0] hpdcache_nline_t;
    typedef logic unsigned [     HPDCACHE_SET_WIDTH-1:0] hpdcache_set_t;
    typedef logic unsigned [     HPDCACHE_TAG_WIDTH-1:0] hpdcache_tag_t;
    typedef logic unsigned [  $clog2(HPDCACHE_WAYS)-1:0] hpdcache_way_t;
    typedef logic unsigned [          HPDCACHE_WAYS-1:0] hpdcache_way_vector_t;
    typedef logic unsigned [HPDCACHE_WORD_IDX_WIDTH-1:0] hpdcache_word_t;

    typedef struct packed {
        hpdcache_tag_t  tag;
        logic [1:0]     reserved;
    } hpdcache_dir_entry_t;

    localparam int unsigned HPDCACHE_DIR_RAM_WIDTH       = $bits(hpdcache_dir_entry_t);
    localparam int unsigned HPDCACHE_DIR_RAM_DEPTH       = HPDCACHE_SETS;
    localparam int unsigned HPDCACHE_DIR_RAM_ADDR_WIDTH  = $clog2(HPDCACHE_DIR_RAM_DEPTH);

    typedef logic [HPDCACHE_DIR_RAM_ADDR_WIDTH-1:0] hpdcache_dir_addr_t;

    function automatic hpdcache_way_t hpdcache_way_vector_to_index(input hpdcache_way_vector_t way);
        for (int unsigned i = 0; i < HPDCACHE_WAYS; i++) begin
            if (way[i]) return hpdcache_way_t'(i);
        end
        return 0;
    endfunction

    //  }}}

    //  Definition of constants and types for HPDcache data memory
    //  {{{
    localparam int unsigned HPDCACHE_DATA_WAYS_PER_RAM_WORD =
        hpdcache_params_pkg::PARAM_DATA_WAYS_PER_RAM_WORD;

    localparam int unsigned HPDCACHE_DATA_SETS_PER_RAM = /* FIXME this parameter is currently ignored */
        hpdcache_params_pkg::PARAM_DATA_SETS_PER_RAM;

    //  HPDcache DATA RAM implements write byte enable
    localparam bit HPDCACHE_DATA_RAM_WBYTEENABLE =
        hpdcache_params_pkg::PARAM_DATA_RAM_WBYTEENABLE;

    //  Define the number of memory contiguous words that can be accessed
    //  simultaneously from the cache.
    //  -  This limits the maximum width for the data channel from requesters
    //  -  This impacts the refill latency
    localparam int unsigned HPDCACHE_ACCESS_WORDS = hpdcache_params_pkg::PARAM_ACCESS_WORDS;


    localparam int unsigned HPDCACHE_DATA_RAM_WIDTH        =
            HPDCACHE_DATA_WAYS_PER_RAM_WORD*HPDCACHE_WORD_WIDTH;
    localparam int unsigned HPDCACHE_DATA_RAM_Y_CUTS       = HPDCACHE_WAYS/HPDCACHE_DATA_WAYS_PER_RAM_WORD;
    localparam int unsigned HPDCACHE_DATA_RAM_X_CUTS       = HPDCACHE_ACCESS_WORDS;
    localparam int unsigned HPDCACHE_DATA_RAM_ACCESS_WIDTH = HPDCACHE_ACCESS_WORDS*HPDCACHE_WORD_WIDTH;
    localparam int unsigned HPDCACHE_DATA_RAM_ENTR_PER_SET = HPDCACHE_CL_WORDS/HPDCACHE_ACCESS_WORDS;
    localparam int unsigned HPDCACHE_DATA_RAM_DEPTH        = HPDCACHE_SETS*HPDCACHE_DATA_RAM_ENTR_PER_SET;
    localparam int unsigned HPDCACHE_DATA_RAM_ADDR_WIDTH   = $clog2(HPDCACHE_DATA_RAM_DEPTH);

    typedef logic [                     HPDCACHE_WORD_WIDTH-1:0]      hpdcache_data_word_t;
    typedef logic [                   HPDCACHE_WORD_WIDTH/8-1:0]      hpdcache_data_be_t;
    typedef logic [        $clog2(HPDCACHE_DATA_RAM_Y_CUTS)-1:0]      hpdcache_data_ram_row_idx_t;
    typedef logic [ $clog2(HPDCACHE_DATA_WAYS_PER_RAM_WORD)-1:0]      hpdcache_data_ram_way_idx_t;

    typedef logic [HPDCACHE_DATA_RAM_ADDR_WIDTH-1:0]                  hpdcache_data_ram_addr_t;
    typedef hpdcache_data_word_t[HPDCACHE_DATA_WAYS_PER_RAM_WORD-1:0] hpdcache_data_ram_data_t;
    typedef hpdcache_data_be_t  [HPDCACHE_DATA_WAYS_PER_RAM_WORD-1:0] hpdcache_data_ram_be_t;

    typedef hpdcache_data_ram_data_t
        [HPDCACHE_DATA_RAM_Y_CUTS-1:0]
        [HPDCACHE_DATA_RAM_X_CUTS-1:0]
        hpdcache_data_entry_t;

    typedef hpdcache_data_ram_be_t
        [HPDCACHE_DATA_RAM_Y_CUTS-1:0]
        [HPDCACHE_DATA_RAM_X_CUTS-1:0]
        hpdcache_data_be_entry_t;

    typedef logic
        [HPDCACHE_DATA_RAM_X_CUTS-1:0]
        hpdcache_data_row_enable_t;

    typedef hpdcache_data_row_enable_t
        [HPDCACHE_DATA_RAM_Y_CUTS-1:0]
        hpdcache_data_enable_t;

    typedef hpdcache_data_ram_addr_t
        [HPDCACHE_DATA_RAM_Y_CUTS-1:0]
        [HPDCACHE_DATA_RAM_X_CUTS-1:0]
        hpdcache_data_addr_t;
    //  }}}

    //  Definition of interface with miss handler
    //  {{{
    localparam int unsigned HPDCACHE_REFILL_DATA_WIDTH = HPDCACHE_DATA_RAM_ACCESS_WIDTH;

    //    Use feedthrough FIFOs from the refill handler to the core. This
    //    reduces the latency (by one cycle) but adds an additional timing path
    localparam bit HPDCACHE_REFILL_CORE_RSP_FEEDTHROUGH =
        hpdcache_params_pkg::PARAM_REFILL_CORE_RSP_FEEDTHROUGH;

    typedef hpdcache_data_word_t[HPDCACHE_ACCESS_WORDS-1:0] hpdcache_refill_data_t;
    typedef hpdcache_data_be_t  [HPDCACHE_ACCESS_WORDS-1:0] hpdcache_refill_be_t;
    //  }}}

    //  Definition of interface with requesters
    //  {{{
    localparam int unsigned HPDCACHE_REQ_DATA_WIDTH = HPDCACHE_REQ_WORDS*HPDCACHE_WORD_WIDTH;
    localparam int unsigned HPDCACHE_REQ_DATA_BYTES = HPDCACHE_REQ_DATA_WIDTH/8;
    localparam int unsigned HPDCACHE_REQ_WORD_INDEX_WIDTH = $clog2(HPDCACHE_REQ_WORDS);
    localparam int unsigned HPDCACHE_REQ_BYTE_OFFSET_WIDTH = $clog2(HPDCACHE_REQ_DATA_BYTES);
    localparam int unsigned HPDCACHE_REQ_OFFSET_WIDTH = HPDCACHE_PA_WIDTH - HPDCACHE_TAG_WIDTH;

    typedef logic                       [HPDCACHE_PA_WIDTH-1:0] hpdcache_req_addr_t;
    typedef logic               [HPDCACHE_REQ_OFFSET_WIDTH-1:0] hpdcache_req_offset_t;
    typedef hpdcache_data_word_t       [HPDCACHE_REQ_WORDS-1:0] hpdcache_req_data_t;
    typedef hpdcache_data_be_t         [HPDCACHE_REQ_WORDS-1:0] hpdcache_req_be_t;
    typedef logic                                         [2:0] hpdcache_req_size_t;
    typedef logic               [HPDCACHE_REQ_SRC_ID_WIDTH-1:0] hpdcache_req_sid_t;
    typedef logic             [HPDCACHE_REQ_TRANS_ID_WIDTH-1:0] hpdcache_req_tid_t;

    //      Definition of operation codes
    //      {{{
    typedef enum logic [3:0] {
        HPDCACHE_REQ_LOAD                 = 4'h0,
        HPDCACHE_REQ_STORE                = 4'h1,
        // RESERVED                     = 4'h2,
        // RESERVED                     = 4'h3,
        HPDCACHE_REQ_AMO_LR               = 4'h4,
        HPDCACHE_REQ_AMO_SC               = 4'h5,
        HPDCACHE_REQ_AMO_SWAP             = 4'h6,
        HPDCACHE_REQ_AMO_ADD              = 4'h7,
        HPDCACHE_REQ_AMO_AND              = 4'h8,
        HPDCACHE_REQ_AMO_OR               = 4'h9,
        HPDCACHE_REQ_AMO_XOR              = 4'ha,
        HPDCACHE_REQ_AMO_MAX              = 4'hb,
        HPDCACHE_REQ_AMO_MAXU             = 4'hc,
        HPDCACHE_REQ_AMO_MIN              = 4'hd,
        HPDCACHE_REQ_AMO_MINU             = 4'he,
        HPDCACHE_REQ_CMO                  = 4'hf
    } hpdcache_req_op_t;
    //      }}}

    //      Definition of CMO codes
    //      {{{
    typedef enum hpdcache_req_size_t {
        HPDCACHE_REQ_CMO_FENCE            = 3'h0,
        // RESERVED                     = 3'h1,
        HPDCACHE_REQ_CMO_INVAL_NLINE      = 3'h2,
        HPDCACHE_REQ_CMO_INVAL_SET_WAY    = 3'h3,
        HPDCACHE_REQ_CMO_INVAL_ALL        = 3'h4,
        HPDCACHE_REQ_CMO_PREFETCH         = 3'h5
    } hpdcache_req_cmo_t;
    //      }}}

    //      Definition of PMA flags
    //      {{{
    typedef struct packed
    {
        logic uncacheable;
        logic io; //  FIXME: for future use
    } hpdcache_pma_t;
    //      }}}

    //      Definition of interfaces
    //      {{{
    //          Request Interface
    typedef struct packed
    {
        hpdcache_req_offset_t addr_offset;
        hpdcache_req_data_t   wdata;
        hpdcache_req_op_t     op;
        hpdcache_req_be_t     be;
        hpdcache_req_size_t   size;
        hpdcache_req_sid_t    sid;
        hpdcache_req_tid_t    tid;
        logic                 need_rsp;

        //  only valid in case of physically indexed requests
        logic                 phys_indexed;
        hpdcache_tag_t        addr_tag;
        hpdcache_pma_t        pma;
    } hpdcache_req_t;

    //          Response Interface
    typedef struct packed
    {
        hpdcache_req_data_t   rdata;
        hpdcache_req_sid_t    sid;
        hpdcache_req_tid_t    tid;
        logic                 error;
        logic                 aborted;
    } hpdcache_rsp_t;
    //      }}}

    //      Definition of functions
    //      {{{
    function automatic logic is_load(input hpdcache_req_op_t op);
        case (op)
            HPDCACHE_REQ_LOAD: return 1'b1;
            default:           return 1'b0;
        endcase
    endfunction

    function automatic logic is_store(input hpdcache_req_op_t op);
        case (op)
            HPDCACHE_REQ_STORE: return 1'b1;
            default:            return 1'b0;
        endcase
    endfunction

    function automatic logic is_amo(input hpdcache_req_op_t op);
        case (op)
            HPDCACHE_REQ_AMO_LR,
            HPDCACHE_REQ_AMO_SC,
            HPDCACHE_REQ_AMO_SWAP,
            HPDCACHE_REQ_AMO_ADD,
            HPDCACHE_REQ_AMO_AND,
            HPDCACHE_REQ_AMO_OR,
            HPDCACHE_REQ_AMO_XOR,
            HPDCACHE_REQ_AMO_MAX,
            HPDCACHE_REQ_AMO_MAXU,
            HPDCACHE_REQ_AMO_MIN,
            HPDCACHE_REQ_AMO_MINU:
                return 1'b1;
            default:
                return 1'b0;
        endcase
    endfunction

    function automatic logic is_amo_lr(input hpdcache_req_op_t op);
        case (op)
            HPDCACHE_REQ_AMO_LR: return 1'b1;
            default:             return 1'b0;
        endcase
    endfunction

    function automatic logic is_amo_sc(input hpdcache_req_op_t op);
        case (op)
            HPDCACHE_REQ_AMO_SC: return 1'b1;
            default:             return 1'b0;
        endcase
    endfunction

    function automatic logic is_amo_swap(input hpdcache_req_op_t op);
        case (op)
            HPDCACHE_REQ_AMO_SWAP: return 1'b1;
            default:               return 1'b0;
        endcase
    endfunction

    function automatic logic is_amo_add(input hpdcache_req_op_t op);
        case (op)
            HPDCACHE_REQ_AMO_ADD: return 1'b1;
            default:              return 1'b0;
        endcase
    endfunction

    function automatic logic is_amo_and(input hpdcache_req_op_t op);
        case (op)
            HPDCACHE_REQ_AMO_AND: return 1'b1;
            default:              return 1'b0;
        endcase
    endfunction

    function automatic logic is_amo_or(input hpdcache_req_op_t op);
        case (op)
            HPDCACHE_REQ_AMO_OR:  return 1'b1;
            default:              return 1'b0;
        endcase
    endfunction

    function automatic logic is_amo_xor(input hpdcache_req_op_t op);
        case (op)
            HPDCACHE_REQ_AMO_XOR: return 1'b1;
            default:              return 1'b0;
        endcase
    endfunction

    function automatic logic is_amo_max(input hpdcache_req_op_t op);
        case (op)
            HPDCACHE_REQ_AMO_MAX: return 1'b1;
            default:              return 1'b0;
        endcase
    endfunction

    function automatic logic is_amo_maxu(input hpdcache_req_op_t op);
        case (op)
            HPDCACHE_REQ_AMO_MAXU: return 1'b1;
            default:               return 1'b0;
        endcase
    endfunction

    function automatic logic is_amo_min(input hpdcache_req_op_t op);
        case (op)
            HPDCACHE_REQ_AMO_MIN: return 1'b1;
            default:              return 1'b0;
        endcase
    endfunction

    function automatic logic is_amo_minu(input hpdcache_req_op_t op);
        case (op)
            HPDCACHE_REQ_AMO_MINU: return 1'b1;
            default:               return 1'b0;
        endcase
    endfunction

    function automatic logic is_cmo_inval(
            input hpdcache_req_op_t op,
            input hpdcache_req_size_t sz);
        case (op)
            HPDCACHE_REQ_CMO:
                case (sz)
                  HPDCACHE_REQ_CMO_INVAL_NLINE,
                  HPDCACHE_REQ_CMO_INVAL_SET_WAY,
                  HPDCACHE_REQ_CMO_INVAL_ALL: begin
                    return 1'b1;
                  end
                  default: begin
                    return 1'b0;
                  end
                endcase
            default: begin
              return 1'b0;
            end
        endcase
    endfunction

    function automatic logic is_cmo_inval_by_nline(input hpdcache_req_size_t sz);
        return (sz == HPDCACHE_REQ_CMO_INVAL_NLINE);
    endfunction

    function automatic logic is_cmo_inval_by_set(input hpdcache_req_size_t sz);
        return (sz == HPDCACHE_REQ_CMO_INVAL_SET_WAY);
    endfunction

    function automatic logic is_cmo_inval_all(input hpdcache_req_size_t sz);
        return (sz == HPDCACHE_REQ_CMO_INVAL_ALL);
    endfunction

    function automatic logic is_cmo_fence(
            input hpdcache_req_op_t op,
            input hpdcache_req_size_t sz);
        case (op)
            HPDCACHE_REQ_CMO: begin
                return (sz == HPDCACHE_REQ_CMO_FENCE);
            end
            default: begin
                return 1'b0;
            end
        endcase
    endfunction

    function automatic logic is_cmo_prefetch(
            input hpdcache_req_op_t op,
            input hpdcache_req_size_t sz);
        case (op)
            HPDCACHE_REQ_CMO: begin
                return (sz == HPDCACHE_REQ_CMO_PREFETCH);
            end
            default: begin
                return 1'b0;
            end
        endcase
    endfunction

    function automatic hpdcache_tag_t hpdcache_get_req_addr_tag(input hpdcache_req_addr_t addr);
        return addr[(HPDCACHE_OFFSET_WIDTH + HPDCACHE_SET_WIDTH) +: HPDCACHE_TAG_WIDTH];
    endfunction

    function automatic hpdcache_set_t hpdcache_get_req_addr_set(input hpdcache_req_addr_t addr);
        return addr[HPDCACHE_OFFSET_WIDTH +: HPDCACHE_SET_WIDTH];
    endfunction

    function automatic hpdcache_word_t hpdcache_get_req_addr_word(input hpdcache_req_addr_t addr);
        return addr[$clog2(HPDCACHE_WORD_WIDTH/8) +: HPDCACHE_WORD_IDX_WIDTH];
    endfunction

    function automatic hpdcache_offset_t hpdcache_get_req_addr_offset(input hpdcache_req_addr_t addr);
        return addr[0 +: HPDCACHE_OFFSET_WIDTH];
    endfunction

    function automatic hpdcache_nline_t hpdcache_get_req_addr_nline(input hpdcache_req_addr_t addr);
        return addr[HPDCACHE_OFFSET_WIDTH +: HPDCACHE_NLINE_WIDTH];
    endfunction

    function automatic hpdcache_set_t hpdcache_get_req_offset_set(input hpdcache_req_offset_t offset);
        return offset[HPDCACHE_OFFSET_WIDTH +: HPDCACHE_SET_WIDTH];
    endfunction

    function automatic hpdcache_word_t hpdcache_get_req_offset_word(input hpdcache_req_offset_t offset);
        return offset[$clog2(HPDCACHE_WORD_WIDTH/8) +: HPDCACHE_WORD_IDX_WIDTH];
    endfunction

    //      }}}
    //  }}}

    //  Definition of constants and types for the Miss Status Holding Register (MSHR)
    //  {{{

    //  HPDcache MSHR number of sets
    localparam int unsigned HPDCACHE_MSHR_SETS =
        hpdcache_params_pkg::PARAM_MSHR_SETS;

    //  HPDcache MSHR number of ways
    localparam int unsigned HPDCACHE_MSHR_WAYS =
        hpdcache_params_pkg::PARAM_MSHR_WAYS;

    //  HPDcache MSHR number of ways in the same SRAM word
    localparam int unsigned HPDCACHE_MSHR_WAYS_PER_RAM_WORD =
        hpdcache_params_pkg::PARAM_MSHR_WAYS_PER_RAM_WORD; /* FIXME this parameter is currently ignored */

    //  HPDcache MSHR number of sets in the same SRAM
    localparam int unsigned HPDCACHE_MSHR_SETS_PER_RAM =
        hpdcache_params_pkg::PARAM_MSHR_SETS_PER_RAM; /* FIXME this parameter is currently ignored */

    //  HPDcache MSHR implements write byte enable
    localparam bit HPDCACHE_MSHR_RAM_WBYTEENABLE =
        hpdcache_params_pkg::PARAM_MSHR_RAM_WBYTEENABLE;
    localparam bit HPDCACHE_MSHR_USE_REGBANK =
        hpdcache_params_pkg::PARAM_MSHR_USE_REGBANK;

    localparam int unsigned HPDCACHE_MSHR_SET_WIDTH = $clog2(HPDCACHE_MSHR_SETS);
    localparam int unsigned HPDCACHE_MSHR_WAY_WIDTH = $clog2(HPDCACHE_MSHR_WAYS);
    localparam int unsigned HPDCACHE_MSHR_TAG_WIDTH = HPDCACHE_NLINE_WIDTH - HPDCACHE_MSHR_SET_WIDTH;

    typedef logic unsigned [HPDCACHE_MSHR_SET_WIDTH-1:0] mshr_set_t;
    typedef logic unsigned [HPDCACHE_MSHR_TAG_WIDTH-1:0] mshr_tag_t;
    typedef logic unsigned [HPDCACHE_MSHR_WAY_WIDTH-1:0] mshr_way_t;
    //  }}}

    //  Definition of interface with memory
    //  {{{
    typedef logic [7:0]                           hpdcache_mem_len_t;
    typedef logic [2:0]                           hpdcache_mem_size_t;

    typedef enum logic [1:0] {
        HPDCACHE_MEM_RESP_OK  = 2'b00,
        HPDCACHE_MEM_RESP_NOK = 2'b01
    } hpdcache_mem_error_e;

    typedef enum logic [1:0] {
        HPDCACHE_MEM_READ     = 2'b00,
        HPDCACHE_MEM_WRITE    = 2'b01,
        HPDCACHE_MEM_ATOMIC   = 2'b10
        //  Reserved        = 2'b11 - TODO: CMO ?
    } hpdcache_mem_command_e;

    typedef enum logic [3:0] {
        HPDCACHE_MEM_ATOMIC_ADD  = 4'b0000,
        HPDCACHE_MEM_ATOMIC_CLR  = 4'b0001,
        HPDCACHE_MEM_ATOMIC_SET  = 4'b0010,
        HPDCACHE_MEM_ATOMIC_EOR  = 4'b0011,
        HPDCACHE_MEM_ATOMIC_SMAX = 4'b0100,
        HPDCACHE_MEM_ATOMIC_SMIN = 4'b0101,
        HPDCACHE_MEM_ATOMIC_UMAX = 4'b0110,
        HPDCACHE_MEM_ATOMIC_UMIN = 4'b0111,
        HPDCACHE_MEM_ATOMIC_SWAP = 4'b1000,
        //  Reserved           = 4'b1001,
        //  Reserved           = 4'b1010,
        //  Reserved           = 4'b1011,
        HPDCACHE_MEM_ATOMIC_LDEX = 4'b1100,
        HPDCACHE_MEM_ATOMIC_STEX = 4'b1101
        //  Reserved           = 4'b1110,
        //  Reserved           = 4'b1111
    } hpdcache_mem_atomic_e;

    function automatic hpdcache_mem_size_t get_hpdcache_mem_size(int unsigned bytes);
        if      (bytes ==   0) return 0;
        else if (bytes <=   2) return 1;
        else if (bytes <=   4) return 2;
        else if (bytes <=   8) return 3;
        else if (bytes <=  16) return 4;
        else if (bytes <=  32) return 5;
        else if (bytes <=  64) return 6;
        else if (bytes <= 128) return 7;
        // pragma translate_off
        else    $error("hpdcache: unsupported number of bytes");
        // pragma translate_on
    endfunction
    //  }}}

    //  Definition of constants and types for the Write Buffer (WBUF)
    //  {{{
    localparam int unsigned HPDCACHE_WBUF_DIR_ENTRIES =
        hpdcache_params_pkg::PARAM_WBUF_DIR_ENTRIES;

    localparam int unsigned HPDCACHE_WBUF_DATA_ENTRIES =
        hpdcache_params_pkg::PARAM_WBUF_DATA_ENTRIES;

    localparam int unsigned HPDCACHE_WBUF_WORDS =
        hpdcache_params_pkg::PARAM_WBUF_WORDS;

    localparam int unsigned HPDCACHE_WBUF_TIMECNT_WIDTH =
        hpdcache_params_pkg::PARAM_WBUF_TIMECNT_WIDTH;

    //    Use feedthrough FIFOs from the write-buffer to the NoC. This reduces
    //    the latency (by one cycle) but adds an additional timing path
    localparam bit HPDCACHE_WBUF_SEND_FEEDTHROUGH =
        hpdcache_params_pkg::PARAM_WBUF_SEND_FEEDTHROUGH;

    localparam int unsigned HPDCACHE_WBUF_DATA_WIDTH     = HPDCACHE_REQ_DATA_WIDTH*
                                                           HPDCACHE_WBUF_WORDS;
    localparam int unsigned HPDCACHE_WBUF_DATA_PTR_WIDTH = $clog2(HPDCACHE_WBUF_DATA_ENTRIES);
    localparam int unsigned HPDCACHE_WBUF_DIR_PTR_WIDTH  = $clog2(HPDCACHE_WBUF_DIR_ENTRIES);

    typedef hpdcache_req_addr_t                                 wbuf_addr_t;
    typedef hpdcache_nline_t                                    wbuf_match_t;
    typedef hpdcache_req_data_t                                 wbuf_data_t;
    typedef hpdcache_req_be_t                                   wbuf_be_t;
    typedef wbuf_data_t[HPDCACHE_WBUF_WORDS-1:0]                wbuf_data_buf_t;
    typedef wbuf_be_t  [HPDCACHE_WBUF_WORDS-1:0]                wbuf_be_buf_t;
    typedef logic unsigned   [ HPDCACHE_WBUF_TIMECNT_WIDTH-1:0] wbuf_timecnt_t;
    typedef logic unsigned   [ HPDCACHE_WBUF_DIR_PTR_WIDTH-1:0] wbuf_dir_ptr_t;
    typedef logic unsigned   [HPDCACHE_WBUF_DATA_PTR_WIDTH-1:0] wbuf_data_ptr_t;
    //  }}}

    //  Definition of constants and types for the Replay Table (RTAB)
    //  {{{
    localparam int HPDCACHE_RTAB_ENTRIES = hpdcache_params_pkg::PARAM_RTAB_ENTRIES;

    typedef logic [$clog2(HPDCACHE_RTAB_ENTRIES)-1:0] rtab_ptr_t;
    //  }}}

    //  Definition of constants and types for the uncacheable request handler (UC)
    //  {{{
    typedef struct packed {
        logic is_ld;
        logic is_st;
        logic is_amo_lr;
        logic is_amo_sc;
        logic is_amo_swap;
        logic is_amo_add;
        logic is_amo_and;
        logic is_amo_or;
        logic is_amo_xor;
        logic is_amo_max;
        logic is_amo_maxu;
        logic is_amo_min;
        logic is_amo_minu;
    } hpdcache_uc_op_t;
    //  }}}

    //  Definition of constants and types for the CMO request handler (CMOH)
    //  {{{
    typedef struct packed {
        logic is_inval_by_nline;
        logic is_inval_by_set;
        logic is_inval_all;
        logic is_fence;
    } hpdcache_cmoh_op_t;
    //  }}}
endpackage
