`default_nettype none

`define CACHE_SETS 8
`define CACHE_BLOCK_SIZE 'd16

`define WORD_POISON 32'hBADF00D
`define CACHE_PHYSICAL_TAG_POISON 22'h277BAD

typedef logic [5:0] CacheIndex_t;
typedef logic [3:0] CacheBlockOffset_t;
typedef logic [19:0] CacheVirtualTag_t;
typedef logic [21:0] CachePhysicalTag_t;
typedef logic [$clog2(`CACHE_SETS)-1:0] CacheSetNumber_t;
typedef logic [31:0] Word_t;

typedef enum {CACHE_READ, CACHE_WRITE, CACHE_DRAM_FILL} CacheRequestType_t;

`define CACHE_REQUEST_WIDTH 104

typedef struct packed {
    CacheIndex_t             index      ;
    CacheBlockOffset_t       blockOffset;
    // This field is only valid if requestType is a write
    CachePhysicalTag_t            tag        ;
    Word_t                   writeData     ;
    CacheRequestType_t       requestType;
    logic                    isValid    ;
    logic              [3:0] writeEnable;
    // The user must ensure that this set is safe to write to using the memory
    // locking system.
    CacheSetNumber_t writeSet;
} CacheRequest_t;

module CacheHelper (
    input logic [5:0] index,
    input logic [3:0] offset,
    output logic [`CACHE_REQUEST_WIDTH-1:0] flatRequest
);

    generate
        if($bits(CacheRequest_t) != `CACHE_REQUEST_WIDTH)
            BadTypeLayout foo();
    endgenerate

    function automatic CacheRequest_t readRequestManual(CacheIndex_t index, CacheBlockOffset_t offset);
        return '{
            index: index,
            blockOffset: offset,
            tag: `CACHE_PHYSICAL_TAG_POISON,
            writeData: `WORD_POISON,
            requestType: CACHE_READ,
            isValid: 1'b1,
            writeEnable: 0,
            // This is effectively 32-bits wide which is sufficient, but not technically correct. The "compiler" will truncate the bits to the width of 'writeSet'
            writeSet: '0
        };
    endfunction

    CacheRequest_t request;

    assign request = readRequestManual(index, offset);

    assign flatRequest = request;

endmodule
