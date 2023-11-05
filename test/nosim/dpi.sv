module top;

export "DPI-C" task t;

/* From IEEE 1800-2017 Section 35.4 */

export "DPI-C" f_plus = function \f+ ; // "f+" exported as "f_plus"
export "DPI-C" function f; // "f" exported under its own name
import "DPI-C" init_1 = function void \init[1] (); // "init_1" is a linkage name
import "DPI-C" \begin = function void \init[2] (); // "begin" is a linkage name

/* From IEEE 1800-2017 Section 35.5.4 */

import "DPI-C" function void myInit();

// from standard math library
import "DPI-C" pure function real sin(real foo /* TODO support unnamed params */ );

// from standard C library: memory management
import "DPI-C" function chandle malloc(int size); // standard C function
import "DPI-C" function void free(chandle ptr); // standard C functionS

// abstract data structure: queue
import "DPI-C" function chandle newQueue(input string name_of_queue);

// Note the following import uses the same foreign function for
// implementation as the prior import, but has different SystemVerilog name
// and provides a default value for the argument.
import "DPI-C" newQueue=function chandle newAnonQueue(input string s/* =null TODO support default values */);
import "DPI-C" function chandle newElem(bit [15:0] foo /* TODO support unnamed params */ );
import "DPI-C" function void enqueue(chandle queue, chandle elem);
import "DPI-C" function chandle dequeue(chandle queue);

// miscellanea
import "DPI-C" function bit [15:0] getStimulus(input integer x);
import "DPI-C" context function void processTransaction(chandle elem,
output logic [64:1] arr [0:63]);
import "DPI-C" task checkResults(input string s, bit [511:0] packet);

endmodule
