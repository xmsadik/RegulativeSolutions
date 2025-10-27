class-pool .
*"* class pool for class ZCL_ETR_LEDGER_PARALLEL_TASK

*"* local type definitions
include ZCL_ETR_LEDGER_PARALLEL_TASK==ccdef.

*"* class ZCL_ETR_LEDGER_PARALLEL_TASK definition
*"* public declarations
  include ZCL_ETR_LEDGER_PARALLEL_TASK==cu.
*"* protected declarations
  include ZCL_ETR_LEDGER_PARALLEL_TASK==co.
*"* private declarations
  include ZCL_ETR_LEDGER_PARALLEL_TASK==ci.
endclass. "ZCL_ETR_LEDGER_PARALLEL_TASK definition

*"* macro definitions
include ZCL_ETR_LEDGER_PARALLEL_TASK==ccmac.
*"* local class implementation
include ZCL_ETR_LEDGER_PARALLEL_TASK==ccimp.

*"* test class
include ZCL_ETR_LEDGER_PARALLEL_TASK==ccau.

class ZCL_ETR_LEDGER_PARALLEL_TASK implementation.
*"* method's implementations
  include methods.
endclass. "ZCL_ETR_LEDGER_PARALLEL_TASK implementation
