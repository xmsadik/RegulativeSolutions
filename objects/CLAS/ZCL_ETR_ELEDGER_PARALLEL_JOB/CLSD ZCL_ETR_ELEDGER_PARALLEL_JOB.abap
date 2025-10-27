class-pool .
*"* class pool for class ZCL_ETR_ELEDGER_PARALLEL_JOB

*"* local type definitions
include ZCL_ETR_ELEDGER_PARALLEL_JOB==ccdef.

*"* class ZCL_ETR_ELEDGER_PARALLEL_JOB definition
*"* public declarations
  include ZCL_ETR_ELEDGER_PARALLEL_JOB==cu.
*"* protected declarations
  include ZCL_ETR_ELEDGER_PARALLEL_JOB==co.
*"* private declarations
  include ZCL_ETR_ELEDGER_PARALLEL_JOB==ci.
endclass. "ZCL_ETR_ELEDGER_PARALLEL_JOB definition

*"* macro definitions
include ZCL_ETR_ELEDGER_PARALLEL_JOB==ccmac.
*"* local class implementation
include ZCL_ETR_ELEDGER_PARALLEL_JOB==ccimp.

*"* test class
include ZCL_ETR_ELEDGER_PARALLEL_JOB==ccau.

class ZCL_ETR_ELEDGER_PARALLEL_JOB implementation.
*"* method's implementations
  include methods.
endclass. "ZCL_ETR_ELEDGER_PARALLEL_JOB implementation
