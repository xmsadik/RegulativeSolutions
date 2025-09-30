class-pool .
*"* class pool for class ZCL_ETR_EINVOICE_USERS

*"* local type definitions
include ZCL_ETR_EINVOICE_USERS========ccdef.

*"* class ZCL_ETR_EINVOICE_USERS definition
*"* public declarations
  include ZCL_ETR_EINVOICE_USERS========cu.
*"* protected declarations
  include ZCL_ETR_EINVOICE_USERS========co.
*"* private declarations
  include ZCL_ETR_EINVOICE_USERS========ci.
endclass. "ZCL_ETR_EINVOICE_USERS definition

*"* macro definitions
include ZCL_ETR_EINVOICE_USERS========ccmac.
*"* local class implementation
include ZCL_ETR_EINVOICE_USERS========ccimp.

*"* test class
include ZCL_ETR_EINVOICE_USERS========ccau.

class ZCL_ETR_EINVOICE_USERS implementation.
*"* method's implementations
  include methods.
endclass. "ZCL_ETR_EINVOICE_USERS implementation
