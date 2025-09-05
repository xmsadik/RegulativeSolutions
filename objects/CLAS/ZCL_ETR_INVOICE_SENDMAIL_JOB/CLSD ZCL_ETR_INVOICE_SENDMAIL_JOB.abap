class-pool .
*"* class pool for class ZCL_ETR_INVOICE_SENDMAIL_JOB

*"* local type definitions
include ZCL_ETR_INVOICE_SENDMAIL_JOB==ccdef.

*"* class ZCL_ETR_INVOICE_SENDMAIL_JOB definition
*"* public declarations
  include ZCL_ETR_INVOICE_SENDMAIL_JOB==cu.
*"* protected declarations
  include ZCL_ETR_INVOICE_SENDMAIL_JOB==co.
*"* private declarations
  include ZCL_ETR_INVOICE_SENDMAIL_JOB==ci.
endclass. "ZCL_ETR_INVOICE_SENDMAIL_JOB definition

*"* macro definitions
include ZCL_ETR_INVOICE_SENDMAIL_JOB==ccmac.
*"* local class implementation
include ZCL_ETR_INVOICE_SENDMAIL_JOB==ccimp.

*"* test class
include ZCL_ETR_INVOICE_SENDMAIL_JOB==ccau.

class ZCL_ETR_INVOICE_SENDMAIL_JOB implementation.
*"* method's implementations
  include methods.
endclass. "ZCL_ETR_INVOICE_SENDMAIL_JOB implementation
