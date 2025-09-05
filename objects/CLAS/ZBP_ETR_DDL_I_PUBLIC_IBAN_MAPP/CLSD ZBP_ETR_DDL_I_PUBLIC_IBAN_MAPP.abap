class-pool .
*"* class pool for class ZBP_ETR_DDL_I_PUBLIC_IBAN_MAPP

*"* local type definitions
include ZBP_ETR_DDL_I_PUBLIC_IBAN_MAPPccdef.

*"* class ZBP_ETR_DDL_I_PUBLIC_IBAN_MAPP definition
*"* public declarations
  include ZBP_ETR_DDL_I_PUBLIC_IBAN_MAPPcu.
*"* protected declarations
  include ZBP_ETR_DDL_I_PUBLIC_IBAN_MAPPco.
*"* private declarations
  include ZBP_ETR_DDL_I_PUBLIC_IBAN_MAPPci.
endclass. "ZBP_ETR_DDL_I_PUBLIC_IBAN_MAPP definition

*"* macro definitions
include ZBP_ETR_DDL_I_PUBLIC_IBAN_MAPPccmac.
*"* local class implementation
include ZBP_ETR_DDL_I_PUBLIC_IBAN_MAPPccimp.

*"* test class
include ZBP_ETR_DDL_I_PUBLIC_IBAN_MAPPccau.

class ZBP_ETR_DDL_I_PUBLIC_IBAN_MAPP implementation.
*"* method's implementations
  include methods.
endclass. "ZBP_ETR_DDL_I_PUBLIC_IBAN_MAPP implementation
