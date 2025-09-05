class-pool .
*"* class pool for class ZBP_ETR_DDL_I_COMPANY_MASTER_D

*"* local type definitions
include ZBP_ETR_DDL_I_COMPANY_MASTER_Dccdef.

*"* class ZBP_ETR_DDL_I_COMPANY_MASTER_D definition
*"* public declarations
  include ZBP_ETR_DDL_I_COMPANY_MASTER_Dcu.
*"* protected declarations
  include ZBP_ETR_DDL_I_COMPANY_MASTER_Dco.
*"* private declarations
  include ZBP_ETR_DDL_I_COMPANY_MASTER_Dci.
endclass. "ZBP_ETR_DDL_I_COMPANY_MASTER_D definition

*"* macro definitions
include ZBP_ETR_DDL_I_COMPANY_MASTER_Dccmac.
*"* local class implementation
include ZBP_ETR_DDL_I_COMPANY_MASTER_Dccimp.

*"* test class
include ZBP_ETR_DDL_I_COMPANY_MASTER_Dccau.

class ZBP_ETR_DDL_I_COMPANY_MASTER_D implementation.
*"* method's implementations
  include methods.
endclass. "ZBP_ETR_DDL_I_COMPANY_MASTER_D implementation
