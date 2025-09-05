managed implementation in class zbp_etr_ddl_i_ledger_symm_titl unique;
strict ( 2 );

define behavior for ZETR_DDL_I_LEDGER_SYMM_TITLE alias SYMMTitle
persistent table zetr_t_symmx
lock master
authorization master ( instance )
//etag master <field_name>
{
  mapping for zetr_t_symmx
    {
      Title       = mmtit;
      Description = mmtit_t;
    }
  create;
  update;
  delete;
  field ( readonly:update ) Title;
}