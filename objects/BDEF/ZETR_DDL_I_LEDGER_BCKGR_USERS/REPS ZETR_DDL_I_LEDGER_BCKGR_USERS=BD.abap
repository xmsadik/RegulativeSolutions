managed implementation in class zbp_etr_ddl_i_ledger_bckgr_use unique;
strict ( 2 );

define behavior for zetr_ddl_i_ledger_bckgr_users alias Users
persistent table zetr_t_arkis
lock master
authorization master ( instance )
//etag master <field_name>
{
  mapping for zetr_t_arkis
    {
      CompanyCode = bukrs;
      CreatedBy   = bname;
      UserName    = name1;
    }
  create;
  update;
  delete;
  field ( readonly : update ) CompanyCode, CreatedBy;
}