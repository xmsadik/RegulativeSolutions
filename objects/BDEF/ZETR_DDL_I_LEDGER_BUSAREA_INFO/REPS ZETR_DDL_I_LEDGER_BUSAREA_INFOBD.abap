managed implementation in class zbp_etr_ddl_i_ledger_busarea_i unique;
strict ( 2 );

define behavior for zetr_ddl_i_ledger_busarea_info alias BusinessAreas
persistent table zetr_t_srisa
lock master
authorization master ( instance )
//etag master <field_name>
{
  mapping for zetr_t_srisa
    {
      CompanyCode  = bukrs;
      BranchCode   = bcode;
      BusinessArea = gsber;
    }
  create;
  update;
  delete;
  field ( readonly:update ) CompanyCode, BranchCode, BusinessArea;
}