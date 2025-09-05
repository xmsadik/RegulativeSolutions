managed implementation in class zbp_etr_ddl_i_ledger_refman_do unique;
strict ( 2 );

define behavior for zetr_ddl_i_ledger_refman_docty alias DocTypes
persistent table zetr_t_rbzgb
lock master
authorization master ( instance )
//etag master <field_name>
{
  mapping for zetr_t_rbzgb
    {
      CompanyCode     = bukrs;
      TRADocumentType = gbtur;
    }
  create;
  update;
  delete;
  field ( readonly : update ) CompanyCode, TRADocumentType;
}