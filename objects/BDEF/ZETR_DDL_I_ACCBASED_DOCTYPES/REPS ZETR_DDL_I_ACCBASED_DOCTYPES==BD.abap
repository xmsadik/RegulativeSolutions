managed implementation in class zbp_etr_ddl_i_accbased_doctype unique;
strict ( 2 );

define behavior for zetr_ddl_i_accbased_doctypes alias DocumentTypes
persistent table zetr_t_hbbtr
lock master
authorization master ( instance )
//etag master <field_name>
{
  mapping for zetr_t_hbbtr
    {
      CompanyCode     = bukrs;
      DocumentType    = blart;
      Account1        = hkon1;
      Account2        = hkon2;
      Account3        = hkon3;
      Account4        = hkon4;
      Description     = blart_t;
      TRADocumentType = gbtur;
      PaymentTerms    = oturu;
    }
  create;
  update;
  delete;
  field ( readonly : update ) CompanyCode, DocumentType, Account1, Account2, Account3, Account4;
}