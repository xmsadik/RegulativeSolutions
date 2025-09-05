managed implementation in class zbp_etr_ddl_i_ledger_cashdoc unique;
strict ( 2 );

define behavior for zetr_ddl_i_ledger_cashdoc alias CashDocuments
persistent table zetr_t_ksbel
lock master
authorization master ( instance )
//etag master <field_name>
{
  mapping for zetr_t_ksbel
    {
      CompanyCode       = bukrs;
      DocumentType      = blart;
      TransactionNumber = tisno;
      Description       = blart_t;
      TRADocumentType   = gbtur;
      PaymentTerms      = oturu;
    }
  create;
  update;
  delete;
  field ( readonly : update ) CompanyCode, DocumentType, TransactionNumber;
}