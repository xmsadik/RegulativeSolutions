managed implementation in class zbp_etr_ddl_i_ledger_serials unique;
strict ( 2 );

define behavior for zetr_ddl_i_ledger_serials alias LedgerSerials
persistent table zetr_t_srnmr
lock master
authorization master ( instance )
//etag master <field_name>
{
  mapping for zetr_t_srnmr
    {
      CompanCode        = bukrs;
      XMLType           = xmlty;
      SerialPrefix      = serpr;
      Description       = descr;
      NumberRangeObject = nrobj;
      NumberRangeNumber = numrn;
    }
  create;
  update;
  delete;
  field ( readonly : update ) CompanCode, XMLType;
}