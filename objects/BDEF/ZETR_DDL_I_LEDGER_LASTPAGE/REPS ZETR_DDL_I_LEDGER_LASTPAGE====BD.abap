managed implementation in class zbp_etr_ddl_i_ledger_lastpage unique;
strict ( 1 );

define behavior for zetr_ddl_i_ledger_lastpage //alias <alias_name>
persistent table zetr_t_defcl
lock master
authorization master ( instance )
//etag master <field_name>
{
  create;
  //  update;
  delete;
  field ( readonly : update ) bukrs, gjahr, monat;
  static action create_ledger parameter ZETR_DDL_I_LASTPAGE_SELECTION;
  association _ledgerParts { create; }
}

define behavior for zetr_ddl_i_ledger_lastpage_prt //alias <alias_name>
persistent table zetr_t_oldef
lock dependent by _lastPageEntry
authorization dependent by _lastPageEntry
//etag master <field_name>
{
  //  update;
  delete;
  field ( readonly : update ) bukrs, bcode, gjahr, monat, datbi, partn;
  association _lastPageEntry;
}