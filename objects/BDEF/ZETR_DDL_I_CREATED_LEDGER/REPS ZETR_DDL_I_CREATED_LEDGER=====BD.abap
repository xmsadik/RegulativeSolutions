managed implementation in class zbp_etr_ddl_i_created_ledger unique;
strict ( 1 );

define behavior for zetr_ddl_i_created_ledger alias CreatedLedger
persistent table zetr_t_defcl
lock master
authorization master ( instance )
//etag master <field_name>
{
  //  create;
  //  update;
  //  delete;
  field ( readonly ) bukrs, gjahr, monat;
  association _ledgerParts { }
  association _ledgerTotals { }
  association _ledgerLogs { }

  static action create_ledger parameter ZETR_DDL_I_FINPERIOD_SELECTION;
  action delete_ledger result [1] $self;
  action send_ledger result [1] $self;
  action resend_ledger result [1] $self;
}

define behavior for zetr_ddl_i_created_ledger_part alias LedgerParts
persistent table zetr_t_oldef
lock dependent by _createdLedger
authorization dependent by _createdLedger
//etag master <field_name>
{
  //  update;
  //  delete;
  field ( readonly ) bukrs, bcode, gjahr, monat, datbi, partn;
  association _createdLedger;
  action LoadExcelContent result [1] $self;
}

define behavior for zetr_ddl_i_created_ledger_tot alias LedgerTotals
persistent table zetr_t_defcl
lock dependent by _createdLedger
authorization dependent by _createdLedger
//etag master <field_name>
{
  //  update;
  //  delete;
  field ( readonly ) bukrs, gjahr, monat;
  association _createdLedger;
}

define behavior for zetr_ddl_i_created_ledger_logs alias LedgerLogs
persistent table zetr_t_dfcll
lock dependent by _createdLedger
authorization dependent by _createdLedger
//etag master <field_name>
{
  //  update;
  //  delete;
  field ( readonly ) bukrs, bcode, gjahr, monat, lgsno;
  association _createdLedger;
}