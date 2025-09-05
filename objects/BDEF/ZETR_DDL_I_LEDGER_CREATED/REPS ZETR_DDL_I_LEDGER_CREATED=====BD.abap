unmanaged implementation in class zbp_etr_ddl_i_ledger_created unique;
strict ( 2 ); //Uncomment this line in order to enable strict mode 2. The strict mode has two variants (strict(1), strict(2)) and is prerequisite to be future proof regarding syntax and to be able to release your BO.

define behavior for zetr_ddl_i_ledger_created alias CreatedLedgers
//late numbering
lock master
authorization master ( instance )
//etag master <field_name>
{
  //  create;
  //  update;
  //  delete;
  field ( readonly ) bukrs, gjahr, monat;

  static action create_ledger parameter ZETR_DDL_I_FINPERIOD_SELECTION;

  association _LedgerParts { }
  association _LedgerTotals { }
}

define behavior for zetr_ddl_i_ledger_created_part alias LedgerParts
//late numbering
lock dependent by _CreatedLedger
authorization dependent by _CreatedLedger
//etag master <field_name>
{
    update;
  //  delete;
  field ( readonly ) bukrs, gjahr, monat, datbi, partn;
  association _CreatedLedger;
}

define behavior for zetr_ddl_i_ledger_created_tot alias LedgerTotals
//late numbering
lock dependent by _CreatedLedger
authorization dependent by _CreatedLedger
//etag master <field_name>
{
  //  update;
  //  delete;
  field ( readonly ) bukrs, gjahr, monat;
  association _CreatedLedger;
}