projection;
strict ( 1 );

define behavior for zetr_ddl_p_ledger_lastpage //alias <alias_name>
{
//  use create;
  use delete;

  use action create_ledger;

  use association _ledgerParts { create; }
}

define behavior for zetr_ddl_p_ledger_lastpage_prt //alias <alias_name>
{
//  use delete;

  use association _lastPageEntry;
}