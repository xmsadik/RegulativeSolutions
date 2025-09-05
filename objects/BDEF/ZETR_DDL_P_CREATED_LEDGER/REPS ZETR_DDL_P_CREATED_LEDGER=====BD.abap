projection;
strict ( 1 );

define behavior for zetr_ddl_p_created_ledger alias CreatedLedger
{

  use action create_ledger;
  use action delete_ledger;
  use action send_ledger;
  use action resend_ledger;

  use association _ledgerParts;
  use association _ledgerTotals;
  use association _ledgerLogs;
}

define behavior for zetr_ddl_p_created_ledger_part alias LedgerParts
{
  use action LoadExcelContent;
  use association _createdLedger;
}

define behavior for zetr_ddl_p_created_ledger_tot alias LedgerTotals
{

  use association _createdLedger;
}

define behavior for zetr_ddl_p_created_ledger_logs alias LedgerLogs
{

  use association _createdLedger;
}