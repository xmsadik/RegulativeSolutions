projection;
strict ( 2 );
use side effects;

define behavior for zetr_ddl_p_archive_parameters //alias <alias_name>
{
  use create;
  use update;
  use delete;

  use association _customParameters { create; }
  use association _invoiceSerials { create; }
  use association _xsltTemplates { create; }
  use association _invoiceRules { create; }
}

define behavior for zetr_ddl_p_archive_custom //alias <alias_name>
{
  use update;
  use delete;

  use association _eArchiveParameters;
}

define behavior for zetr_ddl_p_archive_rules //alias <alias_name>
{
  use update;
  use delete;

  use association _eArchiveParameters;
}

define behavior for zetr_ddl_p_archive_serials //alias <alias_name>
{
  use update;
  use delete;

  use association _eArchiveParameters;
  use association _numberStatus { }
  use action createNumbers;
}

define behavior for zetr_ddl_p_archive_numstat //alias <alias_name>
{
  use update;
  use delete;

  use association _archiveSerials;
  use association _eArchiveParameters;
}

define behavior for zetr_ddl_p_archive_xslttemp //alias <alias_name>
{
  use update;
  use delete;

  use association _eArchiveParameters;
}