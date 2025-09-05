projection;
strict ( 2 );
use side effects;

define behavior for zetr_ddl_p_invoice_parameters //alias <alias_name>
{
  use create;
  use update;
  use delete;

  use association _customParameters { create; }
  use association _invoiceSerials { create; }
  use association _xsltTemplates { create; }
  use association _invoiceRules { create; }
}

define behavior for zetr_ddl_p_invoice_custom //alias <alias_name>
{
  use update;
  use delete;

  use association _eInvoiceParameters;
}

define behavior for zetr_ddl_p_invoice_serials //alias <alias_name>
{
  use update;
  use delete;

  use association _eInvoiceParameters;
  use association _numberStatus { }
  use action createNumbers;
}

define behavior for zetr_ddl_p_invoice_numstat //alias <alias_name>
{
  use update;
  use delete;

  use association _invoiceSerials;
  use association _eInvoiceParameters;
}

define behavior for zetr_ddl_p_invoice_xslttemp //alias <alias_name>
{
  use update;
  use delete;

  use association _eInvoiceParameters;
}

define behavior for zetr_ddl_p_invoice_rules //alias <alias_name>
{
  use update;
  use delete;

  use association _eInvoiceParameters;

}