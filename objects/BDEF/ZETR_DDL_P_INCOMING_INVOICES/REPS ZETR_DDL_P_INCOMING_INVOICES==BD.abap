projection;
strict ( 1 );

define behavior for zetr_ddl_p_incoming_invoices alias InvoiceList
{
  use update;
  use delete;
  //  use create;

  use action archiveInvoices;
  use action statusUpdate;
  //  use action downloadInvoices;
  use action sendResponse;
  use action setAsRejected;
  use action addNote;
  use action changePrintStatus;
  use action changeProcessStatus;
  use action changeAccountingStatus;
  use action showSummary;
  use action sendInformationMail;

  use association _invoiceContents;
  use association _invoiceLogs;
  use association _invoiceItems;
}

define behavior for zetr_ddl_p_incoming_invcont alias InvoiceContents
{
//  use update;
  use association _incomingInvoices;
}

define behavior for zetr_ddl_p_incoming_invlogs alias InvoiceLogs
{
//  use update;
  use association _incomingInvoices;
}

define behavior for zetr_ddl_p_incoming_invitem alias InvoiceItems
{
  //  use update;
  use association _incomingInvoices;
}