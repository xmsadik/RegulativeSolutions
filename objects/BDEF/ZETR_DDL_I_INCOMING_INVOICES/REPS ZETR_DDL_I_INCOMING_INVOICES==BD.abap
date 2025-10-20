managed implementation in class zbp_etr_ddl_i_incoming_invoice unique;
strict ( 1 );

define behavior for zetr_ddl_i_incoming_invoices alias InvoiceList
persistent table zetr_t_icinv
//with unmanaged save
lock master
authorization master ( instance )
//etag master <field_name>
//late numbering
{

  mapping for zetr_t_icinv
    {
      DocumentUUID        = docui;
      CompanyCode         = bukrs;
      EnvelopeUUID        = envui;
      InvoiceUUID         = invui;
      InvoiceID           = invno;
      IntegratorUUID      = invii;
      QueryID             = invqi;
      TaxID               = taxid;
      Aliass              = aliass;
      DocumentDate        = bldat;
      ReceiveDate         = recdt;
      Amount              = wrbtr;
      TaxAmount           = fwste;
      WitholdingTaxAmount = wtxam;
      WitholdingTaxRate   = wtxrt;
      WitholdingTaxType   = wtxty;
      WitholdingTaxName   = wtxtx;
      Currency            = waers;
      ExchangeRate        = kursf;
      ProfileID           = prfid;
      InvoiceType         = invty;
      Printed             = prntd;
      Processed           = procs;
      Archived            = archv;
      LastNote            = lnote;
      LastNoteCreatedBy   = luser;
      ResponseStatus      = resst;
      TRAStatusCode       = radsc;
      StatusDetail        = staex;
      PurchasingGroup     = ekgrp;
      PurchaseOrders      = ebeln;
      AccountingDone      = accok;
      CustomField1        = cusfl1;
      CustomField2        = cusfl2;
      CustomField3        = cusfl3;
    }

  //  create;
  update;
  delete;
  field ( readonly : update ) DocumentUUID;
  field ( readonly )
  InvoiceID,
  InvoiceUUID,
  TaxID,
  TaxpayerTitle,
  DocumentDate,
  ReceiveDate,
  Amount,
  TaxAmount,
  WitholdingTaxAmount,
  WitholdingTaxRate,
  WitholdingTaxType,
  WitholdingTaxName,
  ExchangeRate,
  Currency,
  InvoiceType,
  ProfileID,
  ResponseStatus,
  LastNoteCreatedBy,
  LastNote,
  Archived,
  Printed,
  ApplicationResponse,
  AccountingDone,
  Processed;
  field ( features : instance ) PurchasingGroup;
  association _invoiceContents { create; }
  association _invoiceLogs { create; }
  association _invoiceItems { }

  action ( features : instance ) addNote parameter ZETR_DDL_I_NOTE_SELECTION result [1] $self;
  action ( features : instance ) changePrintStatus result [1] $self;
  action ( features : instance ) changeProcessStatus result [1] $self;
  action ( features : instance ) changeAccountingStatus parameter ZETR_DDL_I_ACCSTAT_SELECTION result [1] $self;
  action ( features : instance ) archiveInvoices result [1] $self;
  action ( features : instance ) statusUpdate result [1] $self;
  action showSummary result [1] $self;
  //  action ( features : instance ) downloadInvoices result [1] $self;
  action ( features : instance ) sendResponse parameter zetr_ddl_i_appresp_selection result [1] $self;
  action ( features : instance ) setAsRejected parameter zetr_ddl_i_reject_selection result [1] $self;
  action ( features : instance ) sendInformationMail parameter zetr_ddl_i_send_mail result [1] $self;
}

define behavior for zetr_ddl_i_incoming_invcont alias InvoiceContents
persistent table zetr_t_arcd
//with unmanaged save
lock dependent by _incomingInvoices
authorization dependent by _incomingInvoices
//etag master <field_name>
//late numbering
{
  mapping for zetr_t_arcd
    {
      DocumentUUID = docui;
      ContentType  = conty;
      DocumentType = docty;
      Content      = contn;
    }

  update;
  delete;
  field ( readonly ) DocumentUUID;
  field ( readonly : update ) ContentType, DocumentType;
  association _incomingInvoices;
}

define behavior for zetr_ddl_i_incoming_invlogs alias InvoiceLogs
persistent table zetr_t_logs
//with unmanaged save
lock dependent by _incomingInvoices
authorization dependent by _incomingInvoices
//etag master <field_name>
//late numbering
{
  mapping for zetr_t_logs
    {
      LogUUID      = logui;
      DocumentUUID = docui;
      CreatedBy    = uname;
      CreationDate = datum;
      CreationTime = uzeit;
      LogCode      = logcd;
      LogNote      = lnote;
    }
  update;
  delete;
  field ( readonly ) documentuuid, createdby, creationdate, creationtime, logcode, lognote;
  field ( readonly : update ) LogUUID;
  association _incomingInvoices;
}

define behavior for zetr_ddl_i_incoming_invitem alias InvoiceItems
persistent table zetr_t_icini
//with unmanaged save
lock dependent by _incomingInvoices
authorization dependent by _incomingInvoices
//etag master <field_name>
//late numbering
{
  mapping for zetr_t_icini
    {
      DocumentUUID                   = docui;
      LineNumber                     = linno;
      MaterialDescription            = mdesc;
      Description                    = descr;
      ModelName                      = mdlnm;
      Brand                          = brand;
      BuyerItemIdentification        = buyii;
      SellerItemIdentification       = selii;
      ManufacturerItemIdentification = manii;
      Price                          = netpr;
      DiscountRate                   = disrt;
      discountAmount                 = disam;
      Amount                         = wrbtr;
      TaxAmount                      = fwste;
      Currency                       = waers;
      Quantity                       = menge;
      UnitofMeasure                  = meins;
    }
  // update;
  delete;
  field ( readonly : update ) documentuuid, LineNumber;
  association _incomingInvoices;
}