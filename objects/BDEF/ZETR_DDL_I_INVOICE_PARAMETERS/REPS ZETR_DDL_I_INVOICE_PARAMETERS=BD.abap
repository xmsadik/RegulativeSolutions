managed implementation in class zbp_etr_ddl_i_invoice_paramete unique;
strict ( 2 );

define behavior for zetr_ddl_i_invoice_parameters //alias <alias_name>
persistent table zetr_t_eipar
lock master
authorization master ( instance )
//etag master <field_name>
{
  mapping for zetr_t_eipar
    {
      CompanyCode       = bukrs;
      ValidFrom         = datab;
      ValidTo           = datbi;
      Integrator        = intid;
      ProfileID         = prfid;
      WSEndpoint        = wsend;
      WSEndpointAlt     = wsena;
      WSUser            = wsusr;
      WSPassword        = wspwd;
      GenerateSerial    = genid;
      TaxfreeAgent      = tfagn;
      Barcode           = barcode;
      PKAlias           = pk_alias;
      GBAlias           = gb_alias;
      InternalNumbering = intnum;
      AutoSendMail      = automail;
    }
  create;
  update;
  delete;
  field ( readonly : update ) CompanyCode;
  association _customParameters { create; }
  association _invoiceSerials { create; }
  association _xsltTemplates { create; }
  association _invoiceRules { create; }
}

define behavior for zetr_ddl_i_invoice_custom //alias <alias_name>
persistent table zetr_t_eicus
lock dependent by _eInvoiceParameters
authorization dependent by _eInvoiceParameters
//etag master <field_name>
{
  mapping for zetr_t_eicus
    {
      CompanyCode     = bukrs;
      CustomParameter = cuspa;
      Value           = value;
    }
  update;
  delete;
  field ( readonly ) CompanyCode;
  field ( readonly : update ) CustomParameter;
  field ( mandatory ) Value;
  association _eInvoiceParameters;
}

define behavior for zetr_ddl_i_invoice_serials //alias <alias_name>
persistent table zetr_t_eiser
lock dependent by _eInvoiceParameters
authorization dependent by _eInvoiceParameters
//etag master <field_name>
{
  mapping for zetr_t_eiser
    {
      CompanyCode       = bukrs;
      SerialPrefix      = serpr;
      Description       = descr;
      NextSerial        = nxtsp;
      MainSerial        = maisp;
      NumberRangeNumber = numrn;
      SerialType        = insrt;
    }
  update;
  delete;
  field ( readonly ) CompanyCode;
  field ( readonly : update ) SerialPrefix;
  validation checkSerials on save { field NumberRangeNumber; create; update; }
  association _eInvoiceParameters;
  association _numberStatus { create; }
  action createNumbers parameter ZETR_DDL_I_FISYEAR_SELECTION result [1] $self;
  side effects { action createNumbers affects entity _numberStatus; }
}

define behavior for zetr_ddl_i_invoice_numstat //alias <alias_name>
persistent table zetr_t_edocnum
lock dependent by _eInvoiceParameters
authorization dependent by _eInvoiceParameters
//etag master <field_name>
{
  mapping for zetr_t_edocnum
    {
      CompanyCode       = bukrs;
      NumberRangeObject = nrobj;
      SerialPrefix      = serpr;
      NumberRangeNumber = numrn;
      FiscalYear        = gjahr;
      NumberStatus      = numst;
    }
  update;
  delete;
  field ( readonly ) CompanyCode, NumberRangeObject, SerialPrefix, NumberRangeNumber, FiscalYear;
  association _invoiceSerials;
  association _eInvoiceParameters;
}

define behavior for zetr_ddl_i_invoice_xslttemp //alias <alias_name>
persistent table zetr_t_eixslt
lock dependent by _eInvoiceParameters
authorization dependent by _eInvoiceParameters
//etag master <field_name>
{
  mapping for zetr_t_eixslt
    {
      CompanyCode     = bukrs;
      XSLTTemplate    = xsltt;
      DefaultTemplate = deflt;
      XSLTContent     = xsltc;
      Filename        = filen;
      Mimetype        = mimet;
    }
  update;
  delete;
  field ( readonly ) CompanyCode;
  field ( readonly : update ) XSLTTemplate;
  association _eInvoiceParameters;
}

define behavior for zetr_ddl_i_invoice_rules //alias <alias_name>
persistent table zetr_t_eirules
lock dependent by _eInvoiceParameters
authorization dependent by _eInvoiceParameters
//etag master <field_name>
{
  mapping for zetr_t_eirules
    {
      CompanyCode                    = bukrs;
      RuleType                       = rulet;
      RuleItemNumber                 = rulen;
      RuleDescription                = descr;
      ReferenceDocumentType          = awtyp;
      ProfileIDInput                 = pidin;
      InvoiceTypeInput               = ityin;
      SalesOrganization              = vkorg;
      DistributionChannel            = vtweg;
      Division                       = spart;
      Plant                          = werks;
      SalesDocumentItemCategory      = pstyv;
      CustomerAccountAssignmentGroup = ktgrd;
      BillingDocumentType            = sddty;
      InvoiceReceiptType             = mmdty;
      AccountingDocumentType         = fidty;
      Partner                        = partner;
      SalesDocument                  = vbeln;
      Exclude                        = excld;
      ProfileID                      = pidou;
      InvoiceType                    = ityou;
      TaxExemption                   = taxex;
      SerialPrefix                   = serpr;
      XSLTTemplate                   = xsltt;
      Note                           = note;
      FieldName                      = fname;
      FieldValue                     = value;
    }
  update;
  delete;
  field ( readonly ) CompanyCode;
  field ( readonly : update ) RuleType, RuleItemNumber;
  association _eInvoiceParameters;
}