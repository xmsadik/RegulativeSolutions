managed implementation in class zbp_etr_ddl_i_archive_paramete unique;
strict ( 2 );

define behavior for zetr_ddl_i_archive_parameters //alias <alias_name>
persistent table zetr_t_eapar
lock master
authorization master ( instance )
//etag master <field_name>
{
  mapping for zetr_t_eapar
    {
      CompanyCode       = bukrs;
      ValidFrom         = datab;
      ValidTo           = datbi;
      Integrator        = intid;
      WSEndpoint        = wsend;
      WSEndpointAlt     = wsena;
      WSUser            = wsusr;
      WSPassword        = wspwd;
      GenerateSerial    = genid;
      Barcode           = barcode;
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

define behavior for zetr_ddl_i_archive_custom //alias <alias_name>
persistent table zetr_t_eacus
lock dependent by _earchiveParameters
authorization dependent by _earchiveParameters
//etag master <field_name>
{
  mapping for zetr_t_eacus
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
  association _earchiveParameters;
}

define behavior for zetr_ddl_i_archive_serials //alias <alias_name>
persistent table zetr_t_easer
lock dependent by _earchiveParameters
authorization dependent by _earchiveParameters
//etag master <field_name>
{
  mapping for zetr_t_easer
    {
      CompanyCode       = bukrs;
      SerialPrefix      = serpr;
      Description       = descr;
      NextSerial        = nxtsp;
      MainSerial        = maisp;
      NumberRangeNumber = numrn;
    }
  update;
  delete;
  field ( readonly ) CompanyCode;
  field ( readonly : update ) SerialPrefix;
  validation checkSerials on save { field NumberRangeNumber; create; update; }
  association _earchiveParameters;
  association _numberStatus { create; }
  action createNumbers parameter ZETR_DDL_I_FISYEAR_SELECTION result [1] $self;
  side effects { action createNumbers affects entity _numberStatus; }
}

define behavior for zetr_ddl_i_archive_numstat //alias <alias_name>
persistent table zetr_t_edocnum
lock dependent by _eArchiveParameters
authorization dependent by _eArchiveParameters
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
  association _archiveSerials;
  association _eArchiveParameters;
}

define behavior for zetr_ddl_i_archive_xslttemp //alias <alias_name>
persistent table zetr_t_eaxslt
lock dependent by _earchiveParameters
authorization dependent by _earchiveParameters
//etag master <field_name>
{
  mapping for zetr_t_eaxslt
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
  association _earchiveParameters;
}

define behavior for zetr_ddl_i_archive_rules //alias <alias_name>
persistent table zetr_t_earules
lock dependent by _earchiveParameters
authorization dependent by _earchiveParameters
//etag master <field_name>
{
  mapping for zetr_t_earules
    {
      CompanyCode                    = bukrs;
      RuleType                       = rulet;
      RuleItemNumber                 = rulen;
      RuleDescription                = descr;
      ReferenceDocumentType          = awtyp;
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
      InvoiceType                    = ityou;
      TaxExemption                   = taxex;
      Exclude                        = excld;
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
  association _earchiveParameters;
}