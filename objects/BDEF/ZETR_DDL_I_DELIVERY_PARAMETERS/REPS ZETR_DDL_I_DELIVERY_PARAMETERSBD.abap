managed implementation in class zbp_etr_ddl_i_delivery_paramet unique;
strict ( 2 );

define behavior for zetr_ddl_i_delivery_parameters //alias <alias_name>
persistent table zetr_t_edpar
lock master
authorization master ( instance )
//etag master <field_name>
{
  mapping for zetr_t_edpar
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
      Barcode           = barcode;
      PKAlias           = pk_alias;
      GBAlias           = gb_alias;
      InternalNumbering = intnum;
    }
  create;
  update;
  delete;
  field ( readonly : update ) CompanyCode;
  association _customParameters { create; }
  association _deliverySerials { create; }
  association _xsltTemplates { create; }
  association _deliveryRules { create; }
}

define behavior for zetr_ddl_i_delivery_custom //alias <alias_name>
persistent table zetr_t_edcus
lock dependent by _eDeliveryParameters
authorization dependent by _eDeliveryParameters
//etag master <field_name>
{
  mapping for zetr_t_edcus
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
  association _eDeliveryParameters;
}

define behavior for zetr_ddl_i_delivery_serials //alias <alias_name>
persistent table zetr_t_edser
lock dependent by _eDeliveryParameters
authorization dependent by _eDeliveryParameters
//etag master <field_name>
{
  mapping for zetr_t_edser
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
  association _eDeliveryParameters;
  association _numberStatus { create; }
  action createNumbers parameter ZETR_DDL_I_FISYEAR_SELECTION result [1] $self;
  side effects { action createNumbers affects entity _numberStatus; }
}

define behavior for ZETR_DDL_I_DELIVERY_NUMSTAT //alias <alias_name>
persistent table zetr_t_edocnum
lock dependent by _eDeliveryParameters
authorization dependent by _eDeliveryParameters
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
  association _deliverySerials;
  association _eDeliveryParameters;
}

define behavior for zetr_ddl_i_delivery_xslttemp //alias <alias_name>
persistent table zetr_t_edxslt
lock dependent by _eDeliveryParameters
authorization dependent by _eDeliveryParameters
//etag master <field_name>
{
  mapping for zetr_t_edxslt
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
  association _eDeliveryParameters;
}

define behavior for zetr_ddl_i_delivery_rules //alias <alias_name>
persistent table zetr_t_edrules
lock dependent by _eDeliveryParameters
authorization dependent by _eDeliveryParameters
//etag master <field_name>
{
  mapping for zetr_t_edrules
    {
      CompanyCode              = bukrs;
      RuleType                 = rulet;
      RuleItemNumber           = rulen;
      RuleDescription          = descr;
      ReferenceDocumentType    = awtyp;
      ProfileIDInput           = pidin;
      eDeliveryTypeInput       = dtyin;
      SalesOrganization        = vkorg;
      DistributionChannel      = vtweg;
      Division                 = spart;
      Plant                    = werks;
      StorageLocation          = lgort;
      ReceivingPlant           = umwrk;
      ReceivingStorageLocation = umlgo;
      SpecialStockType         = sobkz;
      MovementType             = bwart;
      DeliveryType             = sddty;
      GoodsMovementType        = mmdty;
      AccountingDocumentType   = fidty;
      Partner                  = partner;
      Exclude                  = excld;
      ProfileID                = pidou;
      eDeliveryType            = dtyou;
      SerialPrefix             = serpr;
      XSLTTemplate             = xsltt;
      Note                     = note;
      FieldName                = fname;
      FieldValue               = value;
    }
  update;
  delete;
  field ( readonly ) CompanyCode;
  field ( readonly : update ) RuleType, RuleItemNumber;
  association _eDeliveryParameters;
}