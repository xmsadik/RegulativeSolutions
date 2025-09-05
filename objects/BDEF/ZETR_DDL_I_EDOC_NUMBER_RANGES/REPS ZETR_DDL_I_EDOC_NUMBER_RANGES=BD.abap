managed implementation in class zbp_etr_ddl_i_edoc_number_rang unique;
strict ( 2 );

define behavior for zetr_ddl_i_edoc_number_ranges alias NumberRanges
//persistent table <???>
with unmanaged save
lock master
authorization master ( instance )
//etag master <field_name>
{
  //  create;
  update;
  //  delete;
  field ( readonly ) CompanyCode, NumberRangeObject;
  association _NumberStatus { }
}

define behavior for zetr_ddl_i_edoc_number_status alias NumberStatuses
persistent table zetr_t_edocnum
lock dependent by _Ranges
authorization dependent by _Ranges
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
  association _Ranges;
//  static action create_new_numbers;
}