managed implementation in class zbp_etr_ddl_i_public_iban_mapp unique;
strict ( 2 );

define behavior for zetr_ddl_i_public_iban_mapping //alias <alias_name>
persistent table zetr_t_eipi
lock master
authorization master ( instance )
//etag master <field_name>
{
  create;
  update;
  delete;
  field ( readonly:update ) TaxID;
  mapping for zetr_t_eipi
    {
      TaxID         = taxid;
      Customer      = kunnr;
      IBAN          = iban;
      PaymentMethod = payment;
    }
}