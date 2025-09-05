managed implementation in class zbp_etr_ddl_i_ledger_symm_info unique;
strict ( 2 );

define behavior for ZETR_DDL_I_LEDGER_SYMM_INFO alias SYMMInfo
persistent table zetr_t_symmb
lock master
authorization master ( instance )
//etag master <field_name>
{
  mapping for zetr_t_symmb
    {
      CompanyCode  = bukrs;
      Title        = mmtit;
      Name         = name;
      Surname      = surname;
      HouseNumber  = house_num;
      Adress1      = adress1;
      Adress2      = adress2;
      Country      = country_u;
      City         = city;
      Subdivision  = ilce;
      PostalCode   = postal_code;
      ContractDate = contrdate;
      ContractName = contrname;
      ContractNo   = contrno;
      TelNumber    = tel_number;
      FaxNumber    = fax_number;
      Email        = email;
      WebSite      = web;
    }
  create;
  update;
  delete;
  field ( readonly : update ) CompanyCode, Title;
}