managed implementation in class zbp_etr_ddl_i_ledger_branch_in unique;
strict ( 2 );

define behavior for zetr_ddl_i_ledger_branch_info alias BranchInfo
persistent table zetr_t_sbblg
lock master
authorization master ( instance )
//etag master <field_name>
{
  mapping for zetr_t_sbblg
    {
      CompanyCode     = bukrs;
      BranchCode      = bcode;
      BranchType      = btype;
      SubCompany      = sub_bukrs;
      SubBusinessArea = sub_gsber;
      BranchName1     = bname1;
      BranchName2     = bname2;
      Address1        = adress1;
      Address2        = adress2;
      HouseNumber     = house_num;
      PostalCode      = postal_code;
      City            = city;
      Country         = country;
      TelNumber       = tel_number;
      FaxNumber       = fax_number;
      EMail           = email;
      Creator         = creator;
      Days45          = days45;
    }
  create;
  update;
  delete;
  field ( readonly:update ) CompanyCode, BranchCode;
}