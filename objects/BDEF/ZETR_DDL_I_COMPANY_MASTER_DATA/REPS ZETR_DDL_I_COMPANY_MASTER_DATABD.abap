managed implementation in class zbp_etr_ddl_i_company_master_d unique;
strict ( 1 );

define behavior for zetr_ddl_i_company_master_data alias CompanyMaster
//persistent table <???>
with unmanaged save
lock master
authorization master ( instance )
//etag master <field_name>
{
  //  create;
  //  update;
  //  delete;
  field ( readonly ) CompanyCode;
  action ( features : instance ) generateData result [1] $self;
}