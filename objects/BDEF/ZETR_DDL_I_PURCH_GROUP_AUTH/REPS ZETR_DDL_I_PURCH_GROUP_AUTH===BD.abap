managed implementation in class zbp_etr_ddl_i_purch_group_auth unique;
strict ( 2 );

define behavior for zetr_ddL_i_purch_group_auth alias PurchaseGroupAuthorizations
persistent table zetr_t_pgaut
lock master
authorization master ( instance )
//etag master <field_name>
{
  create;
  update;
  delete;
  field ( readonly : update ) Username, PurchaseGroup;

  mapping for zetr_t_pgaut
    {
      Username            = uname;
      PurchaseGroup       = ekgrp;
      IncomingInvoiceView = icivw;
    }
}