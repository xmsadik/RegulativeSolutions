managed implementation in class zbp_etr_ddl_i_service_users unique;
strict ( 2 );

define behavior for zetr_ddl_i_service_users //alias <alias_name>
persistent table zetr_t_serv_user
lock master
authorization master ( instance )
//etag master <field_name>
{


  mapping for zetr_t_serv_user
    {
      ServiceId  = service_id;
      ServiceUrl = service_url;
      Username   = username;
      Password   = password;
    }

  field ( readonly : update ) ServiceId;
  create;
  update;
  delete;


}