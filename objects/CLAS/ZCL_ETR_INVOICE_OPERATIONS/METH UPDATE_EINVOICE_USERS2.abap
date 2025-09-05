  METHOD update_einvoice_users2.
    zcl_etr_einvoice_ws=>factory( mv_company_code )->download_registered_taxpayers2( ).
  ENDMETHOD.