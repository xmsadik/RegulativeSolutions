  METHOD create_by_url.
    DATA lv_dest TYPE rfcdest.

    lv_dest = iv_desti.
    "'https://edeftertest.efinans.com.tr/edefter/ws/DefterOlusturWebServices?wsdl'
    TRY.

        DATA(lo_destination) = COND #(
            WHEN ms_srkdb-sm59_dest IS NOT INITIAL
              THEN cl_http_destination_provider=>create_by_destination( lv_dest  )
            ELSE
              cl_http_destination_provider=>create_by_url( iv_url ) ).

        " Create HTTP client
        mo_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).

*        " Close HTTP client
*        IF mo_client IS BOUND.
*          mo_client->close( ).
*        ENDIF.
      CATCH cx_http_dest_provider_error INTO DATA(lx_dest_provider_error).

      CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).

      CATCH cx_web_message_error INTO DATA(lx_web_message_error).

    ENDTRY.

  ENDMETHOD.