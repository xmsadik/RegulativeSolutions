  METHOD send.
    TYPES:
      BEGIN OF http_status,
        code   TYPE i,
        reason TYPE string,
      END OF http_status .
    DATA: lv_url      TYPE string,
          lv_request  TYPE xstring,
          lv_code     TYPE http_status,
*          lv_reason   TYPE string,
          lv_response TYPE string,
          lv_user     TYPE string,
          lv_password TYPE string.

    CALL METHOD me->set_company_info
      EXPORTING
        iv_bukrs = iv_bukrs.

    lv_url = ms_srkdb-srapi.

    CALL METHOD me->create_by_url
      EXPORTING
        iv_url   = lv_url
        iv_desti = ms_srkdb-desti.

    CALL METHOD me->create_request
      EXPORTING
        iv_xml     = iv_xml
        iv_bcode   = iv_bcode
        iv_gjahr   = iv_gjahr
        iv_monat   = iv_monat
        iv_partn   = iv_partn
      RECEIVING
        rv_request = lv_request.

    IF me->mv_auth IS NOT INITIAL.
      lv_user     = ms_srkdb-sausr.
      lv_password = ms_srkdb-sapas.

      mo_client->get_http_request( )->set_authorization_basic(
                i_username = lv_user
                i_password = lv_password ).
    ENDIF.

    me->set_request_header( EXPORTING iv_request = lv_request ).

    mo_client->get_http_request( )->set_binary( lv_request ).

    TRY.
        " Send request and get response
        DATA(lo_response) = mo_client->execute(
            i_method = if_web_http_client=>post
        ).

        " Get the response status
        lv_code = lo_response->get_status( ).
*        data(lv_reason) = lo_response->get_( ).

        " Get the response body
        lv_response = lo_response->get_text( ).

      CATCH cx_web_http_client_error INTO DATA(lx_client_error).
        " Handle client error
      CATCH cx_web_http_conversion_failed INTO DATA(lx_comm_failure).

      CATCH cx_web_message_error INTO DATA(lc_messag).
    ENDTRY.

    CALL METHOD me->convert_response
      EXPORTING
        iv_response     = lv_response
        iv_code         = lv_code-code
        iv_reason       = lv_code-reason
      RECEIVING
        rs_msg_response = rs_msg_response.

    " Close HTTP client
    IF mo_client IS BOUND.
      TRY.
          mo_client->close( ).
        CATCH cx_web_http_client_error INTO DATA(lx_close_error).
          " Log or handle close error
      ENDTRY.

    ENDIF.

  ENDMETHOD.