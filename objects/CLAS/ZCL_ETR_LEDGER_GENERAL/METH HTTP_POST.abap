  METHOD http_post.

    DATA: lv_user  TYPE string,
          lv_pass  TYPE string,
          lv_desti TYPE rfcdest.

    lv_desti = iv_desti.

    CLEAR: ev_status, ev_success, ev_result, ev_errors, ev_errors_type, es_apiret.

    TRY.
        " Create HTTP destination
        DATA(lo_destination) = COND #(
          WHEN iv_desti IS NOT INITIAL
            THEN cl_http_destination_provider=>create_by_destination( lv_desti )
          ELSE
            cl_http_destination_provider=>create_by_url( iv_url ) ).

        " Create HTTP client
        DATA(lo_client) = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).

        " Get request object
        DATA(lo_request) = lo_client->get_http_request( ).

        " Set request headers and body
        lo_request->set_header_field(
            i_name  = 'Accept-Charset'
            i_value = 'ISO-8859-9' ).

        lo_request->set_header_field(
            i_name  = 'Content-Type'
            i_value = 'application/json' ).

        " Set request body
        lo_request->set_text( iv_json ).

        " Set authentication if provided
        IF iv_user IS NOT INITIAL AND iv_pass IS NOT INITIAL.
          lv_user = iv_user.
          lv_pass = iv_pass.

          lo_client->get_http_request( )->set_authorization_basic(
              i_username = lv_user
              i_password = lv_pass ).
        ENDIF.

        " Execute request and get response
        DATA(lo_response) = lo_client->execute( if_web_http_client=>post ).

        " Get response status
        DATA(lv_status) = lo_response->get_status( ).
        ev_status = lv_status-code.

        " Get response body
        ev_result = lo_response->get_text( ).

        " Process response
        IF ev_status = 200 AND ev_result CS '"success":"true"'.
          ev_success = abap_true.
        ELSE.
          ev_success = abap_false.

          " Extract error information
          IF ev_result CS '"success":"true"' OR ev_result CS '"success":"false"'.
            " Call method to convert API return
            es_apiret = convert_api_return( ev_result ).

            " First item in data array or message
            IF es_apiret-data IS NOT INITIAL.
              READ TABLE es_apiret-data INTO DATA(lv_error) INDEX 1.
              ev_errors-message = lv_error.
            ENDIF.

            IF ev_errors-message IS INITIAL.
              ev_errors-message = es_apiret-message.
            ENDIF.

            " Set error type based on transfer type
            CASE es_apiret-transfertype.
              WHEN 'JSON'.
                ev_errors_type-transfertype = 'JSON'.
              WHEN 'ZIP_BASE64'.
                ev_errors_type-transfertype = 'ZIP_BASE64'.
              WHEN 'BASE64'.
                ev_errors_type-transfertype = 'BASE64'.
            ENDCASE.
          ELSE.
            ev_errors-message = ev_result.
            ev_errors_type-transfertype = 'JSON'.
          ENDIF.
        ENDIF.

        " Close HTTP client
        lo_client->close( ).

      CATCH cx_http_dest_provider_error INTO DATA(lx_dest_provider_error).
        ev_success = abap_false.
        ev_errors-message = lx_dest_provider_error->get_text( ).
        ev_errors_type-transfertype = 'JSON'.

      CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
        ev_success = abap_false.
        ev_errors-message = lx_web_http_client_error->get_text( ).
        ev_errors_type-transfertype = 'JSON'.

      CATCH cx_web_message_error INTO DATA(lx_web_message_error).
        ev_success = abap_false.
        ev_errors-message = lx_web_message_error->get_text( ).
        ev_errors_type-transfertype = 'JSON'.
    ENDTRY.
  ENDMETHOD.