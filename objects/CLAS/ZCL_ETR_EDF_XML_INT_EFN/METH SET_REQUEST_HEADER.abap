  METHOD set_request_header.

    DATA lv_length TYPE i.
    DATA lv_length_s TYPE string.

    lv_length = xstrlen( iv_request ).
    lv_length_s = lv_length.
    CONDENSE lv_length_s NO-GAPS.
*    mo_client->request->set_header_field( name = 'Accept-Encoding'   value =  'gzip,deflate' ).
*    mo_client->request->set_header_field( name = 'Content-Type'      value =  'text/xml;charset=UTF-8' ).
*    mo_client->request->set_header_field( name = 'SOAPAction'        value =  '' ).
*    mo_client->request->set_header_field( name = 'Connection'        value =  'Keep-Alive' ).
*    mo_client->request->set_header_field( name = 'Content-Length'    value =  lv_length_s ).
    "Modern way to set headers in ABAP on Cloud
    mo_client->get_http_request( )->set_header_fields( VALUE #(
        ( name = 'Accept-Encoding'  value = 'gzip,deflate' )
        ( name = 'Content-Type'     value = 'text/xml;charset=UTF-8' )
        ( name = 'SOAPAction'       value = '' )
        ( name = 'Connection'       value = 'Keep-Alive' )
        ( name = 'Content-Length'   value = lv_length_s )
    ) ).

  ENDMETHOD.