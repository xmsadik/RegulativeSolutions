  METHOD serialize_data.

    DATA: lc_serializer TYPE REF TO zcl_etr_cls_json_ser,
          lc_zip        TYPE REF TO cl_abap_zip,
          lvdata        TYPE string.


    CREATE OBJECT lc_serializer.

    lc_serializer->set_data( i_data = iv_data ).

    lc_serializer->serialize( ).

    ev_json = lc_serializer->get_data( ).


    IF iv_jsonxstring IS NOT INITIAL OR iv_jsonbase64 IS NOT INITIAL OR iv_jsonbase64zip IS NOT INITIAL.
      TRY.
          " One-line conversion using method chaining
          ev_jsonxstring = cl_abap_conv_codepage=>create_out(
                             codepage = 'UTF-8'  "encoding yerine codepage kullanılıyor
                           )->convert( source = ev_json ).

        CATCH cx_root INTO DATA(lx_error).
          RAISE SHORTDUMP lx_error.
      ENDTRY.
    ENDIF.

    IF iv_jsonbase64 IS NOT INITIAL.

      " For binary data (xstring) to base64
      DATA(lo_conv) = cl_abap_conv_codepage=>create_in( ).

      " Convert xstring to string first if needed
      DATA(lv_string) =  lo_conv->convert( source = ev_jsonxstring ).

      " Then encode to base64
      ev_jsonbase64 = cl_web_http_utility=>encode_base64(
          unencoded = lv_string
      ).

    ENDIF.

    IF iv_jsonzip IS NOT INITIAL OR iv_jsonbase64zip IS NOT INITIAL.
      IF lc_zip IS INITIAL.
        CREATE OBJECT lc_zip.
      ENDIF.

      lc_zip->add(  name    = 'ZIP_FILE'
                    content = ev_jsonxstring ).

      ev_jsonzip = lc_zip->save( ).
    ENDIF.

    IF iv_jsonbase64zip IS NOT INITIAL.

      " Now use the properly typed variable for base64 encoding
      ev_jsonbase64zip = cl_web_http_utility=>encode_x_base64( unencoded = ev_jsonzip ).
    ENDIF.


  ENDMETHOD.