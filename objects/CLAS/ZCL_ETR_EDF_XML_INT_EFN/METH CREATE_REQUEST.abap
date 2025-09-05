  METHOD create_request.

    TYPES:
      BEGIN OF ty_entry,
        key   TYPE string,
        value TYPE string,
      END OF ty_entry.

    DATA:
      lv_bcode       TYPE n LENGTH 4,
      lv_part_no     TYPE zetr_d_part_no,
      lv_request     TYPE string,
      lv_filename    TYPE string,
      lv_base_64     TYPE string,
      lv_zipped_file TYPE xstring,
      lo_zip         TYPE REF TO cl_abap_zip.

    CONSTANTS:
      lc_op_enc TYPE x VALUE 36,
      lc_csv    TYPE string VALUE '.csv',
      lc_zip    TYPE string VALUE '.zip'.

    "Process branch code
    lv_bcode = iv_bcode.

    "Process part number
    lv_part_no = iv_partn.
    SHIFT lv_part_no LEFT DELETING LEADING '0'.
    IF lv_part_no IS INITIAL.
      lv_part_no = '1'.
    ENDIF.

    "Create filename
    lv_filename = |{ ms_srkdb-stcd1 }_{ lv_bcode }_{ iv_gjahr }{ iv_monat }_{ lv_part_no }{ lc_csv }|.

    "Create ZIP file
    TRY.
        "Create ZIP using CREATE OBJECT
        CREATE OBJECT lo_zip.

        "Add content to ZIP
        lo_zip->add( name    = lv_filename
                     content = iv_xml ).

        "Get zipped content
        lv_zipped_file = lo_zip->save( ).

        "Convert to base64 using modern method
        lv_base_64 = cl_web_http_utility=>encode_x_base64( lv_zipped_file ).

        "Update filename for zip
        REPLACE FIRST OCCURRENCE OF lc_csv IN lv_filename WITH lc_zip.

        "Create SOAP request using string templates
        lv_request =
        |<?xml version="1.0" encoding="UTF-8"?>| &&
        |<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:web="http://webservice.edefter.uut.cs.com.tr/">| &&
          |<soapenv:Header>| &&
            |<wsse:Security xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd">| &&
              |<wsse:UsernameToken>| &&
                |<wsse:Username>{ ms_srkdb-sausr }</wsse:Username>| &&
                |<wsse:Password>{ ms_srkdb-sapas }</wsse:Password>| &&
              |</wsse:UsernameToken>| &&
            |</wsse:Security>| &&
          |</soapenv:Header>| &&
          |<soapenv:Body>| &&
            |<web:eDefterCsvFileYukle>| &&
              |<arg0>| &&
                |<donem>{ iv_gjahr }{ iv_monat }</donem>| &&
                |<parametreList>| &&
                  |<entry>| &&
                    |<key>dosya</key>| &&
                    |<value>{ lv_base_64 }</value>| &&
                  |</entry>| &&
                  |<entry>| &&
                    |<key>dosyaIsmi</key>| &&
                    |<value>{ lv_filename }</value>| &&
                  |</entry>| &&
                |</parametreList>| &&
                |<subeKodu>{ lv_bcode }</subeKodu>| &&
                |<vknTckn>{ ms_srkdb-stcd1 }</vknTckn>| &&
              |</arg0>| &&
            |</web:eDefterCsvFileYukle>| &&
          |</soapenv:Body>| &&
        |</soapenv:Envelope>|.

        "Convert string to xstring using modern method
        DATA(lo_conv) = cl_abap_conv_codepage=>create_out( ).
        rv_request = lo_conv->convert( lv_request ).


      CATCH cx_sy_conversion_error INTO DATA(lx_conv_error).
        "Handle conversion error

      CATCH cx_parameter_invalid_range INTO DATA(lx_param_error).
        "Handle parameter error

    ENDTRY.



*    DATA lv_bcode       TYPE n LENGTH 4.
*    DATA lv_part_no     TYPE zetr_d_part_no.
*    DATA lv_request     TYPE string.
*    DATA lv_filename    TYPE string.
*    DATA lv_base_64     TYPE string.
*    DATA lo_zip         TYPE REF TO cl_abap_zip.
*    DATA lv_zipped_file TYPE xstring.
*    CONSTANTS lc_op_enc TYPE x VALUE 36.
*
*    lv_bcode = iv_bcode.
*
*    lv_part_no = iv_partn.
*    SHIFT lv_part_no LEFT DELETING LEADING '0'.
*
*    IF lv_part_no IS INITIAL.
*      lv_part_no = '1'.
*    ENDIF.
*
*    CONCATENATE ms_srkdb-stcd1
*                '_'
*                lv_bcode
*                '_'
*                iv_gjahr
*                iv_monat
*                '_'
*                lv_part_no
*                '.csv'
*                INTO lv_filename.
*
*    CREATE OBJECT lo_zip.
*    lo_zip->add(
*      EXPORTING
*        name           = lv_filename
*        content        = iv_xml
*    ).
*
*    lv_zipped_file = lo_zip->save( ).
*
*    CLEAR lv_base_64.
*    CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
*      EXPORTING
*        input  = lv_zipped_file
*      IMPORTING
*        output = lv_base_64.
*
*    REPLACE FIRST OCCURRENCE OF '.csv' IN lv_filename WITH '.zip'.
*
*    CONCATENATE
*    '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:web="http://webservice.edefter.uut.cs.com.tr/">'
*      '<soapenv:Header>'
*        '<wsse:Security xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd">'
*          '<wsse:UsernameToken>'
*            '<wsse:Username>' ms_srkdb-sausr '</wsse:Username>'
*            '<wsse:Password>' ms_srkdb-sapas '</wsse:Password>'
*          '</wsse:UsernameToken>'
*        '</wsse:Security>'
*      '</soapenv:Header>'
*       '<soapenv:Body>'
*          '<web:eDefterCsvFileYukle>'
*             '<arg0>'
*                '<donem>' iv_gjahr iv_monat '</donem>'
*                '<parametreList>'
*                   '<entry>'
*                      '<key>dosya</key>'
*                      '<value>' lv_base_64 '</value>'
*                   '</entry>'
*                   '<entry>'
*                      '<key>dosyaIsmi</key>'
*                      '<value>' lv_filename '</value>'
*                   '</entry>'
*                '</parametreList>'
*                '<subeKodu>' lv_bcode '</subeKodu>'
*                '<vknTckn>' ms_srkdb-stcd1 '</vknTckn>'
*             '</arg0>'
*          '</web:eDefterCsvFileYukle>'
*       '</soapenv:Body>'
*    '</soapenv:Envelope>'
*    INTO lv_request.
*
*    CALL FUNCTION 'ECATT_CONV_STRING_TO_XSTRING'
*      EXPORTING
*        im_string  = lv_request
*      IMPORTING
*        ex_xstring = rv_request.

  ENDMETHOD.