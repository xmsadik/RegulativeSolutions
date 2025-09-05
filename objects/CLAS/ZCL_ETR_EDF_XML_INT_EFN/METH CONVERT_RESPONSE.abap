  METHOD convert_response.

    TYPES:
      BEGIN OF ty_xml_table,
        cname  TYPE string,
        cvalue TYPE string,
      END OF ty_xml_table,
      tt_xml_table TYPE STANDARD TABLE OF ty_xml_table WITH EMPTY KEY.

    DATA:
      lv_xml_input TYPE xstring,
      ls_xml_table TYPE ty_xml_table,
      lt_xml_table TYPE tt_xml_table,
      lv_code      TYPE string.

    TRY.
        "Convert string to xstring
        lv_xml_input = cl_abap_conv_codepage=>create_out( )->convert( iv_response ).

        "Create XML reader
        DATA(lo_reader) = cl_sxml_string_reader=>create( lv_xml_input ).

        "Parse XML
        DATA: lv_name TYPE string.

        DO.
          DATA(lo_current) = lo_reader->read_next_node( ).
          IF lo_current IS INITIAL.
            EXIT.
          ENDIF.

          CASE lo_current->type.
            WHEN if_sxml_node=>co_nt_element_open.
              DATA(lo_element) = CAST if_sxml_open_element( lo_current ).
              DATA(ls_qname) = lo_element->qname.
              lv_name = ls_qname-name.

            WHEN if_sxml_node=>co_nt_value.
              DATA(lo_value) = CAST if_sxml_value_node( lo_current ).
              APPEND VALUE ty_xml_table(
                cname  = lv_name
                cvalue = lo_value->get_value( )
              ) TO lt_xml_table.
          ENDCASE.
        ENDDO.

        "Process response
        IF iv_code = '200'.
          READ TABLE lt_xml_table INTO ls_xml_table
            WITH KEY cname = 'sonucKodu'.
          IF sy-subrc = 0.
            rs_msg_response-msgty = COND #(
              WHEN ls_xml_table-cvalue = '907' THEN 'S'  "CSV dosyası başarıyla yüklendi
              ELSE 'E'
            ).
          ELSE.
            rs_msg_response-msgty = 'E'.
          ENDIF.
        ELSE.
          rs_msg_response-msgty = 'E'.
        ENDIF.

        "Get result description
        READ TABLE lt_xml_table INTO ls_xml_table
          WITH KEY cname = 'sonucAciklama'.
        IF sy-subrc = 0.
          rs_msg_response-msgtx = ls_xml_table-cvalue.
        ENDIF.

        "Modern string concatenation
        rs_msg_response-msgtx = |{ rs_msg_response-msgtx } HTTP Status Code: { iv_code } - { iv_response }|.

      CATCH cx_sxml_parse_error INTO DATA(lx_parse_error).
        rs_msg_response-msgty = 'E'.
        rs_msg_response-msgtx = lx_parse_error->get_text( ).
      CATCH cx_transformation_error INTO DATA(lx_transform_error).
        rs_msg_response-msgty = 'E'.
        rs_msg_response-msgtx = lx_transform_error->get_text( ).
      CATCH cx_sy_conversion_error INTO DATA(lx_conversion_error).
        rs_msg_response-msgty = 'E'.
        rs_msg_response-msgtx = lx_conversion_error->get_text( ).
    ENDTRY.





*    DATA lv_xml_input TYPE xstring.
*    DATA ls_xml_table TYPE smum_xmltb.
*    DATA lt_xml_table TYPE TABLE OF smum_xmltb.
*    DATA lt_return    TYPE TABLE OF bapiret2.
*    DATA lv_code      TYPE string.
*
*    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
*      EXPORTING
*        text   = iv_response
*      IMPORTING
*        buffer = lv_xml_input
*      EXCEPTIONS
*        failed = 1
*        OTHERS = 2.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
*
*    CALL FUNCTION 'SMUM_XML_PARSE'
*      EXPORTING
*        xml_input = lv_xml_input
*      TABLES
*        xml_table = lt_xml_table
*        return    = lt_return.
*
*    IF iv_code EQ '200'.
*      CLEAR ls_xml_table.
*      READ TABLE lt_xml_table INTO ls_xml_table WITH KEY cname = 'sonucKodu'.
*
*      IF ls_xml_table-cvalue EQ '907'.  "CSV dosyası başarıyla yüklendi
*        rs_msg_response-msgty = 'S'.
*      ELSE.
*        rs_msg_response-msgty = 'E'.
*      ENDIF.
*    ELSE.
*      rs_msg_response-msgty = 'E'.
*    ENDIF.
*
*    CLEAR ls_xml_table.
*    READ TABLE lt_xml_table INTO ls_xml_table WITH KEY cname = 'sonucAciklama'.
*    rs_msg_response-msgtx = ls_xml_table-cvalue.
*
*    lv_code = iv_code.
*
*    CONCATENATE rs_msg_response-msgtx
*                'HTTP Status Code:'
*                lv_code
*                '-'
*                iv_response
*                INTO rs_msg_response-msgtx
*                SEPARATED BY space.

  ENDMETHOD.