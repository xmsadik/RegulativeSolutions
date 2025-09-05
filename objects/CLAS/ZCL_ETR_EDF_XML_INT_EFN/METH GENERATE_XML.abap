  METHOD generate_xml.
    TYPES:
      BEGIN OF ty_csv_line,
        line TYPE string,
      END OF ty_csv_line,
      tt_csv_lines TYPE STANDARD TABLE OF ty_csv_line WITH EMPTY KEY.

    DATA:
      lt_csv_lines TYPE tt_csv_lines,
      lv_csv_line  TYPE ty_csv_line,
      ls_item      TYPE zetr_s_efn_csv_item,
      lv_csv_str   TYPE string,
      lv_xstring   TYPE xstring.

    CONSTANTS:
      lc_separator     TYPE c LENGTH 1 VALUE ';',
      lc_newline       TYPE string VALUE cl_abap_char_utilities=>newline,
      lc_encoding_utf8 TYPE string VALUE 'UTF-8',
      lc_bom_utf8      TYPE xstring VALUE cl_abap_char_utilities=>byte_order_mark_utf8.

    TRY.
        "Add header for part numbers less than 2
        IF mv_partn < '000002'.
          "Convert header structure to CSV string
          DATA(lv_header_csv) = me->structure_to_csv(
            is_structure = ms_efinans_csv-header
            iv_separator = lc_separator ).

          "Append header line
          APPEND VALUE #( line = lv_header_csv ) TO lt_csv_lines.
        ENDIF.

        "Process items
        LOOP AT ms_efinans_csv-item INTO ls_item.
          "Convert amounts using modern string processing
          ls_item-amount = replace( val  = ls_item-amount
                                  sub  = ','
                                  with = '.' ).
          ls_item-total_debit = replace( val  = ls_item-total_debit
                                       sub  = ','
                                       with = '.' ).
          ls_item-total_credit = replace( val  = ls_item-total_credit
                                        sub  = ','
                                        with = '.' ).

          "Convert item structure to CSV string
          DATA(lv_item_csv) = me->structure_to_csv(
            is_structure = ls_item
            iv_separator = lc_separator ).

          "Clean up special characters
          lv_item_csv = replace( val  = lv_item_csv
                               sub  = '"'
                               with = '' ).

          "Append item line
          APPEND VALUE #( line = lv_item_csv ) TO lt_csv_lines.
        ENDLOOP.

        "Combine all lines with newline
        CLEAR lv_csv_str.
        LOOP AT lt_csv_lines INTO lv_csv_line.
          "Use string templates for concatenation
          IF lv_csv_str IS INITIAL.
            lv_csv_str = lv_csv_line-line.
          ELSE.
            lv_csv_str = |{ lv_csv_str }{ lc_newline }{ lv_csv_line-line }|.
          ENDIF.
        ENDLOOP.

        CONSTANTS lc_mimetype TYPE c LENGTH 50 VALUE 'text/plain; charset=utf-8'.
*
*        "Convert string to xstring using transformation
*        CALL TRANSFORMATION id
*          SOURCE data = lv_csv_str
*          RESULT XML lv_xstring.

        lv_xstring = cl_abap_conv_codepage=>create_out( )->convert( lv_csv_str ).


        "Add BOM for UTF-8
        ev_filex = lc_bom_utf8 &&  lv_xstring.
        ev_ftype = 'CSV'.

        "lc_bom_utf8 && *

        DATA(lo_conv) = cl_abap_conv_codepage=>create_out( codepage = 'UTF-8' ).
        ev_rawstring  = lo_conv->convert( source = lv_csv_str  ).
        ev_csv_str    = lv_csv_str.


      CATCH cx_transformation_error INTO DATA(lx_trans_error).

      CATCH cx_parameter_invalid_range INTO DATA(lx_param_error).

    ENDTRY.
  ENDMETHOD.