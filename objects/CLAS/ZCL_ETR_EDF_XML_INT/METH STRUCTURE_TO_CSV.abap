  METHOD structure_to_csv.
    DATA:
      lo_struct_descr TYPE REF TO cl_abap_structdescr,
      lt_components   TYPE cl_abap_structdescr=>component_table,
      lv_value        TYPE string.

    TRY.
        "Get structure description
        lo_struct_descr ?= cl_abap_typedescr=>describe_by_data( is_structure ).

        IF lo_struct_descr IS NOT BOUND.
*          RAISE EXCEPTION TYPE zcx_etr_edf_static_check
*            EXPORTING
*              textid = VALUE #( msgid = 'ZETR_EDF'
*                               msgno = 006 )
*              text   = 'Invalid structure for CSV conversion'.
        ENDIF.

        lt_components = lo_struct_descr->get_components( ).

        "Process each component
        LOOP AT lt_components INTO DATA(ls_component).
          ASSIGN COMPONENT ls_component-name OF STRUCTURE is_structure TO FIELD-SYMBOL(<fs_value>).
          IF sy-subrc = 0.
            "Convert component value to string
            lv_value = COND #( WHEN <fs_value> IS INITIAL
                              THEN ''
                              ELSE condense( |{ <fs_value> }| ) ).

            "Add separator if not first field
            IF rv_csv IS NOT INITIAL.
              rv_csv = |{ rv_csv }{ iv_separator }|.
            ENDIF.

            "Add value to CSV string
            rv_csv = |{ rv_csv }{ lv_value }|.
          ENDIF.
        ENDLOOP.

      CATCH cx_sy_move_cast_error INTO DATA(lx_cast_error).
*        RAISE EXCEPTION TYPE zcx_etr_edf_static_check
*          EXPORTING
*            textid   = VALUE #( msgid = 'ZETR_EDF'
*                              msgno = 007 )
*            text     = 'Error during structure processing'
*            previous = lx_cast_error.
    ENDTRY.
  ENDMETHOD.