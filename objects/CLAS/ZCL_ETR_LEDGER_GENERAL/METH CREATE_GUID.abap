  METHOD create_guid.
    TRY.
        "Generate UUID using modern class
        DATA(lv_uuid_32) = cl_system_uuid=>create_uuid_c32_static( ).

        "Format UUID with hyphens using string template
        rv_guid = |{ lv_uuid_32+0(8) }-{ lv_uuid_32+8(4) }-{ lv_uuid_32+12(4) }-{ lv_uuid_32+16(4) }-{ lv_uuid_32+20(12) }|.

        "Convert to lowercase
        rv_guid = to_lower( rv_guid ).

      CATCH cx_uuid_error INTO DATA(lx_uuid_error).
        "Handle UUID generation error
*        RAISE EXCEPTION TYPE zcx_etr_edf_static_check
*          EXPORTING
*            textid   = VALUE #( msgid = 'ZETR_EDF'
*                              msgno = 008 )
*            text     = 'Error generating UUID'
*            previous = lx_uuid_error.
    ENDTRY.
  ENDMETHOD.