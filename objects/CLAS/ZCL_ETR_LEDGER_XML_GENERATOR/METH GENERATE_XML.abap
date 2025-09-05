  METHOD generate_xml.

    DATA lo_root     TYPE REF TO cx_root.
    DATA lv_xslt_name TYPE progname.
    FIELD-SYMBOLS <fs_root> TYPE any.

    CASE iv_xmlty.
      WHEN '1'.
        lv_xslt_name = '    '.
        ASSIGN ms_root_y TO <fs_root>.
      WHEN '2'.
        lv_xslt_name = '/ITETR/EDF_XML_YB_FORMAT_ST'.
        ASSIGN ms_root_yb TO <fs_root>.
      WHEN '3'.
        lv_xslt_name = '/ITETR/EDF_XML_YB_FORMAT_ST'.
        ASSIGN ms_root_yb TO <fs_root>.
      WHEN '4'.
        lv_xslt_name = '/ITETR/EDF_XML_K_FORMAT_ST'.
        ASSIGN ms_root_k TO <fs_root>.
      WHEN '5'.
        lv_xslt_name = '/ITETR/EDF_XML_KB_FORMAT_ST'.
        ASSIGN ms_root_kb TO <fs_root>.
      WHEN '6'.
        lv_xslt_name = '/ITETR/EDF_XML_KB_FORMAT_ST'.
        ASSIGN ms_root_kb TO <fs_root>.
      WHEN '7'.
        lv_xslt_name = '/ITETR/EDF_XML_DR_FORMAT_ST'.
        ASSIGN ms_root_dr TO <fs_root>.
    ENDCASE.


    IF <fs_root> IS NOT INITIAL.
      TRY.
          CALL TRANSFORMATION (lv_xslt_name)
            SOURCE root_val = <fs_root>
            RESULT XML rv_xml
            OPTIONS xml_header = 'full'.
        CATCH cx_root INTO lo_root.
      ENDTRY.
    ENDIF.

  ENDMETHOD.