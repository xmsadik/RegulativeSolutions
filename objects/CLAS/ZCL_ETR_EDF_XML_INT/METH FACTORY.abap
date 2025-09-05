  METHOD factory.

    DATA lv_intid TYPE zetr_e_intid.
    DATA lv_class_name TYPE zetr_e_char30.

    CALL METHOD zcl_etr_edf_xml_int=>get_intid
      EXPORTING
        iv_bukrs = iv_bukrs
      RECEIVING
        rv_intid = lv_intid.

    CHECK lv_intid IS NOT INITIAL.

    CONCATENATE mc_child_class_name_prefix
                lv_intid
                INTO lv_class_name.

    CREATE OBJECT ro_object TYPE (lv_class_name) EXPORTING is_header = is_header
                                                           iv_purpose = iv_purpose
                                                           iv_partn = iv_partn.


  ENDMETHOD.