  METHOD send_company_info.


    DATA: ls_ent_info     TYPE zetr_s_comp_post_info,
          lv_json_xstring TYPE xstring,
          lv_json_base64  TYPE string,
          ls_send_values  TYPE zetr_s_api_post,
          lv_send_values  TYPE zetr_e_string,
          lv_url          TYPE string,
          lv_result       TYPE string,
          lv_success      TYPE zetr_E_durum,
          ls_parts        TYPE ZETR_t_oldef.

    gv_bukrs   = i_bukrs.
    gv_bcode   = i_bcode.
    gr_budat[] = tr_budat[].

    get_company( ).
    set_date( ).
    fill_comp_info(
      CHANGING
        cs_parts             = ls_parts
        cv_entityinformation = ls_ent_info-entitycontainer-entityinformation ).

    e_params = gs_params.

    IF gs_params-vdtys IS NOT INITIAL.
      ls_ent_info-entitycontainer-identifierauthoritycode = 'VERGI_DETAYSIZ'.
    ENDIF.

    ls_ent_info-entitycontainer-qualifierentry = 'standard'.

    IF gs_bukrs-tastar IS NOT INITIAL AND gs_bukrs-tastar NE '00000000'.
      CONCATENATE gs_bukrs-tastar+0(4) '-' gs_bukrs-tastar+4(2) '-' gs_bukrs-tastar+6(2) INTO ls_ent_info-entitycontainer-closedate.
    ENDIF.



  ENDMETHOD.