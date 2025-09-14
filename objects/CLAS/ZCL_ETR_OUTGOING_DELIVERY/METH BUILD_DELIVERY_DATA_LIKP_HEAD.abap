  METHOD build_delivery_data_likp_head.
*    IF ms_outdel_data-likp-wbstk <> 'C' AND ms_outdel_data-likp-wbstk <> ''.
*      RAISE EXCEPTION TYPE zcx_etr_regulative_exception
*        MESSAGE e230(zetr_common).
*    ENDIF.
    CONVERT DATE ms_outdel_data-likp-erdat TIME ms_outdel_data-likp-erzet INTO TIME STAMP DATA(lv_timestamp) TIME ZONE 'UTC'.
    DATA(lv_timestamp_text) = CONV zetr_e_descr100( |{ lv_timestamp  TIMESTAMP = ISO TIMEZONE = 'UTC+3' }| ).
    ms_delivery_ubl-issuedate-content = lv_timestamp_text(10).
    ms_delivery_ubl-issuetime-content = lv_timestamp_text+11(8).
*    CONCATENATE ms_outdel_data-likp-erdat+0(4)
*                ms_outdel_data-likp-erdat+4(2)
*                ms_outdel_data-likp-erdat+6(2)
*      INTO ms_delivery_ubl-issuedate-content
*      SEPARATED BY '-'.
*
*    CONCATENATE ms_outdel_data-likp-erzet+0(2)
*                ms_outdel_data-likp-erzet+2(2)
*                ms_outdel_data-likp-erzet+4(2)
*                INTO ms_delivery_ubl-issuetime-content
*                SEPARATED BY ':'.
*    build_delivery_data_common_hdr( ).
  ENDMETHOD.