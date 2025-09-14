  METHOD build_delivery_data_manu_head.
*    build_delivery_data_common_hdr( ).
    CONVERT DATE ms_manual_data-head-erdat TIME ms_manual_data-head-erzet INTO TIME STAMP DATA(lv_timestamp) TIME ZONE 'UTC'.
    DATA(lv_timestamp_text) = CONV zetr_e_descr100( |{ lv_timestamp  TIMESTAMP = ISO TIMEZONE = 'UTC+3' }| ).
    ms_delivery_ubl-issuedate-content = lv_timestamp_text(10).
    ms_delivery_ubl-issuetime-content = lv_timestamp_text+11(8).
*    CONCATENATE ms_manual_data-head-erdat+0(4)
*                ms_manual_data-head-erdat+4(2)
*                ms_manual_data-head-erdat+6(2)
*      INTO ms_delivery_ubl-issuedate-content
*      SEPARATED BY '-'.
*
*    CONCATENATE ms_manual_data-head-erzet+0(2)
*                ms_manual_data-head-erzet+2(2)
*                ms_manual_data-head-erzet+4(2)
*                INTO ms_delivery_ubl-issuetime-content
*                SEPARATED BY ':'.
  ENDMETHOD.