  METHOD build_delivery_data_mkpf_head.
*    build_delivery_data_common_hdr( ).
    CONVERT DATE ms_goodsmvmt_data-mkpf-erdat TIME ms_goodsmvmt_data-mkpf-erzet INTO TIME STAMP DATA(lv_timestamp) TIME ZONE 'UTC'.
    DATA(lv_timestamp_text) = CONV zetr_e_descr100( |{ lv_timestamp  TIMESTAMP = ISO TIMEZONE = 'UTC+3' }| ).
    ms_delivery_ubl-issuedate-content = lv_timestamp_text(10).
    ms_delivery_ubl-issuetime-content = lv_timestamp_text+11(8).
*    CONCATENATE ms_goodsmvmt_data-mkpf-erdat+0(4)
*                ms_goodsmvmt_data-mkpf-erdat+4(2)
*                ms_goodsmvmt_data-mkpf-erdat+6(2)
*      INTO ms_delivery_ubl-issuedate-content
*      SEPARATED BY '-'.
*
*    CONCATENATE ms_goodsmvmt_data-mkpf-erzet+0(2)
*                ms_goodsmvmt_data-mkpf-erzet+2(2)
*                ms_goodsmvmt_data-mkpf-erzet+4(2)
*                INTO ms_delivery_ubl-issuetime-content
*                SEPARATED BY ':'.
  ENDMETHOD.