  METHOD incoming_invoice_summary.
    DATA: lt_summary1 TYPE STANDARD TABLE OF mty_summary1 WITH NON-UNIQUE SORTED KEY by_type COMPONENTS type,
          ls_sumamry1 TYPE mty_summary1,
          lt_summary2 TYPE STANDARD TABLE OF mty_summary2 WITH NON-UNIQUE SORTED KEY by_type COMPONENTS type,
          ls_sumamry2 TYPE mty_summary2,
          lt_summary3 TYPE STANDARD TABLE OF mty_summary3 WITH NON-UNIQUE SORTED KEY by_type COMPONENTS type,
          ls_sumamry3 TYPE mty_summary3.
    CLEAR rt_return.
    LOOP AT it_documents INTO DATA(ls_document).
      CLEAR ls_sumamry1.
      ls_sumamry1-type = zcl_etr_invoice_operations=>conversion_profile_id_output( CONV #( ls_document-ProfileID ) ).
      ls_sumamry1-count = 1.
      COLLECT ls_sumamry1 INTO lt_summary1.

      CLEAR ls_sumamry2.
      ls_sumamry2-type = zcl_etr_invoice_operations=>conversion_profile_id_output( CONV #( ls_document-ProfileID ) ).
      ls_sumamry2-amount = ls_document-amount.
      ls_sumamry2-taxamount = ls_document-taxamount.
      ls_sumamry2-currency = ls_document-currency.
      COLLECT ls_sumamry2 INTO lt_summary2.

      CLEAR ls_sumamry3.
      ls_sumamry3-type = zcl_etr_invoice_operations=>conversion_profile_id_output( CONV #( ls_document-ProfileID ) ).
      ls_sumamry3-status = ls_document-responsetext.
      ls_sumamry3-count = 1.
      COLLECT ls_sumamry3 INTO lt_summary3.
    ENDLOOP.

    LOOP AT lt_summary1 INTO ls_sumamry1.
      APPEND VALUE #( type = if_abap_behv_message=>severity-information
                      id = 'ZETR_COMMON'
                      number = '000'
                      message_v1 = '* * * * * * * *'
                      message_v2 = '* * * * * * * *'
                      message_v3 = '* * * * * * * *'
                      message_v4 = '* * * * * * * *' ) TO rt_return.
      LOOP AT lt_summary3 INTO ls_sumamry3 USING KEY by_type WHERE type = ls_sumamry1-type.
        APPEND VALUE #( type = if_abap_behv_message=>severity-information
                        id = 'ZETR_COMMON'
                        number = '228'
                        message_v1 = ls_sumamry3-status
                        message_v2 = |{ ls_sumamry3-count NUMBER = USER }| ) TO rt_return.
      ENDLOOP.

      APPEND VALUE #( type = if_abap_behv_message=>severity-information
                      id = 'ZETR_COMMON'
                      number = '225'
                      message_v1 = |{ ls_sumamry1-count NUMBER = USER }| ) TO rt_return.

      LOOP AT lt_summary2 INTO ls_sumamry2 USING KEY by_type WHERE type = ls_sumamry1-type.
        APPEND VALUE #( type = if_abap_behv_message=>severity-information
                        id = 'ZETR_COMMON'
                        number = '224'
                        message_v1 = |{ ls_sumamry2-taxamount CURRENCY = ls_sumamry2-Currency NUMBER = USER }|
                        message_v2 = ls_sumamry2-Currency ) TO rt_return.
        APPEND VALUE #( type = if_abap_behv_message=>severity-information
                        id = 'ZETR_COMMON'
                        number = '223'
                        message_v1 = |{ ls_sumamry2-amount CURRENCY = ls_sumamry2-Currency NUMBER = USER }|
                        message_v2 = ls_sumamry2-Currency ) TO rt_return.
      ENDLOOP.

      APPEND VALUE #( type = if_abap_behv_message=>severity-information
                      id = 'ZETR_COMMON'
                      number = '227'
                      message_v1 = ls_sumamry1-type ) TO rt_return.
    ENDLOOP.
  ENDMETHOD.