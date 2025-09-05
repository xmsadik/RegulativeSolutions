CLASS lhc_zetr_ddl_i_ledger_lastpage DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zetr_ddl_i_ledger_lastpage RESULT result.

    METHODS create_ledger FOR MODIFY
      IMPORTING keys FOR ACTION zetr_ddl_i_ledger_lastpage~create_ledger.

ENDCLASS.

CLASS lhc_zetr_ddl_i_ledger_lastpage IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create_ledger.

    TYPES: BEGIN OF ts_params,
             bukrs  TYPE bukrs,
             bcode  TYPE zetr_e_bcode,
             gjahr  TYPE gjahr,
             monat  TYPE monat,
             parno  TYPE zetr_e_edf_piece_no,
             eyevno TYPE zetr_e_edf_end_journal,
             elinen TYPE zetr_e_edf_end_item_no,
           END OF ts_params.

    DATA: is_params TYPE ts_params.

    DATA: ls_reported TYPE RESPONSE FOR REPORTED zetr_ddl_i_ledger_lastpage.

    DATA(ls_params) = VALUE #( keys[ 1 ]-%param DEFAULT VALUE #( ) ).

    " Simple validations for initial values
    IF ls_params-bukrs IS INITIAL.
      APPEND VALUE #( %msg = new_message_with_text(
                       severity = if_abap_behv_message=>severity-error
                       text     = 'Please fill Company Code' )
                     %element-bukrs = if_abap_behv=>mk-on )
        TO ls_reported-zetr_ddl_i_ledger_lastpage.
    ENDIF.

    IF ls_params-gjahr IS INITIAL.
      APPEND VALUE #( %msg = new_message_with_text(
                       severity = if_abap_behv_message=>severity-error
                       text     = 'Please fill Fiscal Year' )
                     %element-gjahr = if_abap_behv=>mk-on )
        TO ls_reported-zetr_ddl_i_ledger_lastpage.
    ENDIF.

    IF ls_params-monat IS INITIAL.
      APPEND VALUE #( %msg = new_message_with_text(
                       severity = if_abap_behv_message=>severity-error
                       text     = 'Please fill Fiscal Period' )
                     %element-monat = if_abap_behv=>mk-on )
        TO ls_reported-zetr_ddl_i_ledger_lastpage.
    ENDIF.

    IF ls_params-parno IS INITIAL.
      APPEND VALUE #( %msg = new_message_with_text(
                       severity = if_abap_behv_message=>severity-error
                       text     = 'Please fill Part No' )
                     %element-gjahr = if_abap_behv=>mk-on )
        TO ls_reported-zetr_ddl_i_ledger_lastpage.
    ENDIF.

    IF ls_params-eyevno IS INITIAL.
      APPEND VALUE #( %msg = new_message_with_text(
                       severity = if_abap_behv_message=>severity-error
                       text     = 'Please fill Beginning Yevno' )
                     %element-monat = if_abap_behv=>mk-on )
        TO ls_reported-zetr_ddl_i_ledger_lastpage.
    ENDIF.

    IF ls_params-elinen IS INITIAL.
      APPEND VALUE #( %msg = new_message_with_text(
                       severity = if_abap_behv_message=>severity-error
                       text     = 'Please fill End Item No' )
                     %element-monat = if_abap_behv=>mk-on )
        TO ls_reported-zetr_ddl_i_ledger_lastpage.
    ENDIF.


    " If any validation failed, return messages and exit
    IF ls_reported IS NOT INITIAL.
      reported = CORRESPONDING #( DEEP ls_reported ).
      RETURN.
    ENDIF.

    DATA(ledger_general) = NEW zcl_etr_ledger_general( ).

    TRY.

        is_params = CORRESPONDING #( DEEP ls_params ).

        ledger_general->process_mid_term_last(
          EXPORTING
            is_params = is_params
          RECEIVING
            rs_result = DATA(ls_result)
        ).

        CASE ls_result-type.
          WHEN 'E' OR 'S'.
            " Determine message severity based on type
            DATA(lv_severity) = COND #(
              WHEN ls_result-type = 'E'
              THEN if_abap_behv_message=>severity-error
              ELSE if_abap_behv_message=>severity-success ).

            " Append message with dynamic severity
            APPEND VALUE #(
              %msg = new_message_with_text(
                severity = lv_severity
                text     = ls_result-message )
              %element-bukrs = if_abap_behv=>mk-on
            ) TO ls_reported-zetr_ddl_i_ledger_lastpage.

          WHEN OTHERS.
            " Handle other cases if needed
        ENDCASE.

      CATCH cx_root INTO DATA(lx_error).
        " Handle any exceptions
        APPEND VALUE #( %msg = new_message_with_text(
                       severity = if_abap_behv_message=>severity-error
                       text     = lx_error->get_text( ) )
                     %element-bukrs = if_abap_behv=>mk-on )
          TO ls_reported-zetr_ddl_i_ledger_lastpage.
    ENDTRY.

    reported = CORRESPONDING #( DEEP ls_reported ).

  ENDMETHOD.

ENDCLASS.