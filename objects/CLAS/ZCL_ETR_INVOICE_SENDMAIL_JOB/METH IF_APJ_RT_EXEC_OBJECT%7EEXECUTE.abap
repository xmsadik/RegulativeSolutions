  METHOD if_apj_rt_exec_object~execute.
    DATA: lt_datum_range TYPE RANGE OF datum,
          lt_bukrs_range TYPE RANGE OF bukrs,
          lt_docui_range TYPE RANGE OF sysuuid_c22,
          lt_stacd_range TYPE RANGE OF zetr_e_stacd.

    LOOP AT it_parameters INTO DATA(ls_parameter).
      CASE ls_parameter-selname.
        WHEN 'S_BUKRS'.
          APPEND INITIAL LINE TO lt_bukrs_range ASSIGNING FIELD-SYMBOL(<ls_bukrs>).
          <ls_bukrs> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_DATUM'.
          APPEND INITIAL LINE TO lt_datum_range ASSIGNING FIELD-SYMBOL(<ls_datum>).
          <ls_datum> = CORRESPONDING #( ls_parameter ).
        WHEN 'S_DOCUI'.
          APPEND INITIAL LINE TO lt_docui_range ASSIGNING FIELD-SYMBOL(<ls_docui>).
          <ls_docui> = CORRESPONDING #( ls_parameter ).
      ENDCASE.
    ENDLOOP.

    IF lt_datum_range IS INITIAL AND lt_docui_range IS INITIAL.
      APPEND INITIAL LINE TO lt_datum_range ASSIGNING <ls_datum>.
      <ls_datum>-sign = 'I'.
      <ls_datum>-option = 'BT'.
      <ls_datum>-low = cl_abap_context_info=>get_system_date( ) - 10.
      <ls_datum>-high = cl_abap_context_info=>get_system_date( ).
    ENDIF.

    APPEND INITIAL LINE TO lt_stacd_range ASSIGNING FIELD-SYMBOL(<ls_stacd>).
    <ls_stacd>-sign = 'E'.
    <ls_stacd>-option = 'EQ'.
    <ls_stacd>-low = ''.
    APPEND INITIAL LINE TO lt_stacd_range ASSIGNING <ls_stacd>.
    <ls_stacd>-sign = 'E'.
    <ls_stacd>-option = 'EQ'.
    <ls_stacd>-low = '2'.
    APPEND INITIAL LINE TO lt_stacd_range ASSIGNING <ls_stacd>.
    <ls_stacd>-sign = 'E'.
    <ls_stacd>-option = 'EQ'.
    <ls_stacd>-low = '1'.

    TRY.
        DATA(lo_log) = cl_bali_log=>create_with_header( cl_bali_header_setter=>create( object = 'ZETR_ALO_REGULATIVE'
                                                                                       subobject = 'INVOICE_UPDOUT_JOB' ) ).
        LOOP AT lt_bukrs_range ASSIGNING <ls_bukrs>.
          DATA(lo_free_text) = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                                 text     = 'Parameter : Company Code->' &&
                                                                            <ls_bukrs>-sign &&
                                                                            <ls_bukrs>-option &&
                                                                            <ls_bukrs>-low &&
                                                                            <ls_bukrs>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_datum_range ASSIGNING <ls_datum>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Date->' &&
                                                                      <ls_datum>-sign &&
                                                                      <ls_datum>-option &&
                                                                      <ls_datum>-low &&
                                                                      <ls_datum>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_docui_range ASSIGNING <ls_docui>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : UUID->' &&
                                                                      <ls_docui>-sign &&
                                                                      <ls_docui>-option &&
                                                                      <ls_docui>-low &&
                                                                      <ls_docui>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        LOOP AT lt_stacd_range ASSIGNING <ls_stacd>.
          lo_free_text = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_information
                                                           text     = 'Parameter : Status->' &&
                                                                      <ls_stacd>-sign &&
                                                                      <ls_stacd>-option &&
                                                                      <ls_stacd>-low &&
                                                                      <ls_stacd>-high ).
          lo_log->add_item( lo_free_text ).
        ENDLOOP.
        SELECT *
          FROM zetr_ddl_i_outgoing_invoices
          WHERE DocumentUUID IN @lt_docui_range
            AND CompanyCode IN @lt_bukrs_range
            AND StatusCode IN @lt_stacd_range
            AND SendDate IN @lt_datum_range
          INTO TABLE @DATA(lt_documents).
        IF sy-subrc = 0.
          DATA(lt_return) = send_mail_to_partner( CORRESPONDING #( lt_documents ) ).
          CLEAR lt_docui_range.
          LOOP AT lt_return INTO DATA(ls_return).
            IF ls_return-id IS NOT INITIAL.
              DATA(lo_message) = cl_bali_message_setter=>create( severity = ls_return-type
                                                                 id = ls_return-id
                                                                 number = ls_return-number
                                                                 variable_1 = ls_return-message_v1
                                                                 variable_2 = ls_return-message_v2
                                                                 variable_3 = ls_return-message_v3
                                                                 variable_4 = ls_return-message_v4 ).
              lo_log->add_item( lo_message ).
            ELSEIF ls_return-emailsent = abap_true.
              APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_return-documentuuid ) TO lt_docui_range.
            ENDIF.
          ENDLOOP.
          IF lt_docui_range IS NOT INITIAL.
            UPDATE zetr_T_oginv
              SET emsnd = @abap_true
              WHERE docui IN @lt_docui_range.
            zcl_etr_regulative_log=>create( it_logs = VALUE #( FOR ls_return_for IN lt_return WHERE ( emailsent = abap_true )
                                                               ( docui = ls_return_for-documentuuid
                                                                 uname = sy-uname
                                                                 datum = cl_abap_context_info=>get_system_date( )
                                                                 uzeit = cl_abap_context_info=>get_system_time( )
                                                                 logcd = zcl_etr_regulative_log=>mc_log_codes-mail ) ) ).
          ENDIF.
        ELSE.
          lo_message = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_information
                                                       id = 'ZETR_COMMON'
                                                       number = '005' ).
          lo_log->add_item( lo_message ).
        ENDIF.

        cl_bali_log_db=>get_instance( )->save_log( log = lo_log assign_to_current_appl_job = abap_true ).
      CATCH cx_bali_runtime.
    ENDTRY.
  ENDMETHOD.