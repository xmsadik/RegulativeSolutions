CLASS lhc_zetr_ddl_i_delivery_serial DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS checkSerials FOR VALIDATE ON SAVE
      IMPORTING keys FOR zetr_ddl_i_delivery_serials~checkSerials.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zetr_ddl_i_delivery_serials RESULT result.

    METHODS createNumbers FOR MODIFY
      IMPORTING keys FOR ACTION zetr_ddl_i_delivery_serials~createNumbers RESULT result.

ENDCLASS.

CLASS lhc_zetr_ddl_i_delivery_serial IMPLEMENTATION.

  METHOD checkSerials.
    READ ENTITIES OF zetr_ddl_i_delivery_parameters IN LOCAL MODE
      ENTITY zetr_ddl_i_delivery_serials
      ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_serials).
    CHECK sy-subrc = 0.

    READ ENTITIES OF zetr_ddl_i_delivery_parameters IN LOCAL MODE
      ENTITY zetr_ddl_i_delivery_parameters
      ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_parameters).
    CHECK sy-subrc = 0.

    LOOP AT lt_serials INTO DATA(ls_serial).
      READ TABLE lt_parameters
        INTO DATA(ls_parameter)
        WITH TABLE KEY id
        COMPONENTS CompanyCode = ls_serial-CompanyCode.
      CHECK sy-subrc = 0.
      CASE ls_parameter-GenerateSerial.
        WHEN 'D'.
          IF strlen( ls_serial-NumberRangeNumber ) <> 1.
            APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                                number   = '214'
                                                severity = if_abap_behv_message=>severity-error ) ) TO reported-zetr_ddl_i_delivery_parameters.
          ENDIF.
        WHEN 'X'.
          IF strlen( ls_serial-NumberRangeNumber ) <> 2.
            APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                                number   = '215'
                                                severity = if_abap_behv_message=>severity-error ) ) TO reported-zetr_ddl_i_delivery_parameters.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD createNumbers.
    DATA: lt_numbers      TYPE TABLE OF zetr_ddl_i_delivery_numstat,
          lv_range_number TYPE c LENGTH 2,
          lv_index        TYPE n LENGTH 1.
    READ TABLE keys INTO DATA(ls_key) INDEX 1.
    IF sy-subrc <> 0 OR ls_key-%param-FiscalYear IS INITIAL.
      APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                          number   = '217'
                                          severity = if_abap_behv_message=>severity-error ) ) TO reported-zetr_ddl_i_delivery_parameters.
      RETURN.
    ENDIF.

    READ ENTITIES OF zetr_ddl_i_delivery_parameters IN LOCAL MODE
      ENTITY zetr_ddl_i_delivery_serials
      ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_serials).
    CHECK sy-subrc = 0.

    READ ENTITIES OF zetr_ddl_i_delivery_parameters IN LOCAL MODE
      ENTITY zetr_ddl_i_delivery_parameters
      ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_parameters).
    CHECK sy-subrc = 0.

    LOOP AT lt_serials INTO DATA(ls_serial).
      READ TABLE lt_parameters
        INTO DATA(ls_parameter)
        WITH TABLE KEY id
        COMPONENTS CompanyCode = ls_serial-CompanyCode.
      CHECK sy-subrc = 0.
      CASE ls_parameter-InternalNumbering.
        WHEN abap_false.
          APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                              number   = '216'
                                              severity = if_abap_behv_message=>severity-error
                                              v1 = ls_parameter-CompanyCode ) ) TO reported-zetr_ddl_i_delivery_parameters.
        WHEN abap_true.
          CASE ls_parameter-GenerateSerial.
            WHEN 'X'.
              SELECT SINGLE @abap_true
                FROM zetr_t_edocnum
                WHERE bukrs = @ls_parameter-CompanyCode
                  AND nrobj = 'ZETR_EDL'
                  AND serpr = @ls_serial-SerialPrefix
                  AND numrn = @ls_serial-NumberRangeNumber
                  AND gjahr = @ls_key-%param-FiscalYear
                INTO @DATA(lv_exists).
              IF lv_exists = abap_false.
                CLEAR lt_numbers.
                APPEND VALUE #( CompanyCode = ls_parameter-CompanyCode
                                SerialPrefix = ls_serial-SerialPrefix
                                NumberRangeObject = 'ZETR_EDL'
                                NumberRangeNumber = ls_serial-NumberRangeNumber
                                FiscalYear = ls_key-%param-FiscalYear ) TO lt_numbers.
              ENDIF.
            WHEN 'D'.
              CLEAR: lv_range_number, lv_index, lt_numbers.
              DO 9 TIMES.
                lv_range_number = ls_serial-NumberRangeNumber && lv_index.
                SELECT SINGLE @abap_true
                  FROM zetr_t_edocnum
                  WHERE bukrs = @ls_parameter-CompanyCode
                    AND nrobj = 'ZETR_EDL'
                    AND serpr = @ls_serial-SerialPrefix
                    AND numrn = @lv_range_number
                    AND gjahr = @ls_key-%param-FiscalYear
                  INTO @lv_exists.
                IF lv_exists = abap_false.
                  APPEND VALUE #( CompanyCode = ls_parameter-CompanyCode
                                  SerialPrefix = ls_serial-SerialPrefix
                                  NumberRangeObject = 'ZETR_EDL'
                                  NumberRangeNumber = lv_range_number
                                  FiscalYear = ls_key-%param-FiscalYear ) TO lt_numbers.
                ENDIF.
                lv_index += 1.
              ENDDO.
          ENDCASE.

          IF lt_numbers IS NOT INITIAL.
            MODIFY ENTITIES OF zetr_ddl_i_delivery_parameters IN LOCAL MODE
              ENTITY zetr_ddl_i_delivery_serials
                CREATE BY \_numberStatus
                FIELDS ( CompanyCode NumberRangeObject SerialPrefix NumberRangeNumber FiscalYear )
                AUTO FILL CID
                WITH VALUE #( ( CompanyCode = ls_parameter-CompanyCode SerialPrefix = ls_serial-SerialPrefix
                              %target = CORRESPONDING #( lt_numbers ) ) )
              FAILED failed.
          ENDIF.
          APPEND VALUE #( %msg = new_message( id       = 'ZETR_COMMON'
                                              number   = '218'
                                              severity = if_abap_behv_message=>severity-success
                                              v1 = ls_serial-SerialPrefix
                                              v2 = ls_key-%param-FiscalYear ) ) TO reported-zetr_ddl_i_delivery_parameters.
      ENDCASE.
    ENDLOOP.

    READ ENTITIES OF zetr_ddl_i_delivery_parameters IN LOCAL MODE
      ENTITY zetr_ddl_i_delivery_serials
      ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT lt_serials.
    result = VALUE #( FOR serial IN lt_serials ( %tky   = serial-%tky
                                                 %param = serial ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lhc_zetr_ddl_i_delivery_parame DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zetr_ddl_i_delivery_parameters RESULT result.

ENDCLASS.

CLASS lhc_zetr_ddl_i_delivery_parame IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

ENDCLASS.