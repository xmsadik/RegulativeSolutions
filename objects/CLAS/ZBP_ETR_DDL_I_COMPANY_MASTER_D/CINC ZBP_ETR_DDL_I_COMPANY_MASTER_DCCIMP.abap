CLASS lhc_zetr_ddl_i_company_master_ DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR CompanyMaster RESULT result.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR CompanyMaster RESULT result.

    METHODS generateData FOR MODIFY
      IMPORTING keys FOR ACTION CompanyMaster~generateData RESULT result.

ENDCLASS.

CLASS lhc_zetr_ddl_i_company_master_ IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD generateData.
    READ ENTITIES OF zetr_ddl_i_company_master_data IN LOCAL MODE
        ENTITY CompanyMaster
        ALL FIELDS WITH
        CORRESPONDING #( keys )
        RESULT DATA(MasterData).
    CHECK sy-subrc = 0 AND MasterData IS NOT INITIAL.
    LOOP AT MasterData INTO DATA(DataLine).
      CASE DataLine-CompanyCode.
        WHEN '*GIB'.
          zcl_etr_regulative_common=>generate_unit_codes( ).
          zcl_etr_regulative_common=>generate_unit_matching( ).
          zcl_etr_regulative_common=>generate_transport_codes( ).
          zcl_etr_regulative_common=>generate_transport_matching( ).
          zcl_etr_regulative_common=>generate_status_codes( ).
          zcl_etr_regulative_common=>generate_tax_codes( ).
          zcl_etr_regulative_common=>generate_tax_exemption_codes( ).
          zcl_etr_regulative_common=>generate_tax_matching( ).
          zcl_etr_regulative_common=>generate_essential_partners( ).
          APPEND VALUE #( CompanyCode = DataLine-CompanyCode
                          %msg = new_message( id       = 'ZETR_COMMON'
                                              number   = '000'
                                              severity = if_abap_behv_message=>severity-success
                                              v1 = 'GİB Data Generated' ) ) TO reported-companymaster.
        WHEN OTHERS.
          SELECT SINGLE *
            FROM zetr_t_cmpin
            WHERE bukrs = @DataLine-CompanyCode
            INTO @DATA(ls_cmpin).
          IF sy-subrc = 0.
            APPEND VALUE #( CompanyCode = DataLine-CompanyCode
                            %msg = new_message( id       = 'ZETR_COMMON'
                                                number   = '000'
                                                severity = if_abap_behv_message=>severity-warning
                                                v1 = 'Company Master Data Already Exists' ) ) TO reported-companymaster.
          ELSE.
            zcl_etr_regulative_common=>generate_company_data( DataLine-CompanyCode ).
            APPEND VALUE #( CompanyCode = DataLine-CompanyCode
                            %msg = new_message( id       = 'ZETR_COMMON'
                                                number   = '000'
                                                severity = if_abap_behv_message=>severity-success
                                                v1 = 'Company Master Data Saved' ) ) TO reported-companymaster.
          ENDIF.
      ENDCASE.
    ENDLOOP.

    READ ENTITIES OF zetr_ddl_i_company_master_data IN LOCAL MODE
      ENTITY CompanyMaster
      ALL FIELDS WITH
      CORRESPONDING #( keys )
      RESULT masterdata.

    result = VALUE #( FOR masterline IN masterdata
             ( %tky   = masterline-%tky
               %param = masterline ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZETR_DDL_I_COMPANY_MASTER_ DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZETR_DDL_I_COMPANY_MASTER_ IMPLEMENTATION.

  METHOD save_modified.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.