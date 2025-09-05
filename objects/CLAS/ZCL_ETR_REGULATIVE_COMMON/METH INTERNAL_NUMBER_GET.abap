  METHOD internal_number_get.
    SELECT SINGLE *
      FROM zetr_t_edocnum
      WHERE bukrs = @iv_company
        AND nrobj = @iv_object
        AND serpr = @iv_serial
        AND numrn = @iv_range_number
        AND gjahr = @iv_year
      INTO @DATA(ls_number).
    IF sy-subrc <> 0.
      ls_number = VALUE #( bukrs = iv_company
                           nrobj = iv_object
                           serpr = iv_serial
                           numrn = iv_range_number
                           gjahr = iv_year ).
      INSERT zetr_t_edocnum FROM @ls_number.
*      COMMIT WORK AND WAIT.
    ENDIF.

    TRY.
        DATA(lo_lock) = cl_abap_lock_object_factory=>get_instance( iv_name = 'EZTR0_LO_NUMBER' ).
        DO 10 TIMES.
          DATA(lv_index) = sy-index.
          TRY.
              lo_lock->enqueue( it_parameter = VALUE #( ( name = 'BUKRS' value = REF #( iv_company ) )
                                                        ( name = 'NROBJ' value = REF #( iv_object ) )
                                                        ( name = 'SERPR' value = REF #( iv_serial ) )
                                                        ( name = 'NUMRN' value = REF #( iv_range_number ) )
                                                        ( name = 'GJAHR' value = REF #( iv_year ) ) ) ).
              EXIT.
            CATCH cx_abap_foreign_lock INTO DATA(lo_foreign_lock).
              IF lv_index < 10.
                WAIT UP TO 1 SECONDS.
              ELSE.
                RAISE EXCEPTION TYPE zcx_etr_regulative_exception
                  EXPORTING
                    previous = lo_foreign_lock.
              ENDIF.
          ENDTRY.
        ENDDO.
        ls_number-numst += 1.
        rv_number = ls_number-numst.
        UPDATE zetr_t_edocnum FROM @ls_number.
*        COMMIT WORK AND WAIT.
        lo_lock->dequeue( it_parameter = VALUE #( ( name = 'BUKRS' value = REF #( iv_company ) )
                                                  ( name = 'NROBJ' value = REF #( iv_object ) )
                                                  ( name = 'SERPR' value = REF #( iv_serial ) )
                                                  ( name = 'NUMRN' value = REF #( iv_range_number ) )
                                                  ( name = 'GJAHR' value = REF #( iv_year ) ) ) ).
      CATCH cx_abap_lock_failure INTO DATA(lo_lock_failure).
        RAISE EXCEPTION TYPE zcx_etr_regulative_exception
          EXPORTING
            previous = lo_lock_failure.
    ENDTRY.
  ENDMETHOD.