  METHOD if_oo_adt_classrun~main.
    SELECT SINGLE *
      FROM zetr_t_edpar
      WHERE wsusr <> ''
      INTO @DATA(ls_parameter).
    CHECK sy-subrc = 0.
    TRY.
        DATA(lo_delivery_operations) = zcl_etr_delivery_operations=>factory( iv_company = ls_parameter-bukrs ).
        DATA(lt_list) = lo_delivery_operations->update_edelivery_users( iv_db_write = abap_true ).
        out->write( |Total Number of Users Updated : "{ lines( lt_list ) }"| ).
      CATCH zcx_etr_regulative_exception INTO DATA(lx_regulative_exception).
        DATA(lv_message) = lx_regulative_exception->get_text( ).
        out->write( |Error Occured : "{ lv_message }"| ).
    ENDTRY.
  ENDMETHOD.