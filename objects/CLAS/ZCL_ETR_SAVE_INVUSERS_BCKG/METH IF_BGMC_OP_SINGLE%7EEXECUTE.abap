  METHOD if_bgmc_op_single~execute.
    modify( ).
    cl_abap_tx=>save( ).
    save( ).
  ENDMETHOD.