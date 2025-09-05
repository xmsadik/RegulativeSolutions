CLASS lhc_zetr_ddL_i_purch_group_aut DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zetr_ddL_i_purch_group_auth RESULT result.

ENDCLASS.

CLASS lhc_zetr_ddL_i_purch_group_aut IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

ENDCLASS.