CLASS lcl_buffer DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA:
      mt_docui_icinv TYPE RANGE OF sysuuid_c22,
      mt_docui_oginv TYPE RANGE OF sysuuid_c22,
      mt_docui_icdlv TYPE RANGE OF sysuuid_c22,
      mt_docui_ogdlv TYPE RANGE OF sysuuid_c22.
ENDCLASS.

CLASS lhc_DeleteEntries DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR DeleteEntries RESULT result.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE DeleteEntries.

    METHODS read FOR READ
      IMPORTING keys FOR READ DeleteEntries RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK DeleteEntries.

ENDCLASS.

CLASS lhc_DeleteEntries IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD delete.
    CLEAR:
      lcl_buffer=>mt_docui_icinv,
      lcl_buffer=>mt_docui_oginv,
      lcl_buffer=>mt_docui_icdlv,
      lcl_buffer=>mt_docui_ogdlv.

    lcl_buffer=>mt_docui_icinv = VALUE #( FOR wa IN keys WHERE ( tabnm = 'ICINV' ) ( sign = 'I' option = 'EQ' low = wa-docui ) ).
    lcl_buffer=>mt_docui_oginv = VALUE #( FOR wa IN keys WHERE ( tabnm = 'OGINV' ) ( sign = 'I' option = 'EQ' low = wa-docui ) ).
    lcl_buffer=>mt_docui_icdlv = VALUE #( FOR wa IN keys WHERE ( tabnm = 'ICDLV' ) ( sign = 'I' option = 'EQ' low = wa-docui ) ).
    lcl_buffer=>mt_docui_ogdlv = VALUE #( FOR wa IN keys WHERE ( tabnm = 'OGDLV' ) ( sign = 'I' option = 'EQ' low = wa-docui ) ).
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZETR_DDL_I_DELETE_TABLES DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZETR_DDL_I_DELETE_TABLES IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
    IF lcl_buffer=>mt_docui_icinv IS NOT INITIAL.
      DELETE FROM zetr_t_icinv WHERE docui IN @lcl_buffer=>mt_docui_icinv.
      DELETE FROM zetr_t_icini WHERE docui IN @lcl_buffer=>mt_docui_icinv.
      DELETE FROM zetr_t_arcd  WHERE docui IN @lcl_buffer=>mt_docui_icinv.
      DELETE FROM zetr_t_logs  WHERE docui IN @lcl_buffer=>mt_docui_icinv.

      reported-deleteentries = VALUE #( BASE reported-deleteentries FOR ls_docui_icinv IN lcl_buffer=>mt_docui_icinv ( %delete = if_abap_behv=>mk-on
                                                                                                                       docui = ls_docui_icinv-low
                                                                                                                       tabnm = 'ICINV'
                                                                                                                       %msg = new_message( id       = 'ZETR_COMMON'
                                                                                                                                           number   = '034'
                                                                                                                                           severity = if_abap_behv_message=>severity-success ) ) ).
    ENDIF.

    IF lcl_buffer=>mt_docui_oginv IS NOT INITIAL.
      DELETE FROM zetr_t_oginv WHERE docui IN @lcl_buffer=>mt_docui_oginv.
      DELETE FROM zetr_t_arcd  WHERE docui IN @lcl_buffer=>mt_docui_oginv.
      DELETE FROM zetr_t_logs  WHERE docui IN @lcl_buffer=>mt_docui_oginv.

      reported-deleteentries = VALUE #( BASE reported-deleteentries FOR ls_docui_oginv IN lcl_buffer=>mt_docui_oginv ( %delete = if_abap_behv=>mk-on
                                                                                                                       docui = ls_docui_oginv-low
                                                                                                                       tabnm = 'OGINV'
                                                                                                                       %msg = new_message( id       = 'ZETR_COMMON'
                                                                                                                                           number   = '034'
                                                                                                                                           severity = if_abap_behv_message=>severity-success ) ) ).
    ENDIF.

    IF lcl_buffer=>mt_docui_icdlv IS NOT INITIAL.
      DELETE FROM zetr_t_icdlv WHERE docui IN @lcl_buffer=>mt_docui_icdlv.
      DELETE FROM zetr_t_icdli WHERE docui IN @lcl_buffer=>mt_docui_icdlv.
      DELETE FROM zetr_t_arcd  WHERE docui IN @lcl_buffer=>mt_docui_icdlv.
      DELETE FROM zetr_t_logs  WHERE docui IN @lcl_buffer=>mt_docui_icdlv.

      reported-deleteentries = VALUE #( BASE reported-deleteentries FOR ls_docui_icdlv IN lcl_buffer=>mt_docui_icdlv ( %delete = if_abap_behv=>mk-on
                                                                                                                       docui = ls_docui_icdlv-low
                                                                                                                       tabnm = 'ICDLV'
                                                                                                                       %msg = new_message( id       = 'ZETR_COMMON'
                                                                                                                                           number   = '034'
                                                                                                                                           severity = if_abap_behv_message=>severity-success ) ) ).
    ENDIF.

    IF lcl_buffer=>mt_docui_ogdlv IS NOT INITIAL.
      DELETE FROM zetr_t_ogdlv WHERE docui IN @lcl_buffer=>mt_docui_ogdlv.
      DELETE FROM zetr_t_ogdli WHERE docui IN @lcl_buffer=>mt_docui_ogdlv.
      DELETE FROM zetr_t_odth  WHERE docui IN @lcl_buffer=>mt_docui_ogdlv.
      DELETE FROM zetr_t_odti  WHERE docui IN @lcl_buffer=>mt_docui_ogdlv.
      DELETE FROM zetr_t_arcd  WHERE docui IN @lcl_buffer=>mt_docui_ogdlv.
      DELETE FROM zetr_t_logs  WHERE docui IN @lcl_buffer=>mt_docui_ogdlv.

      reported-deleteentries = VALUE #( BASE reported-deleteentries FOR ls_docui_ogdlv IN lcl_buffer=>mt_docui_ogdlv ( %delete = if_abap_behv=>mk-on
                                                                                                                       docui = ls_docui_ogdlv-low
                                                                                                                       tabnm = 'OGDLV'
                                                                                                                       %msg = new_message( id       = 'ZETR_COMMON'
                                                                                                                                           number   = '034'
                                                                                                                                           severity = if_abap_behv_message=>severity-success ) ) ).
    ENDIF.
  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.