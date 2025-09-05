  METHOD get_domain_values.
*    TYPES:
*      BEGIN OF ty_domain_value,
*        value_low  TYPE c LENGTH 10,
*        value_text TYPE c LENGTH 60,
*      END OF ty_domain_value,
*      tt_domain_values TYPE STANDARD TABLE OF ty_domain_value WITH KEY value_low.
*
*    DATA:
*      lt_domain_values TYPE tt_domain_values,
*      lo_domain_type   TYPE REF TO cl_abap_elemdescr,
*      lo_domain        TYPE REF TO cl_tpda_script_data_descr.
*
*    TRY.
*        "Get domain fixed values using RTTI
*        cl_abap_typedescr=>describe_by_name(
*          EXPORTING
*            p_name         = 'ZETR_BELGE_TUR_ACK'
*          RECEIVING
*            p_descr_ref   = DATA(lo_domain_type2)
*          EXCEPTIONS
*            type_not_found = 1 ).
*
*        IF sy-subrc <> 0.
**          RAISE EXCEPTION TYPE zcx_etr_edf_static_check
**            EXPORTING
**              textid = VALUE #( msgid = 'ZETR_EDF'
**                              msgno = 001 )
**              text   = |Domain /ITETR/EDF_BELGE_TUR_ACK not found|.
*        ENDIF.
*
*        "Get fixed values
*        DATA(lt_fixed_values) = CAST cl_abap_elemdescr( lo_domain_type2 )->get_ddic_fixed_values( ).
*
*        "Sort the values
*        SORT lt_fixed_values BY low.
*
*        "Convert to target format if needed
*        LOOP AT lt_fixed_values ASSIGNING FIELD-SYMBOL(<ls_fixed_value>).
*          APPEND VALUE #(
*            value_low  = <ls_fixed_value>-low
*            value_text = <ls_fixed_value>-ddtext
*          ) TO lt_domain_values.
*        ENDLOOP.
*
*      CATCH cx_root INTO DATA(lx_root).
*        RAISE EXCEPTION TYPE zcx_etr_edf_static_check
*          EXPORTING
*            textid   = VALUE #( msgid = 'ZETR_EDF'
*                              msgno = 002 )
*            text     = |Error getting domain values: { lx_root->get_text( ) }|
*            previous = lx_root.
*    ENDTRY.
*
*    "Return the sorted values
*    rt_domain_values = lt_domain_values.
  ENDMETHOD.