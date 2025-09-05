  METHOD deserialize_object.
    DATA:
      l_node_type TYPE REF TO cl_abap_typedescr,
      l_ref       TYPE REF TO object.

    offset += 1. "skip {

    l_node_type = cl_abap_typedescr=>describe_by_data( node ) .

* prepare for dynamic access
    CASE l_node_type->kind .
      WHEN cl_abap_typedescr=>kind_ref .
        l_ref = node .
      WHEN cl_abap_typedescr=>kind_struct .

      WHEN OTHERS .
        EXIT.
        RAISE EXCEPTION TYPE cx_st_serialization_error.
    ENDCASE .

    DATA:
      l_done TYPE c LENGTH 1,
      l_len  TYPE i,
      l_name TYPE string.

* handle each component
    WHILE l_done = abap_false .
      "find next key
*      FIND REGEX '"(\w+)\s*":' IN SECTION OFFSET offset OF json
*        MATCH OFFSET offset MATCH LENGTH l_len
*        SUBMATCHES l_name .
      FIND PCRE '"(\w+)\s*":' IN SECTION OFFSET offset OF json
          MATCH OFFSET offset MATCH LENGTH l_len
          SUBMATCHES l_name.
      IF sy-subrc <> 0 .
        l_done = abap_true .
        CONTINUE.
        RAISE EXCEPTION TYPE cx_st_serialization_error.
      ENDIF .
      offset += l_len.

      FIELD-SYMBOLS <comp> TYPE any .

*   dynamic binding to component
      TRANSLATE l_name TO UPPER CASE .
      CASE l_node_type->kind .
        WHEN cl_abap_typedescr=>kind_ref .
          ASSIGN l_ref->(l_name) TO <comp> .
        WHEN cl_abap_typedescr=>kind_struct .
          ASSIGN COMPONENT l_name OF STRUCTURE node TO <comp> .
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
        WHEN OTHERS .
          RAISE EXCEPTION TYPE cx_st_serialization_error.
      ENDCASE .

      DATA:
        l_comp_type TYPE REF TO cl_abap_typedescr,
        l_ref_type  TYPE REF TO cl_abap_refdescr.

*   check component type
      l_comp_type = cl_abap_typedescr=>describe_by_data( <comp> ) .
      CASE l_comp_type->kind .
*     create instance if it's an oref
        WHEN cl_abap_typedescr=>kind_ref .
          l_ref_type ?= l_comp_type .
          l_comp_type = l_ref_type->get_referenced_type( ) .
          CREATE OBJECT <comp> TYPE (l_comp_type->absolute_name).
      ENDCASE .

*   deserialize current component
      deserialize_node(
        EXPORTING
          json = json
        CHANGING
          offset = offset
          node = <comp> ) .

      FIND PCRE ',|\}' IN SECTION OFFSET offset OF json MATCH OFFSET offset.
      IF sy-subrc <> 0 .
        RAISE EXCEPTION TYPE cx_st_serialization_error.
      ENDIF .

      IF json+offset(1) = '}' .
        l_done = abap_true .
      ENDIF .
      offset += l_len. .
    ENDWHILE .

  ENDMETHOD.