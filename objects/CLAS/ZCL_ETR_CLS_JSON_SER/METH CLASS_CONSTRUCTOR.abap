  METHOD class_constructor.
    cl_abap_string_utilities=>c2str_preserving_blanks(
    EXPORTING source = ': '
    IMPORTING dest   = c_colon ) .

    cl_abap_string_utilities=>c2str_preserving_blanks(
        EXPORTING source = ', '
        IMPORTING dest   = c_comma ) .
  ENDMETHOD.