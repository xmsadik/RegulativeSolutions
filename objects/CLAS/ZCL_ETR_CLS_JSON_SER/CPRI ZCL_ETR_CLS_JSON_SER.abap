  PRIVATE SECTION.

    DATA fragments TYPE zetr_tt_string.
    DATA data_ref TYPE REF TO data .
    CLASS-DATA c_colon TYPE zetr_e_string .
    CLASS-DATA c_comma TYPE zetr_e_string .

    METHODS recurse
      IMPORTING
        !data TYPE any
        !name TYPE zetr_e_char30 OPTIONAL .