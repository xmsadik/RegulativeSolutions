  METHOD generate_unit_matching.
    DATA: lt_match TYPE TABLE OF zetr_t_untmc.

    lt_match = VALUE #( ( meins = 'ST' unitc = 'C62' )
                        ( meins = 'G' unitc = 'GRM' )
                        ( meins = 'KG' unitc = 'KGM' )
                        ( meins = 'L' unitc = 'LTR' ) ).

    MODIFY zetr_t_untmc FROM TABLE @lt_match.
  ENDMETHOD.