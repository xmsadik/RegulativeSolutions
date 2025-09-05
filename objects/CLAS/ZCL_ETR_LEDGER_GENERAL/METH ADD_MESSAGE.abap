  METHOD add_message.
    DATA: ls_return TYPE bapiretc.

    ls_return-id = iv_msgid.
    ls_return-type = iv_msgty.
    ls_return-number = iv_msgno.
    ls_return-message_v1 = iv_msgv1.
    ls_return-message_v2 = iv_msgv2.

    " Generate message text
    MESSAGE ID ls_return-id
      TYPE 'S'  " Always use 'S' type for message collection
      NUMBER ls_return-number
      WITH ls_return-message_v1 ls_return-message_v2
           ls_return-message_v3 ls_return-message_v4
      INTO ls_return-message.

    " Append to the message table
    APPEND ls_return TO gt_return.
  ENDMETHOD.