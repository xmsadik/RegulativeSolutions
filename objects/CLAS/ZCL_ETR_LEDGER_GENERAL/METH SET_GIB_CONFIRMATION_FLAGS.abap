  METHOD set_gib_confirmation_flags.

    " Varsayılan olarak dönüş değerini 'başarısız' (false) olarak ayarlıyoruz.
    rv_is_updated = abap_false.

    UPDATE zetr_t_oldef SET
        gbyok = @abap_true,
        gbkok = @abap_true
      WHERE bukrs = @iv_bukrs
        AND bcode = @iv_bcode
        AND gjahr = @iv_gjahr
        AND monat = @iv_monat.

    " sy-subrc kontrolü:
    " sy-subrc = 0 ise, kayıt başarıyla güncellendi.
    " sy-subrc = 4 ise, WHERE koşuluna uyan kayıt bulunamadı.
    IF sy-subrc = 0.
      " Güncelleme başarılı oldu.
      rv_is_updated = abap_true.
    ELSE.
      " Güncelleme yapılamadı (kayıt bulunamadı veya bir veritabanı hatası oluştu).
      rv_is_updated = abap_false.
    ENDIF.


  ENDMETHOD.