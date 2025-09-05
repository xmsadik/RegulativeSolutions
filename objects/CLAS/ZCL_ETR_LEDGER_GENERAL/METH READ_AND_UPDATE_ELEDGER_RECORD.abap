  METHOD read_and_update_eledger_record.

    " Metodun başlangıcında dönüş parametresini varsayılan olarak 'başarısız' (abap_false) ayarlayalım.
    rv_is_updated = abap_false.

    "**********************************************************************
    "* 1. ADIM: KAYDI OKU (READ)
    "**********************************************************************
    " CHANGING parametresi olarak gelen cs_record yapısını, veritabanından
    " okunan yeni verilerle doldurmadan önce temizlemek iyi bir pratiktir.
    CLEAR cs_record.

    " Verilen anahtar alanlara (şirket kodu, mali yıl, ay) göre
    " ilgili kaydı veritabanından oku ve cs_record'a yaz.
    SELECT SINGLE *
      FROM zetr_t_defcl
      WHERE bukrs = @iv_bukrs
        AND gjahr = @iv_gjahr
        AND monat = @iv_monat
      INTO @cs_record.

    " Eğer SELECT ifadesi bir sonuç bulamazsa (sy-subrc <> 0),
    " bu, güncellenecek bir kayıt olmadığı anlamına gelir.
    " Bu durumda metottan hemen çıkılır. rv_is_updated 'false' kalır.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    "**********************************************************************
    "* ARA ADIM: VERİYİ MANİPÜLE ETME (MANIPULATE)
    "**********************************************************************
    " Bu noktada, metodu çağıran kod bloğuna geri dönülür.
    " 'cs_record' artık veritabanından gelen verilerle doludur.
    " Metodu çağıran program, bu yapıdaki (cs_record) alanları
    " istediği gibi değiştirebilir. Debug modunda bu değişiklikleri
    " elle yapabilirsiniz.
    "
    " Örneğin, çağıran kodda şu satırlar olabilir:
    " cs_record-heror = abap_true.
    " cs_record-manup = abap_false.
    "**********************************************************************


    "**********************************************************************
    "* 2. ADIM: KAYDI GÜNCELLE (WRITE)
    "**********************************************************************
    " Değiştirilmiş verileri içeren cs_record yapısını kullanarak
    " veritabanı tablosunu güncelle. UPDATE ... FROM ifadesi,
    " yapıdaki anahtar alanlarını WHERE koşulu için, diğer alanları
    " ise SET ifadesi için otomatik olarak kullanır. Bu, modern ve
    " güvenli bir güncelleme yöntemidir.
    UPDATE zetr_t_defcl FROM @cs_record.

    " UPDATE ... FROM ifadesi başarılı olduğunda sy-subrc 0 değerini alır.
    " Bir veritabanı hatası (örn. kilitlenme) durumunda ise 0'dan farklı bir
    " değer alır.
    IF sy-subrc = 0.
      " Güncelleme başarılı olduysa, dönüş parametresini 'başarılı' (abap_true) yap.
      rv_is_updated = abap_true.
    ELSE.
      " Güncelleme başarısız olursa, rv_is_updated 'false' olarak kalır.
      rv_is_updated = abap_false.
    ENDIF.

  ENDMETHOD.