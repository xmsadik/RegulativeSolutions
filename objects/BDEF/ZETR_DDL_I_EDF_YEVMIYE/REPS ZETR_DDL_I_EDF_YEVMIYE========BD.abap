managed implementation in class zbp_etr_ddl_i_edf_yevmiye unique;
strict ( 2 );

define behavior for ZETR_DDL_I_EDF_YEVMIYE alias YevmiyeKayit
persistent table zetr_t_defky
lock master
authorization master ( instance )
{
  update;

  /* Read-only alanlar - key alanlar değiştirilemez */
  field ( readonly ) Bukrs, Belnr, Gjahr, Docln, Budat, Linen;

  /* Sadece Yevno alanı güncellenebilir */
  field ( mandatory ) Yevno;

  /* Parametresiz action: frontend dosya upload'unu ayrı bir mekanizma ile yapıp
   bu action'ı tetikleyebilir. */
  action uploadCSVFile result [1] $self;

  mapping for zetr_t_defky
    {
      Bukrs   = bukrs;
      Belnr   = belnr;
      Gjahr   = gjahr;
      Docln   = docln;
      Budat   = budat;
      Linen   = linen;
      Yevno   = yevno;
    }
}