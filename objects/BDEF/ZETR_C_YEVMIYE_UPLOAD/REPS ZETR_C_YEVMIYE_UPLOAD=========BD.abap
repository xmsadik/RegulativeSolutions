
projection;
strict (2);

define behavior for ZETR_C_YEVMIYE_UPLOAD alias YevmiyeUpload
{
  use create;
  use update;
  use delete;

  use action uploadCSVFile;

  field (readonly)
    Bukrs,
    Belnr,
    Gjahr,
    Docln,
    Budat,
    Linen,
    Yevno,
    CreatedBy,
    CreatedAt,
    LastChangedBy,
    LastChangedAt;
}