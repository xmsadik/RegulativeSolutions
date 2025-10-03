// CONTRACT-SAFE BEHAVIOR DEFINITION
managed implementation in class ZBP_ETR_I_YEVMIYE_UPLOAD unique;
strict (2);

define behavior for ZETR_I_YEVMIYE_UPLOAD alias YevmiyeUpload
persistent table zetr_t_defky
lock master
authorization master (global)
etag master LastChangedAt
{
  create;
  update;
  delete;

  // CONTRACT SAFE: All database fields readonly except virtual ones
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

  // CONTRACT SAFE: Virtual fields can be modified
  field (features : instance)
    CSVContent,
    ChunkSize;

  // Actions and validations
  action uploadCSVFile result [1] $self;
  validation validateChunkSize on save { create; update; field ChunkSize; }

  // CONTRACT SAFE: Mapping only for persistent fields
  mapping for zetr_t_defky
  {
    Bukrs = bukrs;
    Belnr = belnr;
    Gjahr = gjahr;
    Docln = docln;
    Budat = budat;
    Linen = linen;
    Yevno = yevno;
    // Virtual fields (CSVContent, ChunkSize) do NOT have mapping
  }
}