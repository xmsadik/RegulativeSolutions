managed implementation in class ZBP_CSV_UPLOAD unique;
strict ( 2 );

define behavior for ZETR_I_CSV_UPLOAD alias csvupload
persistent table zetr_t_csvupload
authorization master ( global )
lock master
//etag master LastChangedAt  // ⭐ ETAG ekleyin
{
  // ⭐ CREATE için field kontrolü
  field ( numbering : managed  )
  UploadId;

  field ( readonly )
  CreatedAt, CreatedBy, LastChangedAt, LastChangedBy;

  field ( readonly : update )
  UploadId, EndUser;

  // ⭐ CRUD operasyonları - açıkça tanımlayın
  create;
  update;
  delete;

  determination setuseranddefaults on modify { create; }
  determination processuploadedfile on modify { field Attachment; }
  validation validatechunksize on save { create; update; }

  action processcsvfile result [1] $self;

  mapping for zetr_t_csvupload
  {
    UploadId = upload_id;
    EndUser = end_user;
    Status = status;
    Attachment = attachment;
    MimeType = mimetype;
    Filename = filename;
    ChunkSize = chunk_size;
    CreatedBy = created_by;
    CreatedAt = created_at;
    LastChangedBy = last_changed_by;
    LastChangedAt = last_changed_at;
  }
}