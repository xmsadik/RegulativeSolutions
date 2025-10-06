managed implementation in class zbp_csv_upload unique;
// strict ( 2 );  ⭐ STRICT KALDIRILDI!

define behavior for ZETR_I_CSV_UPLOAD alias CSVUpload
persistent table zetr_t_csvupload
lock master
// ⭐ AUTHORIZATION YOK!
early numbering
{
  create;
  update;
  delete;

  determination setUserAndDefaults on modify { create; }
  determination processUploadedFile on modify { field Attachment; }

  validation validateChunkSize on save { field ChunkSize; }

  action processcsvfile result [1] $self;

  field ( readonly : update ) UploadId;
  field ( readonly ) CreatedBy, CreatedAt, LastChangedBy, LastChangedAt;
  field ( mandatory ) EndUser;

  mapping for zetr_t_csvupload {
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