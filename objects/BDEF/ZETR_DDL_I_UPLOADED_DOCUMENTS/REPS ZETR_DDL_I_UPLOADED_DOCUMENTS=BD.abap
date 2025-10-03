managed implementation in class zbp_etr_ddl_i_uploaded_docs unique;
strict ( 1 );

define behavior for zetr_ddl_i_uploaded_documents alias UploadedDocuments
persistent table zetr_t_updoc
lock master
authorization master ( instance )
late numbering
etag master ChangedAt
{
  mapping for zetr_t_updoc
    {
      DocumentId      = document_id;
      DocumentType    = document_type;
      Filename        = file_name;
      MimeType        = mime_type;
      DocumentContent = document_content;
      Processed       = processed;
      Notes           = notes;
      CreatedBy       = created_by;
      CreatedAt       = created_at;
      ChangedAt       = changed_at;
      changedBy       = changed_by;
    }

  create ( authorization : global );
  update;
  delete ( features : instance );

  field ( readonly ) DocumentId, CreatedBy, CreatedAt, ChangedAt, ChangedBy;
  field ( features : instance ) DocumentType;

  action ( features : instance ) processDocument result [1] $self;
}