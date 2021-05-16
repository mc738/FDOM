module FDOM.Storage.Documents

open System
open FLite.Core

module Internal =

    let initializeDocuments = """
    CREATE TABLE documents (
		reference TEXT NOT NULL,
		name TEXT NOT NULL,
		url_name TEXT NOT NULL,
		CONSTRAINT documents_PK PRIMARY KEY (reference),
		CONSTRAINT documents_UN UNIQUE (url_name)
	);
    """

    let initializeDocumentMetadata = """
	CREATE TABLE document_metadata (
		document_reference TEXT NOT NULL,
		"key" TEXT NOT NULL,
		value TEXT NOT NULL,
		CONSTRAINT document_metadata_UN UNIQUE (document_reference,"key"),
		CONSTRAINT document_metadata_FK FOREIGN KEY (document_reference) REFERENCES documents(reference)
	);
    """

    let initializeDocumentVersions = """
		CREATE TABLE document_versions (
			reference TEXT NOT NULL,
			document_reference TEXT NOT NULL,
			major INTEGER NOT NULL,
			minor INTEGER NOT NULL,
			revision INTEGER NOT NULL,
			suffix TEXT,
			blob_reference TEXT NOT NULL,
			CONSTRAINT blob_reference_PK PRIMARY KEY (reference),
			CONSTRAINT blob_reference_FK FOREIGN KEY (document_reference) REFERENCES documents(reference),
			CONSTRAINT blob_reference_FK_1 FOREIGN KEY (blob_reference) REFERENCES blob_store(reference)
		);
		"""

    let initializeRenderedDocuments = """
	CREATE TABLE rendered_documents (
		reference TEXT NOT NULL,
		version_reference TEXT NOT NULL,
		blob_reference TEXT NOT NULL,
		CONSTRAINT rendered_documents_PK PRIMARY KEY (reference),
		CONSTRAINT rendered_documents_FK FOREIGN KEY (version_reference) REFERENCES document_versions(reference),
		CONSTRAINT rendered_documents_FK_1 FOREIGN KEY (blob_reference) REFERENCES blob_store(reference)
	);
    """

    let initializeDocumentVersionResources = """
	CREATE TABLE document_version_resources (
		version_reference TEXT NOT NULL,
		resource_reference TEXT NOT NULL,
		CONSTRAINT document_version_resources_PK PRIMARY KEY (version_reference,resource_reference),
		CONSTRAINT document_version_resources_FK FOREIGN KEY (version_reference) REFERENCES document_versions(reference),
		CONSTRAINT document_version_resources_FK_1 FOREIGN KEY (resource_reference) REFERENCES resources(reference)
	);
	"""

    let initialize (qh: QueryHandler) =
        qh.ExecuteSqlNonQuery(initializeDocuments)
        |> ignore

        qh.ExecuteSqlNonQuery(initializeDocumentMetadata)
        |> ignore

        qh.ExecuteSqlNonQuery(initializeDocumentVersions)
        |> ignore

        qh.ExecuteSqlNonQuery(initializeRenderedDocuments)
        |> ignore

        qh.ExecuteSqlNonQuery(initializeDocumentVersionResources)
        |> ignore

    /// Internal record used for `FLite` mapping.
    type NewDocument =
        { Reference: Guid
          Name: string
          UrlName: string }

    /// Internal record used for `FLite` mapping.
    type NewDocumentVersion =
        { Reference: Guid
          DocumentReference: Guid
          Major: int
          Minor: int
          Revision: int
          Suffix: string
          BlobReference: Guid }

    type NewRenderedDocument =
        { Reference: Guid
          VersionReference: Guid
          BlobReference: Guid }

    type NewDocumentVersionResource =
        { VersionReference: Guid
          ResourceReference: Guid }

type DocumentHandler(qh: QueryHandler) =

    member dh.Initialize() = Internal.initialize qh

    /// Add a document to the database.
    member dh.AddDocument(name: string, url: string) =
        let docRef = Guid.NewGuid()

        qh.Insert<Internal.NewDocument>(
            "documents",
            { Reference = docRef
              Name = name
              UrlName = url }
        )

        docRef

    member db.AddDocumentVersion(docRef: Guid, major: int, minor: int, revision: int, suffix: string, blobRef: Guid) =
        let versionRef = Guid.NewGuid()

        qh.Insert<Internal.NewDocumentVersion>(
            "document_versions",
            { Reference = versionRef
              DocumentReference = docRef
              Major = major
              Minor = minor
              Revision = revision
              Suffix = suffix
              BlobReference = blobRef }
        )

        versionRef

    member _.AddRenderedDocument(versionRef: Guid, blobRef: Guid) =
        let rdRef = Guid.NewGuid()

        qh.Insert<Internal.NewRenderedDocument>(
            "rendered_documents",
            { Reference = rdRef
              VersionReference = versionRef
              BlobReference = blobRef }
        )

    member _.AddDocumentVersionResource(versionRef: Guid, resourceRef: Guid) =
        qh.Insert<Internal.NewDocumentVersionResource>(
            "document_version_resources",
            { VersionReference = versionRef
              ResourceReference = resourceRef }
        )
