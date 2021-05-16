module FDOM.Storage.Resources

open System
open FLite.Core

module Internal =
    
    let initializeResources = """
    CREATE TABLE resources (
		reference TEXT NOT NULL,
		name TEXT NOT NULL,
		url_name TEXT NOT NULL,
		blob_reference TEXT,
		CONSTRAINT resources_PK PRIMARY KEY (reference),
		CONSTRAINT resources_FK FOREIGN KEY (blob_reference) REFERENCES blob_store(reference)
	);
    """
    
    let initializeResourcesMetadata = """
	CREATE TABLE "resource_metadata" (
		resource_reference TEXT NOT NULL,
		"key" TEXT NOT NULL,
		value TEXT NOT NULL,
		CONSTRAINT resource_reference_UN UNIQUE (resource_reference,"key"),
		CONSTRAINT resource_reference_FK FOREIGN KEY (resource_reference) REFERENCES resources(reference)
	);
	"""
 
    let initialize (qh: QueryHandler) =
        qh.ExecuteSqlNonQuery(initializeResources) |> ignore
        qh.ExecuteSqlNonQuery(initializeResourcesMetadata) |> ignore
   
    type NewResource = {
       Reference: Guid
       Name: string
       UrlName: string
       BlobReference: Guid
   }
   
type ResourceHandler(qh: QueryHandler) =
    
    member rh.Initialize() = Internal.initialize qh
    
    member rn.AddResource(name: string, urlName: string, blobRef: Guid) =
        let resourceRef = Guid.NewGuid()
        qh.Insert<Internal.NewResource>("resources", {
            Reference = resourceRef
            Name = name
            UrlName = urlName
            BlobReference = blobRef
        })
        resourceRef