(ns seisei.web.s3v2
  "Borrowed from https://github.com/weavejester/clj-aws-s3 .  The library
  was NOT working with the DefaultAWSCredentialsProviderChain. it's also
  just too big. i don't need as much as it provides. so i'm going to pare it
  down and see if i can slim it."
  (:require [clojure.string :as str]
            [clj-time.core :as t]
            [clojure.tools.logging :as log]
            [clojure.walk :as walk])
  (:import com.amazonaws.auth.BasicAWSCredentials
           com.amazonaws.auth.DefaultAWSCredentialsProviderChain
           com.amazonaws.auth.BasicSessionCredentials
           com.amazonaws.services.s3.AmazonS3Client
           com.amazonaws.AmazonServiceException
           com.amazonaws.ClientConfiguration
           com.amazonaws.HttpMethod
           com.amazonaws.services.s3.model.AccessControlList
           com.amazonaws.services.s3.model.Bucket
           com.amazonaws.services.s3.model.Grant
           com.amazonaws.services.s3.model.CanonicalGrantee
           com.amazonaws.services.s3.model.CopyObjectResult
           com.amazonaws.services.s3.model.EmailAddressGrantee
           com.amazonaws.services.s3.model.GetObjectRequest
           com.amazonaws.services.s3.model.GetObjectMetadataRequest
           com.amazonaws.services.s3.model.GroupGrantee
           com.amazonaws.services.s3.model.ListObjectsRequest
           com.amazonaws.services.s3.model.ListVersionsRequest
           com.amazonaws.services.s3.model.Owner
           com.amazonaws.services.s3.model.ObjectMetadata
           com.amazonaws.services.s3.model.ObjectListing
           com.amazonaws.services.s3.model.Permission
           com.amazonaws.services.s3.model.PutObjectRequest
           com.amazonaws.services.s3.model.S3Object
           com.amazonaws.services.s3.model.S3ObjectSummary
           com.amazonaws.services.s3.model.S3VersionSummary
           com.amazonaws.services.s3.model.VersionListing
           com.amazonaws.services.s3.model.InitiateMultipartUploadRequest
           com.amazonaws.services.s3.model.AbortMultipartUploadRequest
           com.amazonaws.services.s3.model.CompleteMultipartUploadRequest
           com.amazonaws.services.s3.model.UploadPartRequest
           java.util.concurrent.Executors
           java.io.ByteArrayInputStream
           java.io.File
           java.io.InputStream
           java.nio.charset.Charset))

(defn- s3-client*
  [cred]
    (let [client-configuration (ClientConfiguration.)
          aws-creds   (DefaultAWSCredentialsProviderChain.)
          client      (AmazonS3Client. aws-creds client-configuration)]
      (log/infof "Created S3Client %s with default credential provider chain" client)
      (when-let [endpoint (:endpoint cred)]
        (.setEndpoint client endpoint))
      client))

(def ^{:private true :tag AmazonS3Client}
  s3-client
  (memoize s3-client*))

(defprotocol ^{:no-doc true} Mappable
  "Convert a value into a Clojure map."
  (^{:no-doc true} to-map [x] "Return a map of the value."))

(extend-protocol Mappable
  ObjectMetadata
  (to-map [metadata]
    {:cache-control          (.getCacheControl metadata)
     :content-disposition    (.getContentDisposition metadata)
     :content-encoding       (.getContentEncoding metadata)
     :content-length         (.getContentLength metadata)
     :content-md5            (.getContentMD5 metadata)
     :content-type           (.getContentType metadata)
     :etag                   (.getETag metadata)
     :last-modified          (.getLastModified metadata)
     :server-side-encryption (.getServerSideEncryption metadata)
     :user (walk/keywordize-keys (into {} (.getUserMetadata metadata)))
     :version-id             (.getVersionId metadata)})
   S3Object
   (to-map [object]
     {:content  (.getObjectContent object)
      :metadata (to-map (.getObjectMetadata object))
      :bucket   (.getBucketName object)
      :key      (.getKey object)})
  Bucket
  (to-map [bucket]
    {:name          (.getName bucket)
     :creation-date (.getCreationDate bucket)
     :owner         (to-map (.getOwner bucket))})
  Owner
  (to-map [owner]
    {:id           (.getId owner)
     :display-name (.getDisplayName owner)})
  nil
  (to-map [_] nil))


(defprotocol ^{:no-doc true} ToPutRequest
  "A protocol for constructing a map that represents an S3 put request."
  (^{:no-doc true} put-request [x] "Convert a value into a put request."))

(extend-protocol ToPutRequest
  InputStream
  (put-request [is] {:input-stream is})
  File
  (put-request [f] {:file f})
  String
  (put-request [s]
    (let [bytes (.getBytes s)]
      {:input-stream     (ByteArrayInputStream. bytes)
       :content-length   (count bytes)
       :content-type     (str "text/plain; charset=" (.name (Charset/defaultCharset)))})))

(defmacro set-attr
  "Set an attribute on an object if not nil."
  {:private true}
  [object setter value]
  `(if-let [v# ~value]
     (~setter ~object v#)))

(defn- maybe-int [x]
  (if x (int x)))


(defn- map->ObjectMetadata
  "Convert a map of object metadata into a ObjectMetadata instance."
  [metadata]
  (doto (ObjectMetadata.)
    (set-attr .setCacheControl         (:cache-control metadata))
    (set-attr .setContentDisposition   (:content-disposition metadata))
    (set-attr .setContentEncoding      (:content-encoding metadata))
    (set-attr .setContentLength        (:content-length metadata))
    (set-attr .setContentMD5           (:content-md5 metadata))
    (set-attr .setContentType          (:content-type metadata))
    (set-attr .setServerSideEncryption (:server-side-encryption metadata))
    (set-attr .setUserMetadata
              (walk/stringify-keys (dissoc metadata
                                           :cache-control
                                           :content-disposition
                                           :content-encoding
                                           :content-length
                                           :content-md5
                                           :content-type
                                           :server-side-encryption)))))


(defn- ^PutObjectRequest ->PutObjectRequest
  "Create a PutObjectRequest instance from a bucket name, key and put request
  map."
  [^String bucket ^String key request]
  (cond
   (:file request)
     (let [put-obj-req (PutObjectRequest. bucket key ^java.io.File (:file request))]
       (.setMetadata put-obj-req (map->ObjectMetadata (dissoc request :file)))
       put-obj-req)
   (:input-stream request)
     (PutObjectRequest.
      bucket key
      (:input-stream request)
      (map->ObjectMetadata (dissoc request :input-stream)))))

(declare create-acl) ; used by put-object

(defn put-object
  "Put a value into an S3 bucket at the specified key. The value can be
  a String, InputStream or File (or anything that implements the ToPutRequest
  protocol).

  An optional map of metadata may also be supplied that can include any of the
  following keys:
    :cache-control          - the cache-control header (see RFC 2616)
    :content-disposition    - how the content should be downloaded by browsers
    :content-encoding       - the encoding of the content (e.g. gzip)
    :content-length         - the length of the content in bytes
    :content-md5            - the MD5 sum of the content
    :content-type           - the mime type of the content
    :server-side-encryption - set to AES256 if SSE is required

  An optional list of grant functions can be provided after metadata.
  These functions will be applied to a clear ACL and the result will be
  the ACL for the newly created object."
  [cred bucket key value & [metadata & permissions]]
  (let [req (->> (merge (put-request value) metadata)
                 (->PutObjectRequest bucket key))]
    (when permissions
      (.setAccessControlList req (create-acl permissions)))
    (.putObject (s3-client cred) req)))

(defn- http-method [method]
  (-> method name str/upper-case HttpMethod/valueOf))

(defn delete-object
  "Delete an object from an S3 bucket."
  [cred bucket key]
  (.deleteObject (s3-client cred) bucket key))


(defprotocol ^{:no-doc true} ToClojure
  "Convert an object into an idiomatic Clojure value."
  (^{:no-doc true} to-clojure [x] "Turn the object into a Clojure value."))

(extend-protocol ToClojure
  CanonicalGrantee
  (to-clojure [grantee]
    {:id           (.getIdentifier grantee)
     :display-name (.getDisplayName grantee)})
  EmailAddressGrantee
  (to-clojure [grantee]
    {:email (.getIdentifier grantee)})
  GroupGrantee
  (to-clojure [grantee]
    (condp = grantee
      GroupGrantee/AllUsers           :all-users
      GroupGrantee/AuthenticatedUsers :authenticated-users
      GroupGrantee/LogDelivery        :log-delivery))
  Permission
  (to-clojure [permission]
    (condp = permission
      Permission/FullControl :full-control
      Permission/Read        :read
      Permission/ReadAcp     :read-acp
      Permission/Write       :write
      Permission/WriteAcp    :write-acp)))

(extend-protocol Mappable
  Grant
  (to-map [grant]
    {:grantee    (to-clojure (.getGrantee grant))
     :permission (to-clojure (.getPermission grant))})
  AccessControlList
  (to-map [acl]
    {:grants (set (map to-map (.getGrants acl)))
     :owner  (to-map (.getOwner acl))}))

(defn- permission [perm]
  (case perm
    :full-control Permission/FullControl
    :read         Permission/Read
    :read-acp     Permission/ReadAcp
    :write        Permission/Write
    :write-acp    Permission/WriteAcp))

(defn- grantee [grantee]
  (cond
   (keyword? grantee)
     (case grantee
      :all-users           GroupGrantee/AllUsers
      :authenticated-users GroupGrantee/AuthenticatedUsers
      :log-delivery        GroupGrantee/LogDelivery)
   (:id grantee)
     (CanonicalGrantee. (:id grantee))
   (:email grantee)
     (EmailAddressGrantee. (:email grantee))))

(defn- clear-acl [^AccessControlList acl]
  (doseq [grantee (->> (.getGrants acl)
                       (map #(.getGrantee ^Grant %))
                       (set))]
    (.revokeAllPermissions acl grantee)))

(defn- add-acl-grants [^AccessControlList acl grants]
  (doseq [g grants]
    (.grantPermission acl
      (grantee (:grantee g))
      (permission (:permission g)))))

(defn- update-acl [^AccessControlList acl funcs]
  (let [grants (:grants (to-map acl))
        update (apply comp (reverse funcs))]
    (clear-acl acl)
    (add-acl-grants acl (update grants))))

(defn- create-acl [permissions]
  (doto (AccessControlList.)
    (update-acl permissions)))

(defn update-object-acl
  "Updates the access control list (ACL) for the supplied object using functions
  that update a set of grants"
  [cred ^String bucket ^String key & funcs]
  (let [acl (.getObjectAcl (s3-client cred) bucket key)]
    (update-acl acl funcs)
    (.setObjectAcl (s3-client cred) bucket key acl)))

(defn grant
  "Returns a function that adds a new grant map to a set of grants."
  [grantee permission]
  #(conj % {:grantee grantee :permission permission}))
