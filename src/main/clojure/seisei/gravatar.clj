(ns seisei.gravatar)

(defn md5
  [token]
  (let [hash-bytes
        (doto (java.security.MessageDigest/getInstance "MD5")
          (.reset)
          (.update (.getBytes token)))]
    (.toString
     (new java.math.BigInteger 1 (.digest hash-bytes))
     16)))

(defn gravatar
  [user]
  (let [email      (user :email)
        email      (if (nil? email) nil (.toLowerCase email))
        hash       (md5 email)]
    hash))
