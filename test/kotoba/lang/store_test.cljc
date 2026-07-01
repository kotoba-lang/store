(ns kotoba.lang.store-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.lang.store :as store]
            [kotoba.lang.fs :as fs]
            [kotoba.lang.wit :as wit]))

(defn- byte-arr [coll] (byte-array (map unchecked-byte coll)))

(defn- read-write-policy []
  (-> (wit/policy) (wit/grant "store:read") (wit/grant "store:write")))

(deftest store-shape
  (let [s (store/store (fs/mem-filesystem) (read-write-policy))]
    (is (= "store/" (:prefix s)))
    (is (map? s))))

(deftest put-and-get-roundtrip
  (let [s (store/store (fs/mem-filesystem) (read-write-policy))]
    (store/put s "a" (byte-arr [1 2 3]))
    (is (= [1 2 3] (map int (seq (store/get s "a")))))))

(deftest get-missing-is-nil
  (let [s (store/store (fs/mem-filesystem) (read-write-policy))]
    (is (nil? (store/get s "nope")))))

(deftest exists-and-delete
  (let [s (store/store (fs/mem-filesystem) (read-write-policy))]
    (store/put s "a" (byte-arr [1]))
    (is (true? (store/exists? s "a")))
    (store/delete s "a")
    (is (false? (store/exists? s "a")))))

(deftest deny-by-default-no-capabilities
  (let [s (store/store (fs/mem-filesystem) (wit/policy))]   ; empty policy = deny all
    (is (= ::store/denied (store/put s "a" (byte-arr [1]))))
    (is (= ::store/denied (store/get s "a")))
    (is (= ::store/denied (store/delete s "a")))
    (is (= ::store/denied (store/exists? s "a")))))

(deftest read-only-policy-denies-write
  (let [s (store/store (fs/mem-filesystem) (-> (wit/policy) (wit/grant "store:read")))]
    (is (= ::store/denied (store/put s "a" (byte-arr [1]))))
    (is (= ::store/denied (store/delete s "a")))
    (is (nil? (store/get s "a")))))              ; read allowed, key absent -> nil

(deftest with-policy-changes-grants
  (let [fsb (fs/mem-filesystem)
        s-read (store/store fsb (-> (wit/policy) (wit/grant "store:read")))
        s-rw   (store/with-policy s-read (read-write-policy))]
    (is (= ::store/denied (store/put s-read "a" (byte-arr [1]))))
    (store/put s-rw "a" (byte-arr [1]))
    (is (= [1] (map int (seq (store/get s-rw "a")))))
    ;; read-only store can still read what rw store wrote (same fs)
    (is (= [1] (map int (seq (store/get s-read "a")))))))

(deftest custom-prefix
  (let [s (store/store (fs/mem-filesystem) (read-write-policy) {:prefix "kv/"})]
    (store/put s "a" (byte-arr [1]))
    (is (true? (fs/exists? (:fs s) "kv/a")))))
