(ns hospital.core
  (:use clojure.pprint)
  (:require [clojure.test.check.generators :as gen]
            [schema-generators.generators :as g]
            [hospital.model :as h.model]))

(println (gen/sample gen/boolean, 100))
(println (gen/sample gen/int, 100))
(println (gen/sample gen/string))
(println (gen/sample gen/string-alphanumeric, 100))

(println (gen/sample (gen/vector gen/int 15), 100))
(println (gen/sample (gen/vector gen/int 1 5), 100))
(println (gen/sample (gen/vector gen/int), 100))

(println (g/sample 10 h.model/PacienteID))
(pprint (g/sample 10 h.model/Departamento))
(pprint (g/sample 10 h.model/Hospital))
(println "gerando com generate")
(pprint (g/generate h.model/Hospital))