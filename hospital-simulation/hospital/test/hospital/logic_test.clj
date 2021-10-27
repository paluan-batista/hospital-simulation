(ns hospital.logic-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [hospital.logic :refer :all]
            [hospital.model :as h.model]
            [clojure.test.check.clojure-test :refer (defspec)]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [schema.core :as s]
            [schema-generators.generators :as g]))

(s/set-fn-validation! true)


(deftest cabe-na-fila?-test

  (testing "Que cabe numa fila vazia"
    (is (cabe-na-fila? {:espera []}, :espera)))

  (testing "Que cabe pessoas em filas de tamanho até 4 inclusive"
    (doseq [fila (gen/sample (gen/vector gen/string-alphanumeric 0 4) 100)]
      (is (cabe-na-fila? {:espera fila}, :espera))))

  (testing "Que não cabe na fila quando a fila está cheia"
    (is (not (cabe-na-fila? {:espera [1 5 37 54 21]}, :espera))))

  ; one off da borda do limite pra cima
  (testing "Que não cabe na fila quando tem mais do que uma fila cheia"
    (is (not (cabe-na-fila? {:espera [1 2 3 4 5 6]}, :espera))))

  ; dentro das bordas
  (testing "Que cabe na fila quando tem gente mas não está cheia"
    (is (cabe-na-fila? {:espera [1 2 3 4]}, :espera))
    (is (cabe-na-fila? {:espera [1 2]}, :espera)))

  (testing "Que não cabe quando o departamento não existe"
    (is (not (cabe-na-fila? {:espera [1 2 3 4]}, :raio-x)))))


(defspec coloca-uma-pessoa-em-filas-menores-que-5 100
         (prop/for-all
           [fila (gen/vector gen/string-alphanumeric 0 4)
            pessoa gen/string-alphanumeric]
           (is (= {:espera (conj fila pessoa)}
                  (chega-em {:espera fila} :espera pessoa)))))

; coloquei sufixo, mas voce vai ver prefixo também no mundo
; não sou o maior fã, mas é o que a vida nos oferece
(def nome-aleatorio-gen
  (gen/fmap clojure.string/join
            (gen/vector gen/char-alphanumeric 5 10)))

(defn transforma-vetor-em-fila [vetor]
  (reduce conj h.model/fila-vazia vetor))

(def fila-nao-cheia-gen
  (gen/fmap
    transforma-vetor-em-fila
    (gen/vector nome-aleatorio-gen 0 4)))


(defn transfere-ignorando-erro [hospital para]
  (try
    (transfere hospital :espera para)
    (catch IllegalStateException e
      hospital)))

(defspec transfere-tem-que-manter-a-quantidade-de-pessoas 50
         (prop/for-all
           [
            espera (gen/fmap transforma-vetor-em-fila (gen/vector nome-aleatorio-gen 0 50))
            raio-x fila-nao-cheia-gen
            ultrasom fila-nao-cheia-gen
            vai-para (gen/vector (gen/elements [:raio-x :ultrasom]) 0 50)
            ]
           ; reduce [:raio-x :ultrasom] ==> um unico elemento
           (let [hospital-inicial {:espera espera, :raio-x raio-x, :ultrasom ultrasom}
                 hospital-final (reduce transfere-ignorando-erro hospital-inicial vai-para)]
             (= (total-de-pacientes hospital-inicial)
                (total-de-pacientes hospital-final)))))


(defn adiciona-fila-de-espera [[hospital fila]]
  (assoc hospital :espera fila))

(def hospital-gen
  (gen/fmap
    adiciona-fila-de-espera
    (gen/tuple (gen/not-empty (g/generator h.model/Hospital))
               fila-nao-cheia-gen)))

(def chega-em-gen
  "Gerador de chegadas no hospital"
  (gen/tuple (gen/return chega-em)
             (gen/return :espera)
             nome-aleatorio-gen
             (gen/return 1)))

(defn adiciona-inexistente-ao-departamento [departamento]
  (keyword (str departamento "-inexistente")))

(defn transfere-gen [hospital]
  "Gerados de transferencias no hospital"
  (let [departamentos (keys hospital)
        departamentos-inexistentes (map adiciona-inexistente-ao-departamento departamentos)
        todos-os-departamentos (concat departamentos departamentos-inexistentes)]
    (gen/tuple (gen/return transfere)
               (gen/elements todos-os-departamentos)
               (gen/elements todos-os-departamentos)
               (gen/return 0))))

(defn acao-gen [hospital]
  (gen/one-of [chega-em-gen
               (transfere-gen hospital)]))

(defn acoes-gen [hospital]
  (gen/not-empty (gen/vector (acao-gen hospital) 1 100)))


(defn executa-uma-acao [situacao [funcao param1 param2 diferenca-se-sucesso]]
  (let [hospital (:hospital situacao)
        diferenca-atual (:diferenca situacao)]
    (try
      (let [hospital-novo (funcao hospital param1 param2)]
        {:hospital  hospital-novo
         :diferenca (+ diferenca-se-sucesso diferenca-atual)})
      (catch IllegalStateException e
        situacao)
     
      (catch AssertionError e
        situacao))))

(defspec simula-um-dia-do-hospital-nao-perde-pessoas 50
         (prop/for-all
           [hospital-inicial hospital-gen]
           (let [acoes (gen/generate (acoes-gen hospital-inicial))
                 situacao-inicial {:hospital hospital-inicial, :diferenca 0}
                 total-de-pacientes-inicial (total-de-pacientes hospital-inicial)
                 situacao-final (reduce executa-uma-acao situacao-inicial acoes)
                 total-de-pacientes-final (total-de-pacientes (:hospital situacao-final))]
             (is (= (- total-de-pacientes-final (:diferenca situacao-final)) total-de-pacientes-inicial)))))
















