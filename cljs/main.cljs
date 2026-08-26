(ns keshe);;

(defn char-code [c] (.charCodeAt c 0))

(defn hanzi? [c]
  (let [cp (char-code c)]
    (or (and (>= cp 0x4E00) (<= cp 0x9FFF))
        (and (>= cp 0x3400) (<= cp 0x4DBF))
        (and (>= cp 0x20000) (<= cp 0x2A6DF)))))

(defn hiragana? [c]
  (let [cp (char-code c)]
    (and (>= cp 0x3040) (<= cp 0x309F))))

(defn katakana? [c]
  (let [cp (char-code c)]
    (and (>= cp 0x30A0) (<= cp 0x30FF))))

(defn digit? [c]
  (let [cp (char-code c)]
    (and (>= cp 0x30) (<= cp 0x39))))

(defn whitespace? [c]
  (contains? #{\space \tab \newline \return} c))


(defn read-hanzi-unit
  [chars i]
  (let [c (nth chars i)
        n (count chars)
        hiragana-suffix (loop [j (inc i) acc []]
                          (if (and (< j n) (hiragana? (nth chars j)))
                            (recur (inc j) (conj acc (nth chars j)))
                            [(apply str acc) j]))
        suffix-str (first hiragana-suffix)
        next-j (second hiragana-suffix)]
    [(str c suffix-str) next-j]))

(defn read-katakana-unit
  [chars i]
  (let [n (count chars)
        katakana-word (loop [j i acc []]
                        (if (and (< j n) (katakana? (nth chars j)))
                          (recur (inc j) (conj acc (nth chars j)))
                          [(apply str acc) j]))
        word-str (first katakana-word)
        next-j (second katakana-word)]
    [word-str next-j]))

;;分词器

(defn tokenize [source]
  (let [chars (vec source)
        n (count chars)]
    (loop [i 0
           line 1
           col 1
           tokens []]
      (if (>= i n)
        tokens
        (let [c (nth chars i)]
          (cond
            ;; 1. 空白
            (whitespace? c)
            (if (= c \newline)
              (recur (inc i) (inc line) 1 tokens)
              (recur (inc i) line (inc col) tokens))

            ;; 2. 行注释 
            (= c \;)
            (let [next-i (loop [j (inc i)]
                           (if (or (>= j n) (= (nth chars j) \newline))
                             j
                             (recur (inc j))))]
              (recur next-i line col tokens))

            ;; 3. 块注释
            (and (hanzi? c)
                 (< (+ i 1) n)
                 (= (subs source i (min (+ i 2) n)) "所以"))
            (let [[next-i _] (loop [j (+ i 2) depth 1]
                               (cond
                                 (>= j n) [j false]
                                 (and (= (nth chars j) \也) (= depth 1)) [(inc j) true]
                                 (and (< (+ j 1) n) (= (subs source j (min (+ j 2) n)) "所以")) (recur (+ j 2) (inc depth))
                                 (= (nth chars j) \也) (recur (inc j) (dec depth))
                                 :else (recur (inc j) depth)))]
              (if next-i
                (recur next-i line col tokens)
                (do (js/console.warn (str "未闭合块注释 " line ":" col))
                    (recur n line col tokens))))

            ;; 4. 字符串
            (= c \")
            (let [[s next-i] (loop [j (inc i) acc []]
                               (cond
                                 (>= j n) [(str (apply str acc)) j]
                                 (= (nth chars j) \") [(str (apply str acc)) (inc j)]
                                 (= (nth chars j) \\) (recur (+ j 2) (conj acc (nth chars (inc j))))
                                 :else (recur (inc j) (conj acc (nth chars j)))))]
              (recur next-i line (+ col (- next-i i))
                     (conj tokens {:type :token :value s :line line :col col})))

            ;; 5. 数字
            (digit? c)
            (let [num-str (loop [j i acc []]
                            (if (and (< j n) (digit? (nth chars j)))
                              (recur (inc j) (conj acc (nth chars j)))
                              [(apply str acc) j]))
                  num (js/parseInt (first num-str) 10)]
              (recur (second num-str) line (+ col (count (first num-str)))
                     (conj tokens {:type :number :value num :line line :col col})))

            ;; 6. 汉文
            (hanzi? c)
            (let [[word next-j] (read-hanzi-unit chars i)]
              (recur next-j line (+ col (count word))
                     (conj tokens {:type :token :value word :line line :col col})))

            ;; 7. 片假名
            (katakana? c)
            (let [[word next-j] (read-katakana-unit chars i)]
              (recur next-j line (+ col (count word))
                     (conj tokens {:type :token :value word :line line :col col})))

            ;; 8. 平假名
            (hiragana? c)
            (do (js/console.warn (str "禁止独立平假名 " line ":" col "，字符 '" c "' 已跳过"))
                (recur (inc i) line (inc col) tokens))

            ;; 9. 未知字符
            :else
            (do (js/console.warn (str "未知字符 '" c "' 在 " line ":" col "，已跳过"))
                (recur (inc i) line (inc col) tokens))))))))



(def parse-rules
  (atom
    {"令"  {:syntax [\令 :token :expr]}
     "若"  {:syntax [\若 :expr \者 :exprs \也 :exprs \也]
            :ast    [\若 :expr :exprs :exprs]}
     "夫"  {:syntax [\夫 :exprs \哉]}
     "等"  {:syntax [\等 :expr :expr]}
     "加"  {:syntax [\加 :expr :expr]}
     "减"  {:syntax [\减 :expr :expr]}
     "乘"  {:syntax [\乘 :expr :expr]}
     "除"  {:syntax [\除 :expr :expr]}
     "曰"  {:syntax [\曰 :expr]}
     "列"  {:syntax [\列 :exprs]}
     "之"  {:syntax [\之 :raw \者]   ;; quote
            :ast    [\之 :raw]}
     "所"  {:syntax [\所 :raw \者]   ;; passive
            :ast    [\所 :raw]}
     "元"  {:syntax [\元 :expr]}
     "解"  {:syntax [\解 :expr]}
     "首"  {:syntax [\首 :expr]}
     "余"  {:syntax [\余 :expr]}
     "连"  {:syntax [\连 :expr :expr]}
     "空"  {:syntax [\空 :expr]}
     "长"  {:syntax [\长 :expr]}
     "映"  {:syntax [\映 :expr :expr]}
     "滤"  {:syntax [\滤 :expr :expr]}
     "折"  {:syntax [\折 :expr :expr]}
     "取"  {:syntax [\取 :expr :expr]}
     "围"  {:syntax [\围 :expr :expr]}
     "且"  {:syntax [\且 :expr :expr]}
     "或"  {:syntax [\或 :expr :expr]}
     "非"  {:syntax [\非 :expr]}
     "大"  {:syntax [\大 :expr :expr]}
     "小"  {:syntax [\小 :expr :expr]}
     "阳"  {:syntax [\阳]} 
     "阴"  {:syntax [\阴]}
     "无"  {:syntax [\无]}}))


(def default-rule
  {:syntax [:exprs]})
(defn current-token [tokens pos]
  (nth tokens pos nil))

(defn parse-error [msg token]
  (throw (ex-info msg {:token token})))

(defn literal-token? [x]
  (and (not (keyword? x)) (not= x nil)))

(defn infer-ast-template [syntax]
  (vec (remove literal-token? syntax)))

(defn find-end-token [syntax-rest]
  (first (drop-while #(not (literal-token? %)) syntax-rest)))


(declare parse-expression)
(defn parse-exprs [tokens pos end-token]
  (loop [exprs []
         current-pos pos]
    (let [tok (current-token tokens current-pos)]
      (cond
        (nil? tok)
        (if (nil? end-token)
          [exprs current-pos]
          (parse-error (str "Expected end token '" end-token "', got end of input") nil))

        (= end-token (:value tok))
        [exprs current-pos]

        :else
        (let [[expr new-pos] (parse-expression tokens current-pos)]
          (recur (conj exprs expr) new-pos))))))
(defn parse-raw [tokens pos end-token]
  (loop [raw []
         current-pos pos]
    (let [tok (current-token tokens current-pos)]
      (cond
        (nil? tok)
        (if (nil? end-token)
          [raw current-pos]
          (parse-error (str "Expected end token '" end-token "', got end of input") nil))

        (= end-token (:value tok))
        [raw current-pos]

        :else
        (recur (conj raw tok) (inc current-pos))))))

(defn parse-by-syntax [tokens pos syntax]
  (loop [s (rest syntax)
         args []
         current-pos pos]
    (cond
      (empty? s)
      [args current-pos]
      (literal-token? (first s))
      (let [expected (first s)
            actual   (current-token tokens current-pos)]
        (if (= expected (:value actual))
          (recur (rest s) args (inc current-pos))
          (parse-error (str "Expected delimiter '" expected "', got '" (:value actual) "'")
                       actual)))
      :else
      (case (first s)
        :expr
        (let [[expr new-pos] (parse-expression tokens current-pos)]
          (recur (rest s) (conj args expr) new-pos))

        :token
        (let [t (current-token tokens current-pos)]
          (when (nil? t)
            (parse-error "Expected token, got end of input" nil))
          (recur (rest s) (conj args t) (inc current-pos)))

        :exprs
        (let [end-tok (find-end-token (rest s))
              [exprs new-pos] (parse-exprs tokens current-pos end-tok)]
          (recur (rest s) (conj args exprs) new-pos))

        :raw
        (let [end-tok (find-end-token (rest s))
              [raw-toks new-pos] (parse-raw tokens current-pos end-tok)]
          (recur (rest s) (conj args raw-toks) new-pos))

        (parse-error (str "Unknown placeholder: " (first s))
                     {:value (first s)})))))

(defn parse-by-template [tokens pos op-token syntax ast-template]
  (let [[args new-pos] (parse-by-syntax tokens pos syntax)]
    [(vec (cons op-token args)) new-pos]))

(defn parse-expression [tokens pos]
  (let [tok (current-token tokens pos)]
    (cond
      (nil? tok)
      (parse-error "Unexpected end of input" nil)
      (#{:number :string} (:type tok))
      [tok (inc pos)]
      (= :token (:type tok))
      (let [name    (:value tok)
            rule    (or (get @parse-rules name) default-rule)
            syntax  (:syntax rule)
            ast-tpl (or (:ast rule) (infer-ast-template syntax))]
        (parse-by-template tokens (inc pos) tok syntax ast-tpl))

      :else
      (parse-error (str "Unknown token type: " (:type tok)) tok))))
(defn parse [source]
  (let [tokens (tokenize source)]
    (first (parse-expression tokens 0))))
