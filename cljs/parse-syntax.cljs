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

(def eval-rules (atom ))
