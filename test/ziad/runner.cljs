(ns ziad.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [ziad.pos-test :as pos-test]))

(doo-tests 'ziad.pos-test)
