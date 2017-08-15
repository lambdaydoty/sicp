#lang racket
(require racket/trace)


(car ''abracadabra)
(car (quote (quote abracadabra)))
            ;^^^^^
