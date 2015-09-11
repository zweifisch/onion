(use onion clojurian-syntax redis)

(define (redis handler)
  (let ((conn (redis-connect "127.0.0.1" 6379)))
    (lambda (req)
      (handler (alist-cons 'redis (lambda args (apply redis-command* conn args)) req)))))

(defroutes app
  (GET "/" (redis) (format #f "count: ~A" (redis "get" "counter")))
  (GET "/incr" (redis) (format #f "count: ~A" (redis "incr" "counter")))
  (GET "/bad-incr" (redis) (->> (redis "get" "counter")
                                string->number
                                (+ 1)
                                number->string
                                (redis "set" "counter")))
  (GET "/debug" req (pp req)))

(run (-> app redis))
