(use onion clojurian-syntax)

(defroutes app
  (GET "/" "index")
  (GET "/debug" req (pp req))
  (GET "/query" (query: (page size)) `#(,page ,size))
  (GET "/query-dump" (query) query)
  (POST "/body" (body) body)
  (GET "/hello/:name" (name) (string-append "hello " name)))

(run (-> app body-parser (static "/public" root: "./public")))
