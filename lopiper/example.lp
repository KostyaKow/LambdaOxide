(connectors
   (csv "CSV:jhaudio/customer")
   (erp "ERPNextREST:jhaudio/Customers"))

(fn (kk_filter value)
   (if (erp.find value)
      value
      (quit))
   (if (or blah (like value "%Kons%"))
      value
      false))

(map customer_name
   (=> (let (x (or csv.company_name
               (append csv.first_name ' ' csv.last_name)))
         (str_concat x))))
