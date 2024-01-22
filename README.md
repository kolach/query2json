# query2json

Converts humand readable query text into filter JSON understanded by backend microservices.
Here is a few examples of query text and corresponding JSON filter:

- `a = 1` -> `{"a":1}`
- `a > 1` -> `{"a":{"gt":1}}`
- `a in [1, 2, 3]` -> `{"a":{"inq":[1,2,3]}}`
- `a in 1..2` -> `{"a":{"between":[1,2]}}`
- `a ~= /^test$/` -> `{"a":{"regexp":"^test$"}}`
- `a > 1 or b < 2` -> `{"or":[{"a":{"gt":1}},{"b":{"lt":2}}]}`
- `(a > 1 or b < 2) and !online` -> `{"and":[{"or":[{"a":{"gt":1}},{"b":{"lt":2}}]},{"online":false}]}`
- `a > 10KB` -> `{"a":{"gt":10240}}`
- `a in '2019-01-01'..'2019-12-31'` -> `{"a":{"between":["2019-01-01","2019-12-31"]}}`

License: MIT/Apache-2.0
