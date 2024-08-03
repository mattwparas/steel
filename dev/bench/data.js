window.BENCHMARK_DATA = {
  "lastUpdate": 1722648321244,
  "repoUrl": "https://github.com/mattwparas/steel",
  "entries": {
    "Benchmark": [
      {
        "commit": {
          "author": {
            "name": "Matthew Paras",
            "username": "mattwparas",
            "email": "34500476+mattwparas@users.noreply.github.com"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "03649c12a1957d551b4495bfd01b321008168e01",
          "message": "allow returning bytevectors via ffi (#255)",
          "timestamp": "2024-08-02T04:17:49Z",
          "url": "https://github.com/mattwparas/steel/commit/03649c12a1957d551b4495bfd01b321008168e01"
        },
        "date": 1722648320841,
        "tool": "cargo",
        "benches": [
          {
            "name": "range-big",
            "value": 121202,
            "range": "± 959",
            "unit": "ns/iter"
          },
          {
            "name": "map-big",
            "value": 606808,
            "range": "± 925",
            "unit": "ns/iter"
          },
          {
            "name": "transducer-map",
            "value": 1494506,
            "range": "± 60065",
            "unit": "ns/iter"
          },
          {
            "name": "filter-big",
            "value": 503196,
            "range": "± 1645",
            "unit": "ns/iter"
          },
          {
            "name": "ten-thousand-iterations",
            "value": 835792,
            "range": "± 3344",
            "unit": "ns/iter"
          },
          {
            "name": "ten-thousand-iterations-letrec",
            "value": 1594084,
            "range": "± 14854",
            "unit": "ns/iter"
          },
          {
            "name": "trie-sort-without-optimizations",
            "value": 25382,
            "range": "± 160",
            "unit": "ns/iter"
          },
          {
            "name": "fib-28/fib-28",
            "value": 67817684,
            "range": "± 493263",
            "unit": "ns/iter"
          },
          {
            "name": "thread-creation/thread-creation",
            "value": 847598,
            "range": "± 5195",
            "unit": "ns/iter"
          },
          {
            "name": "engine-creation",
            "value": 25993837,
            "range": "± 273339",
            "unit": "ns/iter"
          },
          {
            "name": "register-fn",
            "value": 185,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "multiple-transducers",
            "value": 8700,
            "range": "± 194",
            "unit": "ns/iter"
          },
          {
            "name": "ackermann-3-3",
            "value": 371520,
            "range": "± 44581",
            "unit": "ns/iter"
          }
        ]
      }
    ]
  }
}