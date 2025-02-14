window.BENCHMARK_DATA = {
  "lastUpdate": 1739497414190,
  "repoUrl": "https://github.com/mattwparas/steel",
  "entries": {
    "Benchmark": [
      {
        "commit": {
          "author": {
            "name": "Sai Karthik",
            "username": "kskarthik",
            "email": "11899221+kskarthik@users.noreply.github.com"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "2ec5e814f38ba9de1a8cc51d581d5226c4db1a49",
          "message": "ci: set docker builds to use registry cache (#300)",
          "timestamp": "2025-02-13T16:34:14Z",
          "url": "https://github.com/mattwparas/steel/commit/2ec5e814f38ba9de1a8cc51d581d5226c4db1a49"
        },
        "date": 1739497413814,
        "tool": "cargo",
        "benches": [
          {
            "name": "range-big",
            "value": 82341,
            "range": "± 131",
            "unit": "ns/iter"
          },
          {
            "name": "map-big",
            "value": 611572,
            "range": "± 4073",
            "unit": "ns/iter"
          },
          {
            "name": "transducer-map",
            "value": 1371268,
            "range": "± 31328",
            "unit": "ns/iter"
          },
          {
            "name": "filter-big",
            "value": 473703,
            "range": "± 879",
            "unit": "ns/iter"
          },
          {
            "name": "ten-thousand-iterations",
            "value": 903971,
            "range": "± 3756",
            "unit": "ns/iter"
          },
          {
            "name": "ten-thousand-iterations-letrec",
            "value": 1344537,
            "range": "± 10146",
            "unit": "ns/iter"
          },
          {
            "name": "trie-sort-without-optimizations",
            "value": 418733,
            "range": "± 2114",
            "unit": "ns/iter"
          },
          {
            "name": "fib-28/fib-28",
            "value": 40363583,
            "range": "± 224562",
            "unit": "ns/iter"
          },
          {
            "name": "thread-creation/thread-creation",
            "value": 934946,
            "range": "± 10796",
            "unit": "ns/iter"
          },
          {
            "name": "engine-creation",
            "value": 32062983,
            "range": "± 514751",
            "unit": "ns/iter"
          },
          {
            "name": "register-fn",
            "value": 193,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "multiple-transducers",
            "value": 8492,
            "range": "± 202",
            "unit": "ns/iter"
          },
          {
            "name": "ackermann-3-3",
            "value": 264912,
            "range": "± 5550",
            "unit": "ns/iter"
          }
        ]
      }
    ]
  }
}