window.BENCHMARK_DATA = {
  "lastUpdate": 1735696286163,
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
          "id": "9b2ada576da13ba0eccd667ecf80e2e2ec613dd0",
          "message": "Mark unbox calls as tail calls when in tail position (#299)\n\n* mark unbox calls as tail calls when in tail position\r\n\r\n* remove the command line as a dependencny",
          "timestamp": "2025-01-01T00:23:28Z",
          "url": "https://github.com/mattwparas/steel/commit/9b2ada576da13ba0eccd667ecf80e2e2ec613dd0"
        },
        "date": 1735696285648,
        "tool": "cargo",
        "benches": [
          {
            "name": "range-big",
            "value": 96575,
            "range": "± 249",
            "unit": "ns/iter"
          },
          {
            "name": "map-big",
            "value": 604027,
            "range": "± 3897",
            "unit": "ns/iter"
          },
          {
            "name": "transducer-map",
            "value": 1522531,
            "range": "± 61996",
            "unit": "ns/iter"
          },
          {
            "name": "filter-big",
            "value": 476342,
            "range": "± 1410",
            "unit": "ns/iter"
          },
          {
            "name": "ten-thousand-iterations",
            "value": 877745,
            "range": "± 4110",
            "unit": "ns/iter"
          },
          {
            "name": "ten-thousand-iterations-letrec",
            "value": 1362041,
            "range": "± 7581",
            "unit": "ns/iter"
          },
          {
            "name": "trie-sort-without-optimizations",
            "value": 430186,
            "range": "± 1804",
            "unit": "ns/iter"
          },
          {
            "name": "fib-28/fib-28",
            "value": 62159881,
            "range": "± 869881",
            "unit": "ns/iter"
          },
          {
            "name": "thread-creation/thread-creation",
            "value": 957363,
            "range": "± 21747",
            "unit": "ns/iter"
          },
          {
            "name": "engine-creation",
            "value": 31574346,
            "range": "± 321614",
            "unit": "ns/iter"
          },
          {
            "name": "register-fn",
            "value": 188,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "multiple-transducers",
            "value": 8892,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "ackermann-3-3",
            "value": 332047,
            "range": "± 20480",
            "unit": "ns/iter"
          }
        ]
      }
    ]
  }
}