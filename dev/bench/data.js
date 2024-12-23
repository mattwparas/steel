window.BENCHMARK_DATA = {
  "lastUpdate": 1734918336318,
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
          "id": "cfe54cca75b9a03d858277aa3cb94701f38bd97b",
          "message": "Update hashset docs (#297)\n\n* refactor hashset functions, get ready for documentation\r\n\r\n* generate docs",
          "timestamp": "2024-12-23T01:26:25Z",
          "url": "https://github.com/mattwparas/steel/commit/cfe54cca75b9a03d858277aa3cb94701f38bd97b"
        },
        "date": 1734918335858,
        "tool": "cargo",
        "benches": [
          {
            "name": "range-big",
            "value": 107973,
            "range": "± 2752",
            "unit": "ns/iter"
          },
          {
            "name": "map-big",
            "value": 630064,
            "range": "± 9186",
            "unit": "ns/iter"
          },
          {
            "name": "transducer-map",
            "value": 1568304,
            "range": "± 14952",
            "unit": "ns/iter"
          },
          {
            "name": "filter-big",
            "value": 485467,
            "range": "± 763",
            "unit": "ns/iter"
          },
          {
            "name": "ten-thousand-iterations",
            "value": 882974,
            "range": "± 4688",
            "unit": "ns/iter"
          },
          {
            "name": "ten-thousand-iterations-letrec",
            "value": 1630157,
            "range": "± 13305",
            "unit": "ns/iter"
          },
          {
            "name": "trie-sort-without-optimizations",
            "value": 484085,
            "range": "± 3034",
            "unit": "ns/iter"
          },
          {
            "name": "fib-28/fib-28",
            "value": 66987482,
            "range": "± 2341878",
            "unit": "ns/iter"
          },
          {
            "name": "thread-creation/thread-creation",
            "value": 905966,
            "range": "± 18630",
            "unit": "ns/iter"
          },
          {
            "name": "engine-creation",
            "value": 27695939,
            "range": "± 524636",
            "unit": "ns/iter"
          },
          {
            "name": "register-fn",
            "value": 190,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "multiple-transducers",
            "value": 9259,
            "range": "± 92",
            "unit": "ns/iter"
          },
          {
            "name": "ackermann-3-3",
            "value": 369708,
            "range": "± 18981",
            "unit": "ns/iter"
          }
        ]
      }
    ]
  }
}