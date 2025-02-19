window.BENCHMARK_DATA = {
  "lastUpdate": 1739929460896,
  "repoUrl": "https://github.com/mattwparas/steel",
  "entries": {
    "Benchmark": [
      {
        "commit": {
          "author": {
            "name": "Roberto Vidal",
            "username": "jrvidal",
            "email": "roberto.vidal@ikumene.com"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "bb07cc7451825dff1cb5907138897c9ee34c91ed",
          "message": "rework unicode escapes (#307)",
          "timestamp": "2025-02-18T23:20:34Z",
          "url": "https://github.com/mattwparas/steel/commit/bb07cc7451825dff1cb5907138897c9ee34c91ed"
        },
        "date": 1739929460245,
        "tool": "cargo",
        "benches": [
          {
            "name": "range-big",
            "value": 80708,
            "range": "± 1717",
            "unit": "ns/iter"
          },
          {
            "name": "map-big",
            "value": 593174,
            "range": "± 970",
            "unit": "ns/iter"
          },
          {
            "name": "transducer-map",
            "value": 1377367,
            "range": "± 38023",
            "unit": "ns/iter"
          },
          {
            "name": "filter-big",
            "value": 449628,
            "range": "± 946",
            "unit": "ns/iter"
          },
          {
            "name": "ten-thousand-iterations",
            "value": 1111289,
            "range": "± 45753",
            "unit": "ns/iter"
          },
          {
            "name": "ten-thousand-iterations-letrec",
            "value": 1400212,
            "range": "± 10939",
            "unit": "ns/iter"
          },
          {
            "name": "trie-sort-without-optimizations",
            "value": 439118,
            "range": "± 5412",
            "unit": "ns/iter"
          },
          {
            "name": "fib-28/fib-28",
            "value": 44040018,
            "range": "± 358110",
            "unit": "ns/iter"
          },
          {
            "name": "thread-creation/thread-creation",
            "value": 910565,
            "range": "± 14207",
            "unit": "ns/iter"
          },
          {
            "name": "engine-creation",
            "value": 31424591,
            "range": "± 489425",
            "unit": "ns/iter"
          },
          {
            "name": "register-fn",
            "value": 192,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "multiple-transducers",
            "value": 8473,
            "range": "± 442",
            "unit": "ns/iter"
          },
          {
            "name": "ackermann-3-3",
            "value": 307242,
            "range": "± 3419",
            "unit": "ns/iter"
          }
        ]
      }
    ]
  }
}