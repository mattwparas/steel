window.BENCHMARK_DATA = {
  "lastUpdate": 1740707231577,
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
          "id": "f1a605a0f3fe321f4605a80c4497eda2eac5ffce",
          "message": "Update getrandom with new feature flag (#317)\n\n* update getrandom with new feature flag\n\n* update rand\n\n* change acquiring the lock",
          "timestamp": "2025-02-27T20:56:23Z",
          "url": "https://github.com/mattwparas/steel/commit/f1a605a0f3fe321f4605a80c4497eda2eac5ffce"
        },
        "date": 1740707231122,
        "tool": "cargo",
        "benches": [
          {
            "name": "range-big",
            "value": 110697,
            "range": "± 597",
            "unit": "ns/iter"
          },
          {
            "name": "map-big",
            "value": 620435,
            "range": "± 1295",
            "unit": "ns/iter"
          },
          {
            "name": "transducer-map",
            "value": 1451783,
            "range": "± 46764",
            "unit": "ns/iter"
          },
          {
            "name": "filter-big",
            "value": 504793,
            "range": "± 1409",
            "unit": "ns/iter"
          },
          {
            "name": "ten-thousand-iterations",
            "value": 1106597,
            "range": "± 25256",
            "unit": "ns/iter"
          },
          {
            "name": "ten-thousand-iterations-letrec",
            "value": 1384872,
            "range": "± 12072",
            "unit": "ns/iter"
          },
          {
            "name": "trie-sort-without-optimizations",
            "value": 444024,
            "range": "± 2008",
            "unit": "ns/iter"
          },
          {
            "name": "fib-28/fib-28",
            "value": 43721806,
            "range": "± 415105",
            "unit": "ns/iter"
          },
          {
            "name": "thread-creation/thread-creation",
            "value": 966574,
            "range": "± 20142",
            "unit": "ns/iter"
          },
          {
            "name": "engine-creation",
            "value": 31600362,
            "range": "± 476082",
            "unit": "ns/iter"
          },
          {
            "name": "register-fn",
            "value": 198,
            "range": "± 68",
            "unit": "ns/iter"
          },
          {
            "name": "multiple-transducers",
            "value": 8913,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "ackermann-3-3",
            "value": 290571,
            "range": "± 7820",
            "unit": "ns/iter"
          }
        ]
      }
    ]
  }
}