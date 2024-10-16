window.BENCHMARK_DATA = {
  "lastUpdate": 1729043086248,
  "repoUrl": "https://github.com/mattwparas/steel",
  "entries": {
    "Benchmark": [
      {
        "commit": {
          "author": {
            "name": "Talia-12",
            "username": "Talia-12",
            "email": "paradox.m.burgess@gmail.com"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "908dea467f65d54c7d6234ca6accfb3cec3c42e1",
          "message": "added change-current-directory and parent-name to fs (#271)",
          "timestamp": "2024-10-15T02:43:58Z",
          "url": "https://github.com/mattwparas/steel/commit/908dea467f65d54c7d6234ca6accfb3cec3c42e1"
        },
        "date": 1729043085811,
        "tool": "cargo",
        "benches": [
          {
            "name": "range-big",
            "value": 123958,
            "range": "± 986",
            "unit": "ns/iter"
          },
          {
            "name": "map-big",
            "value": 588180,
            "range": "± 616",
            "unit": "ns/iter"
          },
          {
            "name": "transducer-map",
            "value": 1713464,
            "range": "± 31034",
            "unit": "ns/iter"
          },
          {
            "name": "filter-big",
            "value": 408448,
            "range": "± 1083",
            "unit": "ns/iter"
          },
          {
            "name": "ten-thousand-iterations",
            "value": 798604,
            "range": "± 5113",
            "unit": "ns/iter"
          },
          {
            "name": "ten-thousand-iterations-letrec",
            "value": 1477540,
            "range": "± 32525",
            "unit": "ns/iter"
          },
          {
            "name": "trie-sort-without-optimizations",
            "value": 482186,
            "range": "± 2813",
            "unit": "ns/iter"
          },
          {
            "name": "fib-28/fib-28",
            "value": 67908005,
            "range": "± 760712",
            "unit": "ns/iter"
          },
          {
            "name": "thread-creation/thread-creation",
            "value": 861305,
            "range": "± 7062",
            "unit": "ns/iter"
          },
          {
            "name": "engine-creation",
            "value": 27356108,
            "range": "± 434354",
            "unit": "ns/iter"
          },
          {
            "name": "register-fn",
            "value": 184,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "multiple-transducers",
            "value": 8592,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "ackermann-3-3",
            "value": 365310,
            "range": "± 7037",
            "unit": "ns/iter"
          }
        ]
      }
    ]
  }
}