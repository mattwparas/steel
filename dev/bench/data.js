window.BENCHMARK_DATA = {
  "lastUpdate": 1733709356977,
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
          "id": "454cde0b659b07b3bd5bb6cb04f665a3f124240f",
          "message": "Fix panic during analysis in LSP (#294)",
          "timestamp": "2024-12-08T22:19:03Z",
          "url": "https://github.com/mattwparas/steel/commit/454cde0b659b07b3bd5bb6cb04f665a3f124240f"
        },
        "date": 1733709356543,
        "tool": "cargo",
        "benches": [
          {
            "name": "range-big",
            "value": 120843,
            "range": "± 402",
            "unit": "ns/iter"
          },
          {
            "name": "map-big",
            "value": 712994,
            "range": "± 16187",
            "unit": "ns/iter"
          },
          {
            "name": "transducer-map",
            "value": 1740451,
            "range": "± 11510",
            "unit": "ns/iter"
          },
          {
            "name": "filter-big",
            "value": 499277,
            "range": "± 584",
            "unit": "ns/iter"
          },
          {
            "name": "ten-thousand-iterations",
            "value": 869594,
            "range": "± 22029",
            "unit": "ns/iter"
          },
          {
            "name": "ten-thousand-iterations-letrec",
            "value": 1584014,
            "range": "± 7988",
            "unit": "ns/iter"
          },
          {
            "name": "trie-sort-without-optimizations",
            "value": 489659,
            "range": "± 6445",
            "unit": "ns/iter"
          },
          {
            "name": "fib-28/fib-28",
            "value": 68021259,
            "range": "± 1165842",
            "unit": "ns/iter"
          },
          {
            "name": "thread-creation/thread-creation",
            "value": 899931,
            "range": "± 24036",
            "unit": "ns/iter"
          },
          {
            "name": "engine-creation",
            "value": 27231452,
            "range": "± 1585324",
            "unit": "ns/iter"
          },
          {
            "name": "register-fn",
            "value": 186,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "multiple-transducers",
            "value": 9548,
            "range": "± 82",
            "unit": "ns/iter"
          },
          {
            "name": "ackermann-3-3",
            "value": 361020,
            "range": "± 18315",
            "unit": "ns/iter"
          }
        ]
      }
    ]
  }
}