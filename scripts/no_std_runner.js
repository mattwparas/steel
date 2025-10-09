#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

async function main() {
  const argv = process.argv.slice(2);
  if (argv.length === 0) {
    console.error('usage: no_std_runner.js <wasm> [--list | --run-index <i>]');
    process.exit(2);
  }
  const wasmPath = argv[0];
  const rest = argv.slice(1);

  let wasmBytes;
  try {
    wasmBytes = fs.readFileSync(wasmPath);
  } catch (err) {
    console.error(`failed to read wasm binary: ${wasmPath}`);
    console.error(err);
    process.exit(1);
  }

  let instance;
  try {
    const result = await WebAssembly.instantiate(wasmBytes, {});
    instance = result.instance;
  } catch (err) {
    console.error('failed to instantiate wasm module');
    console.error(err);
    process.exit(1);
  }

  const exp = instance.exports;
  const memory = exp.memory;

  // --list mode: print JSON array of test names
  if (rest.includes('--list')) {
    const countFn = exp.test_count;
    const namePtrFn = exp.test_name_ptr;
    const nameLenFn = exp.test_name_len;
    if (!(countFn && namePtrFn && nameLenFn && memory)) {
      console.error('wasm module missing test enumeration exports');
      process.exit(1);
    }
    const count = countFn() | 0;
    const view = new Uint8Array(memory.buffer);
    const names = [];
    for (let i = 0; i < count; i++) {
      const ptr = namePtrFn(i) >>> 0;
      const len = nameLenFn(i) | 0;
      if (!ptr || len <= 0) { names.push(`test_${i}`); continue; }
      const slice = view.subarray(ptr, ptr + len);
      names.push(new TextDecoder('utf-8').decode(slice));
    }
    process.stdout.write(JSON.stringify(names));
    process.exit(0);
  }

  // --run-index <i> mode: run a specific test index
  const runIdx = rest.indexOf('--run-index');
  if (runIdx !== -1) {
    const idxStr = rest[runIdx + 1];
    const idx = idxStr ? (idxStr | 0) : -1;
    const runOne = exp.test_run_index;
    if (typeof runOne !== 'function') {
      console.error('wasm module does not export `test_run_index`');
      process.exit(1);
    }
    try {
      const status = runOne(idx) | 0;
      process.exit(status);
    } catch (err) {
      // trap => test failed
      process.exit(1);
    }
  }

  // default: run all tests via `run`
  const run = exp.run;
  if (typeof run !== 'function') {
    console.error('wasm module does not export a `run` function');
    process.exit(1);
  }

  try {
    const status = run();
    process.exit(status | 0);
  } catch (err) {
    console.error('error while executing wasm tests');
    console.error(err);
    process.exit(1);
  }
}

main();
