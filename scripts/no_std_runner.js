#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

async function main() {
  const [,, wasmPath, ...args] = process.argv;
  if (!wasmPath) {
    console.error('no wasm binary path provided');
    process.exit(1);
  }

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

  const run = instance.exports.run;
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
