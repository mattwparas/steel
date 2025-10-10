#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { TextDecoder } = require('util');

function parseOptions(args) {
  const options = {
    list: false,
    runIndex: null,
    runIndexProvided: false,
    junitPath: null,
    junitSuiteName: null,
    displayName: null,
    passthrough: [],
  };

  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    switch (arg) {
      case '--list':
        options.list = true;
        break;
      case '--run-index':
        if (i + 1 >= args.length) {
          console.error('--run-index requires an index argument');
          process.exit(2);
        }
        {
          const idxStr = args[++i];
          const parsed = Number.parseInt(idxStr, 10);
          if (!Number.isInteger(parsed)) {
            console.error(`invalid value for --run-index: ${idxStr}`);
            process.exit(2);
          }
          options.runIndex = parsed;
        }
        options.runIndexProvided = true;
        break;
      case '--junit':
        if (i + 1 >= args.length) {
          console.error('--junit requires a path argument');
          process.exit(2);
        }
        options.junitPath = args[++i];
        break;
      case '--junit-suite-name':
        if (i + 1 >= args.length) {
          console.error('--junit-suite-name requires a value');
          process.exit(2);
        }
        options.junitSuiteName = args[++i];
        break;
      case '--display-name':
        if (i + 1 >= args.length) {
          console.error('--display-name requires a value');
          process.exit(2);
        }
        options.displayName = args[++i];
        break;
      default:
        options.passthrough.push(arg);
        break;
    }
  }

  if (!options.junitPath && process.env.STEEL_WASM_JUNIT) {
    options.junitPath = process.env.STEEL_WASM_JUNIT;
  }
  if (!options.junitSuiteName && process.env.STEEL_WASM_JUNIT_SUITE) {
    options.junitSuiteName = process.env.STEEL_WASM_JUNIT_SUITE;
  }
  if (!options.displayName && process.env.STEEL_WASM_DISPLAY_NAME) {
    options.displayName = process.env.STEEL_WASM_DISPLAY_NAME;
  }

  return options;
}

function enumerateTests(exp, memory) {
  const countFn = exp.test_count;
  const namePtrFn = exp.test_name_ptr;
  const nameLenFn = exp.test_name_len;
  if (
    typeof countFn !== 'function' ||
    typeof namePtrFn !== 'function' ||
    typeof nameLenFn !== 'function' ||
    !memory
  ) {
    return null;
  }

  const decoder = new TextDecoder('utf-8');
  const count = countFn() | 0;
  const names = [];
  for (let i = 0; i < count; i++) {
    const ptr = namePtrFn(i) >>> 0;
    const len = nameLenFn(i) | 0;
    if (!ptr || len <= 0) {
      names.push(`test_${i}`);
      continue;
    }
    try {
      const view = new Uint8Array(memory.buffer, ptr, len);
      names.push(decoder.decode(view));
    } catch (_err) {
      names.push(`test_${i}`);
    }
  }
  return names;
}

function formatDuration(seconds) {
  const safeSeconds = Number.isFinite(seconds) ? seconds : 0;
  const text = `${safeSeconds.toFixed(3)}s`.padStart(9, ' ');
  return `[${text}]`;
}

function escapeXml(value) {
  return String(value).replace(/[<>&'"]/g, (ch) => {
    switch (ch) {
      case '<':
        return '&lt;';
      case '>':
        return '&gt;';
      case '&':
        return '&amp;';
      case '"':
        return '&quot;';
      case "'":
        return '&apos;';
      default:
        return ch;
    }
  });
}

function writeJUnitReport(reportPath, suiteName, cases) {
  const total = cases.length;
  const failures = cases.filter((c) => c.status === 'fail').length;
  const time = cases.reduce((sum, c) => sum + c.duration, 0);

  const lines = [];
  lines.push('<?xml version="1.0" encoding="UTF-8"?>');
  lines.push(
    `<testsuite name="${escapeXml(suiteName)}" tests="${total}" failures="${failures}" time="${time.toFixed(6)}">`,
  );
  for (const testCase of cases) {
    const timeAttr = testCase.duration.toFixed(6);
    const className = escapeXml(testCase.className);
    const caseName = escapeXml(testCase.name);
    lines.push(`  <testcase classname="${className}" name="${caseName}" time="${timeAttr}">`);
    if (testCase.status === 'fail') {
      const message = escapeXml(testCase.errorMessage || 'test failed');
      const details = testCase.errorDetails ? escapeXml(testCase.errorDetails) : '';
      if (details) {
        lines.push(`    <failure message="${message}">${details}</failure>`);
      } else {
        lines.push(`    <failure message="${message}" />`);
      }
    }
    lines.push('  </testcase>');
  }
  lines.push('</testsuite>');

  const dir = path.dirname(reportPath);
  if (dir && dir !== '.') {
    fs.mkdirSync(dir, { recursive: true });
  }
  fs.writeFileSync(reportPath, `${lines.join('\n')}\n`);
}

function guessSuiteName(wasmPath) {
  const file = path.basename(wasmPath).replace(/\.wasm$/i, '');
  return file.replace(/-[0-9a-f]+$/i, '') || file;
}

async function main() {
  const argv = process.argv.slice(2);
  if (argv.length === 0) {
    console.error('usage: no_std_runner.js <wasm> [options]');
    return 2;
  }

  const wasmPath = argv[0];
  const opts = parseOptions(argv.slice(1));

  let wasmBytes;
  try {
    wasmBytes = fs.readFileSync(wasmPath);
  } catch (err) {
    console.error(`failed to read wasm binary: ${wasmPath}`);
    console.error(err);
    return 1;
  }

  let instance;
  try {
    const result = await WebAssembly.instantiate(wasmBytes, {});
    instance = result.instance;
  } catch (err) {
    console.error('failed to instantiate wasm module');
    console.error(err);
    return 1;
  }

  const exp = instance.exports;
  const memory = exp.memory;

  const testNames = enumerateTests(exp, memory);

  if (opts.list) {
    if (!testNames) {
      console.error('wasm module missing test enumeration exports');
      return 1;
    }
    process.stdout.write(JSON.stringify(testNames));
    return 0;
  }

  if (opts.runIndexProvided) {
    const runOne = exp.test_run_index;
    if (typeof runOne !== 'function') {
      console.error('wasm module does not export `test_run_index`');
      return 1;
    }
    try {
      const status = runOne(opts.runIndex) | 0;
      return status;
    } catch (_err) {
      return 1;
    }
  }

  const runOne = exp.test_run_index;
  if (!testNames || typeof runOne !== 'function') {
    const runAll = exp.run;
    if (typeof runAll !== 'function') {
      console.error('wasm module does not export per-test API or `run` function');
      return 1;
    }
    try {
      const status = runAll();
      return status | 0;
    } catch (err) {
      console.error('error while executing wasm tests');
      console.error(err);
      return 1;
    }
  }

  const displayName = opts.displayName || guessSuiteName(wasmPath);
  const suiteName = opts.junitSuiteName || displayName;
  const results = [];
  let exitCode = 0;
  const runStart = process.hrtime.bigint();

  for (let i = 0; i < testNames.length; i++) {
    const testName = testNames[i];
    const start = process.hrtime.bigint();
    let status = 'pass';
    let errorMessage = '';
    let errorDetails = '';

    try {
      const raw = runOne(i);
      if (typeof raw === 'number' && (raw | 0) !== 0) {
        status = 'fail';
        errorMessage = `returned non-zero status ${raw | 0}`;
      }
    } catch (err) {
      status = 'fail';
      errorMessage = err && err.message ? String(err.message) : 'trap during test execution';
      errorDetails = err && err.stack ? String(err.stack) : '';
    }

    const elapsed = Number(process.hrtime.bigint() - start) / 1e9;
    const durationStr = formatDuration(elapsed);
    const statusStr = status === 'pass' ? 'PASS' : 'FAIL';
    console.log(`${statusStr} ${durationStr} ${displayName} ${testName}`);

    results.push({
      name: testName,
      className: suiteName,
      duration: elapsed,
      status,
      errorMessage,
      errorDetails,
    });

    if (status === 'fail') {
      exitCode = 1;
    }
  }

  const totalDuration = Number(process.hrtime.bigint() - runStart) / 1e9;
  const passed = results.filter((r) => r.status === 'pass').length;
  const failed = results.length - passed;
  console.log('');
  console.log(`Summary ${formatDuration(totalDuration)} ${passed} passed; ${failed} failed`);

  if (failed > 0) {
    console.error('');
    console.error('failures:');
    for (const entry of results.filter((r) => r.status === 'fail')) {
      console.error(`  ${displayName} ${entry.name}`);
      if (entry.errorMessage) {
        console.error(`    ${entry.errorMessage}`);
      }
    }
  }

  if (opts.junitPath) {
    try {
      writeJUnitReport(opts.junitPath, suiteName, results);
    } catch (err) {
      console.error(`failed to write junit report to ${opts.junitPath}`);
      console.error(err);
      exitCode = 1;
    }
  }

  return exitCode;
}

main()
  .then((code) => {
    if (typeof code === 'number') {
      process.exit(code);
    } else {
      process.exit(1);
    }
  })
  .catch((err) => {
    console.error('unexpected error while executing wasm tests');
    console.error(err);
    process.exit(1);
  });
