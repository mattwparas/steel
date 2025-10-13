use anyhow::{anyhow, bail, Context, Result};
use clap::{ArgAction, Parser};
use std::fs;
use std::path::PathBuf;
use std::sync::mpsc;
use std::thread;
use std::time::Instant;
use wasmtime::{Engine, Instance, Memory, Module, Store};

#[derive(Debug, Parser)]
#[command(name = "test-runner")]
#[command(about = "Rust runner for no_std wasm tests", long_about = None)]
struct Cli {
    /// Path to the compiled wasm test artifact
    wasm: PathBuf,

    /// List tests to stdout as JSON and exit
    #[arg(long, action = ArgAction::SetTrue)]
    list: bool,

    /// Run a single test by index and return its status code
    #[arg(long)]
    run_index: Option<i32>,

    /// Output JUnit XML report to this path
    #[arg(long)]
    junit: Option<PathBuf>,

    /// JUnit testsuite name (defaults to display-name)
    #[arg(long)]
    junit_suite_name: Option<String>,

    /// Displayed suite name in console output
    #[arg(long)]
    display_name: Option<String>,

    /// Parallel jobs (defaults to available parallelism)
    #[arg(long)]
    jobs: Option<usize>,
}

#[derive(Clone, Debug)]
struct TestCaseResult {
    name: String,
    class_name: String,
    duration: f64,
    status: TestStatus,
    error_message: Option<String>,
    error_details: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TestStatus {
    Pass,
    Fail,
}

fn read_env_overrides(cli: &mut Cli) {
    if cli.junit.is_none() {
        if let Ok(path) = std::env::var("STEEL_WASM_JUNIT") {
            cli.junit = Some(PathBuf::from(path));
        }
    }
    if cli.junit_suite_name.is_none() {
        if let Ok(name) = std::env::var("STEEL_WASM_JUNIT_SUITE") {
            cli.junit_suite_name = Some(name);
        }
    }
    if cli.display_name.is_none() {
        if let Ok(name) = std::env::var("STEEL_WASM_DISPLAY_NAME") {
            cli.display_name = Some(name);
        }
    }
}

fn format_duration(seconds: f64) -> String {
    let safe = if seconds.is_finite() { seconds } else { 0.0 };
    format!("[{:.3}s]", safe).replace('[', "[ ").replace('s', "s ")
}

fn escape_xml(s: &str) -> String {
    s.chars()
        .map(|c| match c {
            '<' => "&lt;".into(),
            '>' => "&gt;".into(),
            '&' => "&amp;".into(),
            '"' => "&quot;".into(),
            '\'' => "&apos;".into(),
            _ => c.to_string(),
        })
        .collect::<String>()
}

fn write_junit_report(path: &PathBuf, suite_name: &str, cases: &[TestCaseResult]) -> Result<()> {
    let total = cases.len();
    let failures = cases.iter().filter(|c| c.status == TestStatus::Fail).count();
    let time: f64 = cases.iter().map(|c| c.duration).sum();

    let mut out = String::new();
    out.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    out.push_str(&format!(
        "<testsuite name=\"{}\" tests=\"{}\" failures=\"{}\" time=\"{:.6}\">\n",
        escape_xml(suite_name),
        total,
        failures,
        time
    ));
    for case in cases {
        let time_attr = format!("{:.6}", case.duration);
        let class_name = escape_xml(&case.class_name);
        let name = escape_xml(&case.name);
        out.push_str(&format!(
            "  <testcase classname=\"{}\" name=\"{}\" time=\"{}\">\n",
            class_name, name, time_attr
        ));
        if case.status == TestStatus::Fail {
            let msg = escape_xml(case.error_message.as_deref().unwrap_or("test failed"));
            if let Some(details) = &case.error_details {
                out.push_str(&format!(
                    "    <failure message=\"{}\">{}</failure>\n",
                    msg,
                    escape_xml(details)
                ));
            } else {
                out.push_str(&format!("    <failure message=\"{}\" />\n", msg));
            }
        }
        out.push_str("  </testcase>\n");
    }
    out.push_str("</testsuite>\n");

    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent)
                .with_context(|| format!("creating directory for {:?}", path))?;
        }
    }
    fs::write(path, out).with_context(|| format!("writing junit report to {:?}", path))?;
    Ok(())
}

struct WasmHarness {
    engine: Engine,
    module: Module,
}

impl WasmHarness {
    fn new(wasm: &PathBuf) -> Result<Self> {
        let engine = Engine::default();
        let module = Module::from_file(&engine, wasm)
            .with_context(|| format!("loading wasm module from {:?}", wasm))?;
        Ok(Self { engine, module })
    }

    fn enumerate_tests(&self) -> Result<Option<Vec<String>>> {
        let mut store = Store::new(&self.engine, ());
        let instance = Instance::new(&mut store, &self.module, &[])
            .context("instantiating wasm for enumeration")?;

        let count = match instance.get_typed_func::<(), i32>(&mut store, "test_count") {
            Ok(f) => f,
            Err(_) => return Ok(None),
        };
        let name_ptr = match instance.get_typed_func::<i32, i32>(&mut store, "test_name_ptr") {
            Ok(f) => f,
            Err(_) => return Ok(None),
        };
        let name_len = match instance.get_typed_func::<i32, i32>(&mut store, "test_name_len") {
            Ok(f) => f,
            Err(_) => return Ok(None),
        };
        let memory: Memory = match instance.get_memory(&mut store, "memory") {
            Some(m) => m,
            None => return Ok(None),
        };

        let total = count.call(&mut store, ()).context("calling test_count")? as usize;
        let mut names = Vec::with_capacity(total);
        for i in 0..total {
            let ptr = name_ptr
                .call(&mut store, i as i32)
                .context("calling test_name_ptr")? as usize;
            let len = name_len
                .call(&mut store, i as i32)
                .context("calling test_name_len")? as usize;
            let mem = memory.data(&store);
            if ptr == 0 || len == 0 || ptr.saturating_add(len) > mem.len() {
                names.push(format!("test_{}", i));
                continue;
            }
            let slice = &mem[ptr..ptr + len];
            let s = std::str::from_utf8(slice).unwrap_or("").to_string();
            if s.is_empty() {
                names.push(format!("test_{}", i));
            } else {
                names.push(s);
            }
        }
        Ok(Some(names))
    }

    fn run_one(&self, index: i32) -> Result<i32> {
        let mut store = Store::new(&self.engine, ());
        let instance = Instance::new(&mut store, &self.module, &[])
            .context("instantiating wasm for test run")?;

        if let Ok(run_index) = instance.get_typed_func::<i32, i32>(&mut store, "test_run_index") {
            let status = run_index
                .call(&mut store, index)
                .context("calling test_run_index")?;
            return Ok(status);
        }

        if let Ok(run_all) = instance.get_typed_func::<(), i32>(&mut store, "run") {
            let status = run_all.call(&mut store, ()).context("calling run")?;
            return Ok(status);
        }

        bail!("wasm module does not export per-test API or `run` function")
    }
}

fn main() -> Result<()> {
    let mut cli = Cli::parse();
    read_env_overrides(&mut cli);

    let harness = WasmHarness::new(&cli.wasm)?;

    if cli.list {
        let Some(names) = harness.enumerate_tests()? else {
            bail!("wasm module missing test enumeration exports");
        };
        let json = serde_json::to_string(&names)?;
        print!("{}", json);
        return Ok(());
    }

    if let Some(idx) = cli.run_index {
        // Run single test, return its status as process exit
        let status = harness.run_one(idx)?;
        std::process::exit(status);
    }

    // Parallel test execution with per-test instances
    let display_name = cli
        .display_name
        .clone()
        .unwrap_or_else(|| guess_suite_name(&cli.wasm));
    let suite_name = cli
        .junit_suite_name
        .clone()
        .unwrap_or_else(|| display_name.clone());

    let names = harness
        .enumerate_tests()?
        .ok_or_else(|| anyhow!("wasm module missing test enumeration exports"))?;
    let total = names.len();

    let jobs = cli
        .jobs
        .or_else(|| std::thread::available_parallelism().ok().map(|n| n.get()))
        .unwrap_or(1)
        .max(1);

    let start_all = Instant::now();
    let (tx, rx) = mpsc::channel::<(usize, TestCaseResult)>();

    for worker_id in 0..jobs {
        let tx = tx.clone();
        let harness = WasmHarness::new(&cli.wasm)?; // fresh per-worker (engine/module are cheap to clone/load)
        let names = names.clone();
        let display_name_cloned = display_name.clone();
        let suite_name_cloned = suite_name.clone();
        thread::spawn(move || {
            for i in (worker_id..names.len()).step_by(jobs) {
                let test_name = names[i].clone();
                let t0 = Instant::now();
                let mut status = TestStatus::Pass;
                let mut error_msg = None;
                let mut error_details = None;
                match harness.run_one(i as i32) {
                    Ok(code) => {
                        if code != 0 {
                            status = TestStatus::Fail;
                            error_msg = Some(format!("returned non-zero status {}", code));
                        }
                    }
                    Err(err) => {
                        status = TestStatus::Fail;
                        error_msg = Some("trap during test execution".to_string());
                        error_details = Some(format!("{}", err));
                    }
                }

                let dur = t0.elapsed().as_secs_f64();
                let line = format!(
                    "{} {} {} {}",
                    if status == TestStatus::Pass { "PASS" } else { "FAIL" },
                    format_duration(dur),
                    display_name_cloned,
                    test_name
                );
                println!("{}", line);

                let _ = tx.send((i, TestCaseResult {
                    name: test_name,
                    class_name: suite_name_cloned.clone(),
                    duration: dur,
                    status,
                    error_message: error_msg,
                    error_details,
                }));
            }
        });
    }
    drop(tx);

    let mut results: Vec<Option<TestCaseResult>> = vec![None; total];
    let mut failed = 0usize;
    while let Ok((i, res)) = rx.recv() {
        if res.status == TestStatus::Fail {
            failed += 1;
        }
        results[i] = Some(res);
    }

    let elapsed = start_all.elapsed().as_secs_f64();
    let passed = total - failed;
    println!("");
    println!("Summary {} {} passed; {} failed", format_duration(elapsed), passed, failed);

    if failed > 0 {
        eprintln!("");
        eprintln!("failures:");
        for res in results.iter().filter_map(|r| r.as_ref()) {
            if res.status == TestStatus::Fail {
                eprintln!("  {} {}", display_name, res.name);
                if let Some(msg) = &res.error_message {
                    eprintln!("    {}", msg);
                }
            }
        }
    }

    if let Some(path) = &cli.junit {
        let flat: Vec<TestCaseResult> = results
            .into_iter()
            .map(|o| o.expect("all results collected"))
            .collect();
        write_junit_report(path, &suite_name, &flat)?;
    }

    if failed > 0 {
        std::process::exit(1);
    }
    Ok(())
}

fn guess_suite_name(path: &PathBuf) -> String {
    let file = path
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("")
        .to_string();
    let file = file.trim_end_matches(".wasm").to_string();
    let trimmed = file
        .split('-')
        .next()
        .map(|s| s.to_string())
        .unwrap_or(file);
    if trimmed.is_empty() { "wasm-tests".to_string() } else { trimmed }
}
