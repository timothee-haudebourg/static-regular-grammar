use std::{env, fs, path::Path, process::Command};

#[test]
fn invalid_inputs_report_source_diagnostics() {
	let executable = env::current_exe().unwrap();
	let dependencies = executable.parent().unwrap();
	let prefix = format!("{}static_regular_grammar-", env::consts::DLL_PREFIX);
	// Use only the newest artifact: a reused target directory can hold stale
	// builds from other toolchains, which rustc rejects with E0514.
	let artifact = fs::read_dir(dependencies)
		.unwrap()
		.map(|entry| entry.unwrap().path())
		.filter(|path| {
			path.file_name()
				.unwrap()
				.to_string_lossy()
				.starts_with(&prefix)
				&& path
					.extension()
					.is_some_and(|extension| extension == env::consts::DLL_EXTENSION)
		})
		.max_by_key(|path| fs::metadata(path).and_then(|m| m.modified()).ok())
		.expect("compiled derive macro artifact");
	let root = Path::new(env!("CARGO_MANIFEST_DIR"));
	let output_dir = dependencies.join("diagnostic-fixtures");
	fs::create_dir_all(&output_dir).unwrap();
	for (fixture, expected) in [
		("invalid_inner_type", "invalid inner type"),
		("malformed_grammar", "invalid ABNF grammar:"),
	] {
		let source = format!("tests/ui/{fixture}.rs");
		let output = Command::new(env::var_os("RUSTC").unwrap_or_else(|| "rustc".into()))
			.current_dir(root)
			.env("CARGO_TARGET_DIR", &output_dir)
			.env("CARGO_MANIFEST_DIR", root)
			.env_remove("OUT_DIR")
			.args(["--edition=2021", "--crate-type=lib", "--error-format=short"])
			.arg("--extern")
			.arg(format!("static_regular_grammar={}", artifact.display()))
			.arg("--out-dir")
			.arg(&output_dir)
			.arg(&source)
			.output()
			.unwrap();
		assert_eq!(
			output.status.code(),
			Some(1),
			"{fixture} must fail to compile"
		);
		let diagnostics = String::from_utf8(output.stderr).unwrap();
		assert!(diagnostics.contains(expected), "{diagnostics}");
		assert!(!diagnostics.contains("panicked"), "{diagnostics}");
		let source_location = if fixture == "invalid_inner_type" {
			":4:20:"
		} else {
			":6:10:"
		};
		assert!(
			diagnostics.contains(&format!("{source}{source_location}")),
			"{diagnostics}"
		);
	}
}
