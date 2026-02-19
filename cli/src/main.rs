use indicatif::{ProgressBar, ProgressStyle};
use kisu::{Type, Value, target::eval::NativeFn};
use miette::Result;
use std::{collections::HashMap, path::PathBuf, time::Duration};

use argh::FromArgs;

#[derive(FromArgs)]
/// Kisu cli
struct Args {
    #[argh(positional)]
    path: Option<PathBuf>,

    #[argh(option, description = "kisu code", short = 'c')]
    code: Option<String>,
}

fn builtins() -> HashMap<String, NativeFn> {
    let mut builtins = HashMap::new();
    builtins.insert(
        "print".into(),
        NativeFn {
            name: "print".into(),
            fun: Box::new(|val| {
                if let Value::String(s) = val {
                    println!("{s}");
                }
                Value::Bool(true)
            }),
            arg_ty: Type::String,
            ret_ty: Type::Bool,
        },
    );
    builtins
}

fn main() -> Result<()> {
    let args: Args = argh::from_env();

    if let Some(path) = args.path {
        let source = std::fs::read_to_string(path).map_err(|e| miette::miette!(e.to_string()))?;
        run(&source);
    }

    if let Some(source) = args.code {
        run(&source);
    }

    Ok(())
}

fn run(source: &str) {
    let pb = ProgressBar::new_spinner();
    pb.enable_steady_tick(Duration::from_millis(120));
    pb.set_style(
        ProgressStyle::with_template("{spinner:.blue} {msg:.magenta}")
            .unwrap()
            .tick_strings(&["⢎ ", "⠎⠁", "⠊⠑", "⠈⠱", " ⡱", "⢀⡰", "⢄⡠", "⢆⡀", ""]),
    );
    pb.set_message("Running kisu...");
    let result = kisu::run_with_native_functions(source, builtins());
    pb.finish_with_message(format!("{result:?}"));
}
