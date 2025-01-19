use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use std::sync::{Arc, LazyLock};
use std::time::{Duration, Instant};
use console::Style;

static mut STEP_COUNTER: usize = 0;

pub fn get_step() -> usize {
    unsafe {
        STEP_COUNTER += 1;
        STEP_COUNTER
    }
}

static MULTI_PROGRESS: LazyLock<Arc<MultiProgress>> = LazyLock::new(|| Arc::new(MultiProgress::new()));

pub fn log_progress<F, T>(message: &'static str, f: F) -> T
where
    F: FnOnce() -> T
{
    let start_time = Instant::now();
    
    let style: ProgressStyle = ProgressStyle::with_template("{prefix:.bold.dim} {spinner} {wide_msg}")
        .unwrap()
        .tick_strings(&["⢎⡰", "⢎⡡", "⢎⡑", "⢎⠱", "⠎⡱", "⢊⡱", "⢌⡱", "⢆⡱", ""]);
    let green_bold = Style::new().green().bold();

    let bar = MULTI_PROGRESS.add(ProgressBar::new_spinner());
    bar.set_style(style);
    bar.set_prefix(format!("[{}/?]", get_step()));
    bar.set_message(message);
    bar.enable_steady_tick(Duration::from_millis(100));
    
    let result = f();
    
    bar.finish_with_message(format!("{} {} {:?}", green_bold.apply_to("DONE"), message, start_time.elapsed()));
    
    result
}

pub fn log_success(message: &str) {
    let green_bold = Style::new().green().bold();
    
    MULTI_PROGRESS.println(format!("{}", green_bold.apply_to(message))).unwrap()
}

pub fn clear_progress() {
    MULTI_PROGRESS.clear().unwrap();
}