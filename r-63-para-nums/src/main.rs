use crossterm::{ cursor, execute, style::{ Color, PrintStyledContent, Stylize }, terminal };
use std::{ io::{ self }, thread, time::Duration };
use clap::{ Arg, Command };
use rand::seq::SliceRandom;

// ASCII Art Options
const CAT: &str = r#"
 /\_/\  
( o.o ) 
 > ^ <
"#;

const DOG: &str = r#"
 / \__
(    @\____
 /         O
/   (_____/
/_____/   U
"#;

const FISH: &str =
    r#"
        .-"      "-.
       /            \
      |              |
      |,  .-.  .-.  ,|
      | )(_o/  \o_)( |
      |/     /\     \|
      (_     ^^     _)
       \__|IIIIII|__/
        | \IIIIII/ |
        \          /
         `--------`
"#;

const ASCII_IMAGES: [&str; 3] = [CAT, DOG, FISH];

fn main() {
    let matches = Command::new("ascii-art-cli")
        .version("1.0")
        .about("Displays ASCII art in the terminal with color and animation")
        .arg(
            Arg::new("image")
                .help("Choose an image: cat, dog, fish, or random")
                .required(false)
                .index(1)
        )
        .arg(
            Arg::new("animate")
                .short('a')
                .long("animate")
                .help("Animate the ASCII image")
                .takes_value(false)
        )
        .get_matches();

    let ascii_image = match matches.get_one::<String>("image").map(|s| s.as_str()) {
        Some("cat") => CAT,
        Some("dog") => DOG,
        Some("fish") => FISH,
        Some("random") | None => ASCII_IMAGES.choose(&mut rand::thread_rng()).unwrap(),
        _ => {
            eprintln!("Invalid option. Choose: cat, dog, fish, or random.");
            return;
        }
    };

    clear_screen();

    let use_animation = matches.is_present("animate");
    if use_animation {
        animate_ascii(ascii_image);
    } else {
        print_ascii(ascii_image);
    }
}

/// Clears the terminal screen.
fn clear_screen() {
    execute!(io::stdout(), terminal::Clear(terminal::ClearType::All)).unwrap();
}

/// Prints ASCII art with random colors.
fn print_ascii(ascii: &str) {
    let colors = [
        Color::Red,
        Color::Green,
        Color::Blue,
        Color::Yellow,
        Color::Magenta,
        Color::Cyan,
    ];
    let mut stdout = io::stdout();

    for line in ascii.lines() {
        let random_color = *colors.choose(&mut rand::thread_rng()).unwrap();
        execute!(stdout, PrintStyledContent(line.with(random_color))).unwrap();
        println!();
    }
}

/// Animates the ASCII art by printing it line by line with a delay.
fn animate_ascii(ascii: &str) {
    let colors = [
        Color::Red,
        Color::Green,
        Color::Blue,
        Color::Yellow,
        Color::Magenta,
        Color::Cyan,
    ];
    let mut stdout = io::stdout();

    for line in ascii.lines() {
        let random_color = *colors.choose(&mut rand::thread_rng()).unwrap();

        // Move cursor up to overwrite previous lines (optional)
        execute!(stdout, cursor::MoveUp(1)).unwrap();

        // Print each line with color
        execute!(stdout, PrintStyledContent(line.with(random_color))).unwrap();
        println!();

        // Delay for animation effect
        thread::sleep(Duration::from_millis(200));
    }
}
