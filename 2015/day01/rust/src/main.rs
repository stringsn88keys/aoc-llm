use std::env;
use std::fs;
use std::process;

/// Calculate the final floor based on parentheses instructions
fn calculate_floor(instructions: &str) -> i32 {
    instructions.chars().fold(0, |floor, ch| {
        match ch {
            '(' => floor + 1,
            ')' => floor - 1,
            _ => floor,
        }
    })
}

/// Find the position of the first character that causes Santa to enter the basement (floor -1)
fn find_basement_position(instructions: &str) -> Option<usize> {
    let mut floor = 0;
    for (position, ch) in instructions.chars().enumerate() {
        match ch {
            '(' => floor += 1,
            ')' => floor -= 1,
            _ => continue,
        }
        
        if floor == -1 {
            return Some(position + 1); // Return 1-based position
        }
    }
    None
}

fn main() {
    // Get command line arguments
    let args: Vec<String> = env::args().collect();
    
    // Determine input file
    let filename = if args.len() > 1 {
        &args[1]
    } else {
        "../data/input-full.txt"
    };
    
    // Read the input file
    let contents = match fs::read_to_string(filename) {
        Ok(contents) => contents.trim().to_string(),
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            process::exit(1);
        }
    };
    
    // Part A: Calculate final floor
    let final_floor = calculate_floor(&contents);
    println!("Santa ends up on floor {}", final_floor);
    
    // Part B: Find basement position
    match find_basement_position(&contents) {
        Some(position) => {
            println!("The first character that causes Santa to enter the basement is at position {}", position);
        }
        None => {
            println!("Santa never enters the basement");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_calculate_floor_examples() {
        assert_eq!(calculate_floor("(())"), 0);
        assert_eq!(calculate_floor("()()"), 0);
        assert_eq!(calculate_floor("((("), 3);
        assert_eq!(calculate_floor("(()(()("), 3);
        assert_eq!(calculate_floor("))((((("), 3);
        assert_eq!(calculate_floor("())"), -1);
        assert_eq!(calculate_floor("))("), -1);
        assert_eq!(calculate_floor(")))"), -3);
        assert_eq!(calculate_floor(")())())"), -3);
    }
    
    #[test]
    fn test_calculate_floor_edge_cases() {
        assert_eq!(calculate_floor(""), 0);
        assert_eq!(calculate_floor("abc"), 0); // Non-parentheses characters
        assert_eq!(calculate_floor("a(b)c"), 0);
    }

    #[test]
    fn test_find_basement_position_examples() {
        assert_eq!(find_basement_position(")"), Some(1));
        assert_eq!(find_basement_position("()())"), Some(5));
    }
    
    #[test]
    fn test_find_basement_position_edge_cases() {
        assert_eq!(find_basement_position("((("), None);
        assert_eq!(find_basement_position(""), None);
        assert_eq!(find_basement_position("()()"), None);
        assert_eq!(find_basement_position("(())"), None);
    }
}