use std::env;
use std::fs;
use std::process;

/// Calculate wrapping paper needed for a present with dimensions l x w x h
fn calculate_wrapping_paper(l: u32, w: u32, h: u32) -> u32 {
    // Surface area: 2*l*w + 2*w*h + 2*h*l
    let surface_area = 2*l*w + 2*w*h + 2*h*l;
    
    // Extra paper: area of smallest side
    let sides = [l*w, w*h, h*l];
    let smallest_side = sides.iter().min().unwrap();
    
    surface_area + smallest_side
}

/// Parse a line like "2x3x4" into dimensions
fn parse_dimensions(line: &str) -> Result<(u32, u32, u32), Box<dyn std::error::Error>> {
    let parts: Vec<&str> = line.trim().split('x').collect();
    if parts.len() != 3 {
        return Err(format!("Invalid dimension format: {}", line).into());
    }
    
    let l = parts[0].parse::<u32>()?;
    let w = parts[1].parse::<u32>()?;
    let h = parts[2].parse::<u32>()?;
    
    Ok((l, w, h))
}

/// Solve part A: calculate total wrapping paper needed
fn solve_part_a(filename: &str) -> Result<u32, Box<dyn std::error::Error>> {
    let contents = fs::read_to_string(filename)?;
    let mut total_paper = 0;
    
    for line in contents.lines() {
        if line.trim().is_empty() {
            continue;
        }
        
        let (l, w, h) = parse_dimensions(line)?;
        let paper_needed = calculate_wrapping_paper(l, w, h);
        total_paper += paper_needed;
    }
    
    Ok(total_paper)
}

fn main() {
    // Determine input file
    let args: Vec<String> = env::args().collect();
    let filename = if args.len() > 1 {
        &args[1]
    } else {
        "../data/input.txt"
    };
    
    match solve_part_a(filename) {
        Ok(total_paper) => {
            println!("The elves should order {} square feet of wrapping paper", total_paper);
        }
        Err(err) => {
            eprintln!("Error: {}", err);
            process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_calculate_wrapping_paper_examples() {
        assert_eq!(calculate_wrapping_paper(2, 3, 4), 58);
        assert_eq!(calculate_wrapping_paper(1, 1, 10), 43);
    }
    
    #[test]
    fn test_calculate_wrapping_paper_edge_cases() {
        assert_eq!(calculate_wrapping_paper(2, 2, 2), 28); // Cube
        assert_eq!(calculate_wrapping_paper(1, 1, 1), 7);  // Unit cube
    }

    #[test]
    fn test_parse_dimensions() {
        assert_eq!(parse_dimensions("2x3x4").unwrap(), (2, 3, 4));
        assert_eq!(parse_dimensions("1x1x10").unwrap(), (1, 1, 10));
        assert_eq!(parse_dimensions("100x200x300").unwrap(), (100, 200, 300));
        
        assert!(parse_dimensions("invalid").is_err());
        assert!(parse_dimensions("1x2").is_err());
    }
    
    #[test]
    fn test_solve_part_a_with_test_input() {
        let result = solve_part_a("../data/input-test-a.txt").unwrap();
        assert_eq!(result, 101);
    }
}