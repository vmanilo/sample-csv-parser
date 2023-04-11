mod parser;

extern crate lazy_static;

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{Read, Write};

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref CELL_ADDRESS_RGX: Regex = Regex::new(r"^([A-Z])(\d+)$").unwrap();
    static ref CELL_ABOVE_RGX: Regex = Regex::new(r"^([A-Z])\^$").unwrap();
    static ref CELL_LAST_EVALUATED_ABOVE_RGX: Regex = Regex::new(r"^([A-Z])\^v$").unwrap();
    static ref CELL_COLUMN_RGX: Regex = Regex::new(r"^@(\w+)<(\d+)>$").unwrap();
}

#[derive(Debug)]
struct Position {
    row: usize,
    col: usize,
}

#[derive(Debug)]
struct Spreadsheet {
    rows: Vec<Vec<String>>,
    columns: HashMap<String, Position>,
}

impl parser::Sheet for Spreadsheet {
    fn get(&self, cell: &str, r: usize) -> String {
        match cell {
            cell if CELL_ADDRESS_RGX.is_match(cell) => {
                let cap = CELL_ADDRESS_RGX.captures(cell).unwrap();
                let row = (cap[2].parse::<u32>().unwrap() - 1) as usize;
                let col = (cap[1].chars().next().unwrap() as u32 - 'A' as u32) as usize;

                self.rows[row][col].clone()
            }

            cell if CELL_ABOVE_RGX.is_match(cell) => {
                let cap = CELL_ABOVE_RGX.captures(cell).unwrap();
                let col = (cap[1].chars().next().unwrap() as u32 - 'A' as u32) as usize;

                self.rows[r - 1][col].clone()
            }

            cell if CELL_LAST_EVALUATED_ABOVE_RGX.is_match(cell) => {
                let cap = CELL_LAST_EVALUATED_ABOVE_RGX.captures(cell).unwrap();
                let col = (cap[1].chars().next().unwrap() as u32 - 'A' as u32) as usize;

                let mut val: String = "".to_string();

                for i in (0..r).rev() {
                    if self.rows[i].len() <= col {
                        continue;
                    }

                    val = self.rows[i][col].clone();
                    if val.starts_with("!") || val.starts_with("=") {
                        continue;
                    }

                    break;
                }

                val
            }

            cell if CELL_COLUMN_RGX.is_match(cell) => {
                let cap = CELL_COLUMN_RGX.captures(cell).unwrap();
                let col = cap[1].to_string();
                let row = cap[2].parse::<u32>().unwrap() as usize;

                if !self.columns.contains_key(col.as_str()) {
                    panic!("column {} not found", col)
                }

                let pos = self.columns.get(col.as_str()).unwrap();
                self.rows[pos.row + row][pos.col].clone()
            }


            _ => "0".to_string()
        }
    }
}

impl Spreadsheet {
    fn new() -> Self {
        Spreadsheet {
            rows: Vec::new(),
            columns: HashMap::new(),
        }
    }

    fn new_from_string(content: &String) -> Self {
        let mut sheet = Spreadsheet::new();

        for (r, line) in content.split_terminator("\n").enumerate() {
            if line.len() == 0 {
                continue;
            }

            let mut row: Vec<String> = Vec::new();

            for (c, item) in line.split_terminator("|").enumerate() {
                row.push(item.to_string());

                if item.starts_with("!") {
                    let column_name = item.to_string().split_off(1);
                    sheet.columns.insert(column_name, Position { row: r, col: c });
                }
            }

            sheet.rows.push(row);
        }

        sheet
    }

    fn to_string(self) -> String {
        let mut rows: Vec<String> = Vec::new();

        for row in self.rows.iter() {
            rows.push(row.join("|").to_string());
        }

        rows.join("\n").to_string()
    }

    fn eval(&mut self) -> Result<(), parser::SyntaxError>  {
        for r in 0..self.rows.len() {
            for c in 0..self.rows[r].len() {
                let cell = &self.rows[r][c];
                if cell.starts_with("=") {
                    let tokens = parser::lex(cell.clone().split_off(1))?;
                    let mut token_iter = tokens.iter().peekable();
                    let mut parser = parser::Parser::new(&mut token_iter);
                    let ast = parser.parse()?;
                    self.rows[r][c] = format!("{}", ast.eval(self, r).str());
                }
            }
        }

        Ok(())
    }

    fn copy_formulas_above(&mut self) -> Result<(), parser::SyntaxError> {
        for r in 0..self.rows.len() {
            for c in 0..self.rows[r].len() {
                let cell = &self.rows[r][c];
                if cell.starts_with("=") && cell == "=^^" {
                    let tokens = parser::lex(self.rows[r - 1][c].clone().split_off(1))?;
                    let mut token_iter = tokens.iter().peekable();
                    let mut parser = parser::Parser::new(&mut token_iter);
                    let mut ast = parser.parse()?;
                    ast.inc_formula(self, r);
                    self.rows[r][c] = format!("={}", ast.to_string());
                }
            }
        }

        Ok(())
    }
}

fn main() -> std::io::Result<()> {
    let file_path = env::args().nth(1).expect("no file path given");

    let mut file = File::open(file_path.as_str())?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let mut sheet = Spreadsheet::new_from_string(&contents);

    let formulas_res = sheet.copy_formulas_above();
    if formulas_res.is_err() {
        panic!("copy_formulas failed: {}", formulas_res.err().unwrap())
    }

    let eval_res = sheet.eval();
    if eval_res.is_err() {
        panic!("eval failed: {}", eval_res.err().unwrap())
    }

    let mut result_file = File::create(format!("{}_computed", file_path))?;
    result_file.write_all(sheet.to_string().as_bytes())?;

    Ok(())
}
