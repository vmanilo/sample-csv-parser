# Sample CSV parser with custom formulas

## Dependencies

It requires to install Rust on the system: https://www.rust-lang.org/tools/install

## Build

To build the app just use Makefile:

```bash
$ make build
```

## Usage

Once you get the binary, you can run it against the csv file, results will be written to `<file_name>_computed` file.

```bash
$ ./csv-parser transactions.csv
```