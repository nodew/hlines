# hlines

hlines is a high-performance line counting tool for source code, implemented in Haskell. It demonstrates different approaches to parallel and asynchronous programming by providing multiple processing strategies:

- **Parallel**: Uses Haskell's parallel processing capabilities
- **STM**: Implements Software Transactional Memory for concurrent operations
- **Streamly**: Stream processing using the Streamly library
- **Conduit**: Stream processing using the Conduit library (default)

This project serves as both a practical tool and an educational example of different concurrent programming paradigms in Haskell.

## Features

- Multiple processing strategies to demonstrate different concurrent programming approaches
- Supports various programming languages through language-specific configurations
- Fast and efficient line counting with parallel processing
- Detailed statistics including blank lines, comment lines, and code lines
- Cross-platform compatibility

## Installation

1. Make sure you have GHC (Glasgow Haskell Compiler) and Cabal installed
2. Clone this repository
3. Build the project:
```bash
cabal build
```

## Usage

```bash
hlines <path> [--strategy <strategy>]
```

### Arguments
- `<path>`: Directory path to analyze

### Options
- `-s, --strategy <strategy>`: Processing strategy to use (default: conduit)

### Available Strategies
- `parallel`: Parallel based processing
- `stm`: STM based processing
- `streamly`: Stream processing using Streamly
- `conduit`: Stream processing using Conduit (default)

### Examples
```bash
# Count lines in current directory using default strategy (conduit)
hlines .

# Count lines in src directory using parallel strategy
hlines src --strategy parallel

# Count lines in a project using streamly strategy
hlines /path/to/project -s streamly
```

## Contributing

Contributions are welcome! Here's how you can help:

1. Fork the repository
2. Create a new branch for your feature
3. Add your changes
4. Write/update tests if necessary
5. Update documentation
6. Submit a pull request

When contributing, you might want to:
- Add support for new programming languages
- Implement new processing strategies
- Improve performance of existing implementations
- Add new features or enhance existing ones
- Fix bugs
- Improve documentation

## License

This project is licensed under the BSD-3-Clause License - see the LICENSE file for details.

