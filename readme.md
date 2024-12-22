# Enkel

Enkel is an Ultra-Minimalist Terminal Editor designed specifically for IoT Devices. The name "Enkel" comes from the Swedish word meaning "simple" or "straightforward", reflecting the editor's core philosophy.

## About

Enkel is purpose-built for working in minimalist environments, particularly IoT terminals where resources are limited. Its lightweight design and efficient implementation make it ideal for editing text files in constrained systems.

## Features

- Minimalist interface
- Low resource usage
- Terminal-based operation
- Basic text search
- Built with Zig programming language

## Requirements

- Unix-like environment (Linux, macOS, BSD)
- Windows is not supported due to terminal dependencies

## Usage

```bash
enkel <filename>
```

### Key Bindings

- `Ctrl-S`: Save file
- `Ctrl-Q`: Quit
- `Ctrl-F`: Find text

## Building from Source

Requires latest Zig compiler. Build using:

```bash
zig build-exe enkel.zig
```

## License

MIT License - Copyright (c) 2024 Ramsyana - ramsyana[at]mac[dot]com