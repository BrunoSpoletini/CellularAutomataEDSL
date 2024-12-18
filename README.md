# CA-DSL

## Overview

CA-DSL is a domain-specific language (DSL) for creating and simulating cellular automata. This project provides a framework to define the rules and behaviors of cellular automata and visualize their evolution over time.
![GUI preview](https://github.com/user-attachments/assets/ec363d62-9018-4b9a-afd2-1a6a52e7cc48)



## Features

- Define custom cellular automata rules
- Simulate the evolution of cellular automata
- Visualize the automata in a graphical interface

## Installation

To install CA-DSL, you need to have [Haskell](https://www.haskell.org/) and [Stack](https://docs.haskellstack.org/en/stable/) installed on your system. Then, you can clone the repository and build the project using Stack.

```sh
git clone https://github.com/BrunoSpoletini/CellularAutomataEDSL.git
cd CellularAutomataEDSL
stack build
```

## Usage

To run the CA-DSL simulator, use the following command:

```sh
stack run
```
This will a compile the necessary files and start a local server with the GUI.
You can define your own cellular automata rules in the `./definitions` directory and visualize them using the [provided graphical interface](http://127.0.0.1:8023/).

## Examples

Here are some example cellular automata that you can try:

- **Game of Life**: A classic cellular automaton where cells live, die, or reproduce based on their neighbors.
- **Seeds**: The Seeds automaton is known for creating intricate and often chaotic patterns, making it a fascinating subject for study and visualization.

## Contributing

Contributions are welcome! If you have any ideas, suggestions, or bug reports, please open an issue or submit a pull request on GitHub.

## License

This project is licensed under the BSD-3-Clause License. See the [LICENSE](LICENSE) file for details.

## Contact

For any questions or inquiries, please contact Bruno Spoletini at bruno_spoletini@hotmail.com.
