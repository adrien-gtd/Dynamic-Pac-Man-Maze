# Dynamic Pac-Man Maze

An exciting Pac-Man inspired game where players navigate through a randomly generated maze to reach the exit while avoiding a pursuing enemy. The player starts by choosing the size of the maze, which is then generated with random walls and paths. The player is represented by a blue circle and must reach the bottom right corner of the maze while avoiding the red circle that slowly chases them. This project is written in OCaml with a graphical interface and was developed as part of a functional programming course.

## Features
- Dynamic maze generation based on player input
- Player movement using "ZQSD" keys
- Real-time enemy pursuit mechanics
- Graphical user interface for an immersive experience
- Game-over condition when the red circle catches the player

## How to Play
1. Clone the repository and navigate to the project directory.
2. Run the game script to start the graphical interface.
3. Choose the size of the maze.
4. Navigate the blue circle to the bottom right corner to win the game.
5. Avoid the red circle, which moves slowly towards you. If it catches you, the game is over.

### Installation
1. Clone this repository:
   ```bash
   git clone https://github.com/yourusername/dynamic-pacman-maze.git
   ```
2. Navigate to the project directory:
   ```bash
   cd dynamic-pacman-maze
   ```
3. Compile the project:
   ```bash
   ocamlc -thread -o projet unix.cma threads.cma graphics.cma projet.ml
   ```
4. Run the game script:
   ```bash
   ./projet
   ```

