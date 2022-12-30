# sudoku_solver
functional programming (Ocaml)

Here are the following steps to run this sudoku solver.
I use "utop" the universal toplevel for ocaml which is very useful to run programs like these.
(I run it on macOS, but the steps for windows are very similiar)

1. INSTALLATION OF OCaml USING HOMEBREW
    Search Homebrew and copy the first link that should look something like:
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    
    Open terminal and paste
    
2. INSTALL OCaml (continued)
    Go to the OCaml homepage and search for "install OCaml"
    For macOS paste the two following commands in the terminal:
    brew install ocaml --> brew install opam 
    
3. INSTALLING UTOP
    paste the two following commands in order:
        opam init ----> opam install utop
        
4. OPEN VSCODE
    pull request this repository
    open the terminal and paste in order the two following commands:
        eval `opam config env` ---> utop
        
5. IN UTOP
    write: #use "sudoku.ml";;
    finally: sudoku_solver grid;;
    
6.HAVE FUN
    there is already a grid in sudoku.ml but you can change it to whatever you like.
    know that the zeros are empty square.
    Whenever you updated your grid just use: #use "sudoku.ml";; again to update the toplevel
    
        

        
        
        
        
    
