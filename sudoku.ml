let grid = 
  [[0;2;0;0;0;0;0;0;0];
   [0;0;0;6;0;0;0;0;3];
   [0;7;4;0;8;0;0;0;0];
   [0;0;0;0;0;3;0;0;2];
   [0;8;0;0;4;0;0;1;0];
   [6;0;0;5;0;0;0;0;0];
   [0;0;0;0;1;0;7;8;0];
   [5;0;0;0;0;9;0;0;0];
   [0;0;0;0;0;0;0;4;0]] 
  
exception InvalidGrid 
  
  
let rec get_columns (input : 'a list list) : 'a list list = 
  let rec helper (input : 'a list list) (acc : 'a list list) = 
    match input with
    | [] -> acc
    | row :: rest_of_rows -> 
        helper rest_of_rows (List.map2 (fun x y -> x @ [y]) acc row)
  in helper input [[];[];[];[];[];[];[];[];[]]
        
        
let rec get_squares (input : 'a list list) = 
  let rec helper (input : 'a list list) (acc : 'a list list) = 
    if input = [] then acc
    else 
      let (row1 :: row2 :: row3 :: rest) = input in
      let rec create_squares (row1 : 'a list) (row2 : 'a list) (row3 : 'a list) (acc2 : 'a list list) =
        if (row1 = []) && (row2 = []) && (row3 = []) then acc2
        else
          let (a :: b :: c :: rest1) = row1 in
          let (d :: e :: f :: rest2) = row2 in
          let (g :: h :: i :: rest3) = row3 in
          let my_list = [a;b;c;d;e;f;g;h;i] in
          create_squares rest1 rest2 rest3 (acc2 @ [my_list]) 
      in
      let three_squares = create_squares row1 row2 row3 [] in
      helper rest (acc @ three_squares)
  in helper input []
                
    
let get_specific_row (row_input : 'a list list) (row_num : int) : 'a list =
  List.nth row_input row_num
  
let get_specific_column (col_input : 'a list list) (col_num : int) : 'a list =
  List.nth col_input col_num
  
let get_specific_square (square_input : 'a list list) (square_num : int) : 'a list = 
  List.nth square_input square_num
    
let check_row (some_row : 'a list) (some_num : 'a) : bool = 
  List.exists (fun x -> x = some_num) some_row
    
let check_col (some_col : 'a list) (some_num : 'a) : bool = 
  List.exists (fun x -> x = some_num) some_col
    
let check_square (some_square : 'a list) (some_num : 'a) : bool = 
  List.exists (fun x -> x = some_num) some_square
    
let find_square_index (row_index : int) (col_index : int) : int =
  if (0 <= row_index) && (row_index <= 2) && (0 <= col_index) && (col_index <= 2) then 0
  else if (0 <= row_index) && (row_index <= 2) && (3 <= col_index) && (col_index <= 5) then 1
  else if (0 <= row_index) && (row_index <= 2) && (6 <= col_index) && (col_index <= 8) then 2
  else if (3 <= row_index) && (row_index <= 5) && (0 <= col_index) && (col_index <= 2) then 3
  else if (3 <= row_index) && (row_index <= 5) && (3 <= col_index) && (col_index <= 5) then 4
  else if (3 <= row_index) && (row_index <= 5) && (6 <= col_index) && (col_index <= 8) then 5
  else if (6 <= row_index) && (row_index <= 8) && (0 <= col_index) && (col_index <= 2) then 6
  else if (6 <= row_index) && (row_index <= 8) && (3 <= col_index) && (col_index <= 5) then 7
  else 8
  

let find_free_square (some_row : 'a list) : int option = 
  let rec helper (some_row : 'a list) (acc : int) = 
    match some_row with
    | [] -> None
    | x :: xs -> if x = 0 then Some(acc) else helper xs (acc + 1)
  in helper some_row 0
    
    
let num_conflict (some_number : int) (row_index : int) (col_index : int) (square_index : int) 
    (row_input : 'a list list) : bool = 
  let col_input = get_columns row_input in
  let square_input = get_squares row_input in
  let my_row = get_specific_row row_input row_index in
  let my_col = get_specific_column col_input col_index in
  let my_square = get_specific_square square_input square_index in
  
  (check_row my_row some_number) || 
  (check_col my_col some_number) ||
  (check_square my_square some_number) 
  
let print_grid (grid : 'a list list) : unit = 
  let grid_length = List.length grid in
  let grid_width = List.length (List.nth grid 0) in
  for x = 0 to (grid_length - 1) do
    if (x = 0) || (x = 3) || (x = 6)  then print_string "-------------\n";
    for y = 0 to (grid_width - 1) do 
      if y = 0 then print_string "|"; 
      print_int (List.nth (List.nth grid x) y); 
      if (y = 2) || (y = 5) || (y = 8) then print_string "|" 
    done;
    Printf.printf "\n%!"
  done;
  print_string "-------------\n";
  Printf.printf "\n%!"
  
  
let solver (input : 'a list list) : 'a list list =
  let rec main (input : 'a list list) (row_tracker : int) = 
    if row_tracker = 9 then input
    else
      let curr_row = get_specific_row input row_tracker in
      let col_index = find_free_square curr_row in
      match col_index with 
      | None -> main input (row_tracker + 1)
      | Some(x) -> 
          let square_index = find_square_index row_tracker x in 
          let rec try_numbers (row_tracker : int) (col_index : int) (square_index : int) (input : 'a list list) (curr_row : 'a list) (counter : int) = 
            if counter = 10 then raise InvalidGrid
            else if  not (num_conflict counter row_tracker col_index square_index input) then
              let updated_grid = 
                List.mapi (fun i row -> 
                    if i = row_tracker 
                    then List.mapi (fun j element -> if j = col_index then counter else element) curr_row 
                    else row) input in
              try 
                main updated_grid row_tracker
              with
              | InvalidGrid -> try_numbers row_tracker col_index square_index input curr_row (counter+1)
            else try_numbers row_tracker col_index square_index input curr_row (counter+1)
          in try_numbers row_tracker x square_index input curr_row 1
  in main input 0
          

let sudoku_solver (input : 'a list list) =
  let my_sol = solver grid in
  print_grid my_sol
    
    




          
              

                                
                            
                            
                                    
  
                            
                      
                    
              
      
      
          
          
              
          
        
        
  

                   
                   
