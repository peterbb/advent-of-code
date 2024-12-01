%token <int> NUM
%token EOF
%start <int list * int list> parse  
%%

parse:
    l=lines { 
        match Core.List.transpose_exn l with
        | [l1; l2] -> l1, l2
        | _ -> assert false
    }

lines:
    | EOF { [] }
    | x=NUM; y=NUM; f=lines { [x; y] :: f }