%token COMMA ARROW EOF
%token <int> NUMBER

%start <(int * int * int * int) list> lines
%%

lines: l=list(line); EOF { l }

line:
    | x1=NUMBER; COMMA; y1=NUMBER; ARROW; x2=NUMBER; COMMA; y2=NUMBER
        { x1, y1, x2, y2 }
