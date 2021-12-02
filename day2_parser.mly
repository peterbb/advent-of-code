%token FORWARD DOWN UP EOF
%token <int> NUMBER

%{ 
open Day2_command
%}

%start <Day2_command.t list> commands
%%

commands: l=list(command); EOF { l }

command:
    | FORWARD; n=NUMBER { Forward n }
    | DOWN; n=NUMBER { Down n }
    | UP; n=NUMBER { Up n }
