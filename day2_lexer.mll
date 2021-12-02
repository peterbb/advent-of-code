{ 
    open Day2_parser
}

rule read = parse
| [' ' '\t' '\r' '\n']+ { read lexbuf }
| "up" { UP }
| "down" { DOWN }
| "forward" { FORWARD }
| ['0'-'9']+ as n { NUMBER (int_of_string n) }
| eof { EOF }

