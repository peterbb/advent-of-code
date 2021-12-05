{ 
    open Day5_parser
}

rule read = parse
| [' ' '\t' '\r' '\n']+ { read lexbuf }
| ['0'-'9']+ as n { NUMBER (int_of_string n) }
| "," { COMMA }
| "->" { ARROW }
| eof { EOF }

