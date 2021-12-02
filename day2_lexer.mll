{ 
    open Day2_parser
}

let digit = ['0'-'9']
let number = digit+

rule read = parse
| [' ' '\t' '\r' '\n']+ { read lexbuf }
| "up" { UP }
| "down" { DOWN }
| "forward" { FORWARD }
| number as n { NUMBER (int_of_string n) }
| eof { EOF }

