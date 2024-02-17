
{
module Parser(parse) where
import Syntax as Syntax
import Lexer
import ParseUtils
import Constant as Const
}

%name parse Start
%tokentype { Token }
%error { parseError }

%token
    INT { (Token _ _ _ (Number $$))}
    BOOL { (Token _ _ _ (Lexer.Boolean $$))}
    IDENT { (Token _ _ _ (Identifier $$))}
    EOF { (Token _ _ _ Eof) }
    'let' { (Token _ _ _ (Symbol "let"))}
    'in' { (Token _ _ _ (Symbol "in"))}
    'rec' { (Token _ _ _ (Symbol "rec"))}
    'and' { (Token _ _ _ (Symbol "and"))}
    'case' { (Token _ _ _ (Symbol "case"))}
    'of' { (Token _ _ _ (Symbol "of"))}
    'if' { (Token _ _ _ (Symbol "if"))}
    'then' { (Token _ _ _ (Symbol "then"))}
    'else' { (Token _ _ _ (Symbol "else"))}
    'fun' { (Token _ _ _ (Symbol "fun"))}
    'handle' { (Token _ _ _ (Symbol "handle")) }
    'with' { (Token _ _ _ (Symbol "with")) }
    'resume' { (Token _ _ _ (Symbol "resume")) }
    '->' { (Token _ _ _ (Symbol "->"))}
    ARROW { (Token _ _ _ (Symbol "->"))}
    '>=' { (Token _ _ _ (Symbol ">="))}
    '<=' { (Token _ _ _ (Symbol "<="))}
	'==' { (Token _ _ _ (Symbol "=="))}
    '=' { (Token _ _ _ (Symbol "="))}
    '!=' { (Token _ _ _ (Symbol "!="))}
    ',' { (Token _ _ _ (Symbol ","))} 
    '[' { (Token _ _ _ (Symbol "["))} 
    ']' { (Token _ _ _ (Symbol "]"))}
    '{' { (Token _ _ _ (Symbol "{"))}
    '}' { (Token _ _ _ (Symbol "}"))}
    '+' { (Token _ _ _ (Symbol "+"))}
    '-' { (Token _ _ _ (Symbol "-"))}
    '*' { (Token _ _ _ (Symbol "*"))}
    '/' { (Token _ _ _ (Symbol "/"))}
    '(' { (Token _ _ _ (Symbol "("))}
    ')' { (Token _ _ _ (Symbol ")"))} 
    '<' { (Token _ _ _ (Symbol "<"))}
    '>' { (Token _ _ _ (Symbol ">"))}
    ':' { (Token _ _ _ (Symbol ":"))}
    '|' { (Token _ _ _ (Symbol "|"))}
    '.' { (Token _ _ _ (Symbol "."))}
    '_' { (Token _ _ _ (Symbol "_"))}
    ';' { (Token _ _ _ (Symbol ";"))}

%nonassoc 'of' 'in' 'with'      
%right '->'
%left '*' '/'
%left '+' '-'
%left '>' '<' '>=' '<=' '=' '!='
%left '|'

%%

option(x) : x  { Just $1 }
          |    { Nothing }

many(x) :            { [] }
        | x many(x)  { $1 : $2 }

many1(x) : x           { [$1] }
         | x many1(x)  { $1 : $2 }

sepBy1(x,delim) : x                       { [ $1 ] }
                | x delim sepBy1(x,delim)  { $1 : $3 }

sepBy(x,delim) : sepBy1(x,delim) { $1 }
               |                 { [] }

endWith(x,end) : x end { $1 }


Start : Expr EOF { Program $1 }



Type : IDENT  { TypeVar $1 }
   | Type '->' Type { TypeArrow $1 $3 }
   | '(' sepBy1(Type,',') ')' { case $2 of
                                           [] -> TypeVar "unit"
                                           [x] -> x
                                           xs -> TypeTuple xs }

Op : '+' { $1 } 
   | '-' { $1 }
   | '*' { $1 }
   | '/' { $1 }      
   | '>' { $1 }   
   | '<' { $1 }   
   | '>=' { $1 }   
   | '<=' { $1 }             
   | '!=' { $1 }
   | '==' {$1}

Binding : IDENT '=' Expr                         { ($1, $3) }

Expr : 'fun' IDENT '->' Expr                     { Fun $2 $4 }
     | 'let' Binding 'in' Expr                   { let (n,e) = $2 in Let n e $4 }
	 | 'let' 'rec' sepBy1(Binding,'and') 'in' Expr  { recBindings $3 $5 }
     | 'if' Expr 'then' Expr 'else' Expr         { If $2 $4 $6 }
     | 'case' Expr 'of' option('|') sepBy1(MatchingCase,'|') 
	                                             { let (pats,cases) = unzip $5 in Match $2 pats cases }
     | Expr ';' Expr                             { Sequence $1 $3 } 
     | 'handle' Expr 'with' option('|') sepBy1(Handler,'|')  
	                                             { Handle $2 $5 }
	 | 'resume' Expr Expr                        { Resume $2 $3 }
     | Term                                      { $1 }

MatchingCase : Constant '->' Expr { ($1, $3) }

Handler : IDENT '(' IDENT ',' IDENT ')' '->' Expr { ($1, $3, $5, $8) }

Term : Term Op Term                              { Prim (selectPrimOp $2) [$1, $3] }
     | Term Atom                                 { App $1 $2 }
     | Atom                                      { $1 }


Atom : IDENT                                     { Var $1 }
     | '(' sepBy(Expr,',') ')'                   { case $2 of 
                                                      []  -> Syntax.Const Unit
                                                      [x] -> x
                                                      xs -> Tuple xs }
     | Atom ':' Type                             { Anno $1 $3 }
     | Constant                                  { Syntax.Const $1 }

Constant : INT          { Const.Integer $1 }
         | BOOL         { Const.Boolean $1 }


