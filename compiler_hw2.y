/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;
    struct symtab{
	    char name[20];
	    char type[20];
            int addr;
            int line;
            char element_type[20];
	};
    int scope=0;
    int address=0;
    int scopes_index[1000];
    int scopes_index_[1000];
    int print_type=0;
    int conversion_t=0;
    int flag=0;

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;
    struct symtab symtable[10][20];
    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }

    /* Symbol table function - you can add new function if needed. */
    static void create_symbol(int s);
    static void insert_symbol(char* t,int type,int scope);
    int lookup_symbol(char *a);
    int lookup_type();
    static void dump_symbol(int s);
%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    char str[50];
    /* ... */
}



/* Token without return */
%token VAR 
%token INT FLOAT BOOL STRING
%token INC DEC
%token GEQ LEQ EQL NEQ
%token '=' ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token LAND LOR
%token NEWLINE PRINT PRINTLN
%token IF ELSE FOR
%token TRUE FALSE

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT
%token <i_val> BOOL_LIT
%token <str> ID 

/* Nonterminal with return, which need to sepcify type */
%type <i_val> Type TypeName ArrayType

%type <str> add_op mul_op cmp_op unary_op assign_op binary_op Literal
/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%
Program
    : StatementList { dump_symbol(scope);}
;

StatementList
    : StatementList Statement
    | Statement
;
Statement
    : DeclarationStmt NEWLINE 
    | SimpleStmt NEWLINE
    | Block NEWLINE
    | IfStmt NEWLINE
    | ForStmt NEWLINE
    | PrintStmt NEWLINE
    | NEWLINE
;
SimpleStmt
    : AssignmentStmt 
    | ExpressionStmt
    | IncDecStmt
;
DeclarationStmt
    : VAR ID Type   { insert_symbol($2,$3,scope); 
		    }
    | VAR ID Type '=' Expression  { insert_symbol($2,$3,scope); 
		    }
;
AssignmentStmt
    : Expression assign_op Expression { printf("%s\n",$2); }
;
assign_op
    : '='{ strcpy($$,"ASSIGN"); } 
    | ADD_ASSIGN { strcpy($$,"ADD_ASSIGN"); }
    | SUB_ASSIGN { strcpy($$,"SUB_ASSIGN"); }
    | MUL_ASSIGN { strcpy($$,"MUL_ASSIGN"); }
    | QUO_ASSIGN { strcpy($$,"QUO_ASSIGN"); }
    | REM_ASSIGN { strcpy($$,"REM_ASSIGN"); }
;

ExpressionStmt
    : Expression
;
IncDecStmt
    : Expression  INC { printf("INC\n");}  
    | Expression  DEC { printf("DEC\n"); } 
;
Type
    : TypeName {$$=$1;}
    | ArrayType {$$=$1;}
;
TypeName
    : INT { $$ = 0; }
    | FLOAT { $$ = 1; }
    | STRING { $$ = 2; }
    | BOOL {$$ = 3; }
;
ArrayType
    : '['{ flag=1; } Expression ']' Type { $$ = $5+4; }
;
Block
    : '{' { scope++;  } StatementList '}'{ dump_symbol(scope); if(scopes_index_[scope]>0) scopes_index_[scope]--; scope--; }
;
IfStmt
    : IF Condition Block
    | IF Condition Block ELSE IfStmt
    | IF Condition Block ELSE Block
;
Condition
    : Expression
;
ForStmt
    : FOR Condition Block
    | FOR ForClause Block
;
ForClause
    : InitStmt ';' Condition ';' PostStmt
;
InitStmt
    : SimpleStmt
;
PostStmt
     : SimpleStmt
;
PrintStmt
    : PRINT '(' Expression ')'   {
                                        if(print_type==0) printf("PRINT bool\n");
                                        else if(print_type==1) printf("PRINT int32\n");
                                        else if(print_type==2) printf("PRINT float32\n");
                                        else printf("PRINT string\n");
				 }
    | PRINTLN '(' Expression ')' { 
					if(print_type==0) printf("PRINTLN bool\n");
					else if(print_type==1) printf("PRINTLN int32\n");
					else if(print_type==2) printf("PRINTLN float32\n");
					else printf("PRINTLN string\n");
					flag=0;
				 }
;
Expression 
    : AExpression LOR AExpression { printf("LOR\n"); print_type=0; }
    | AExpression
;

AExpression
    : AExpression LAND BExpr { printf("LAND\n"); print_type=0; }
    | BExpr
;

BExpr
    : BExpr cmp_op CExpr { printf("%s\n",$2);  }
    | CExpr
;
CExpr
    : CExpr add_op DExpr { printf("%s\n",$2); }
    | DExpr
;
DExpr
    : DExpr mul_op UnaryExpr { printf("%s\n",$2); }
    | UnaryExpr
;
 
UnaryExpr
    : PrimaryExpr
    | unary_op UnaryExpr { printf("%s\n",$1); }
;
binary_op
    : LOR
    | LAND
    | cmp_op
    | add_op
    | mul_op
;
cmp_op
    : EQL { strcpy($$,"EQL"); }
    | NEQ { strcpy($$,"NEQ"); }
    | '<' { strcpy($$,"LSS"); }
    | LEQ { strcpy($$,"LEQ"); } 
    | '>' { strcpy($$,"GTR"); }
    | GEQ { strcpy($$,"GEQ"); }
;
add_op
    : '+' { strcpy($$,"ADD"); print_type=1;}
    | '-' { strcpy($$,"SUB"); print_type=1;}
;
mul_op
    : '*' { strcpy($$,"MUL"); }
    | '/' { strcpy($$,"QUO"); print_type=2;}
    | '%' { strcpy($$,"REM"); print_type=2;}
;
unary_op
    : '+' { strcpy($$,"POS"); }
    | '-' { strcpy($$,"NEG"); }
    | '!' { strcpy($$,"NOT"); }
;
PrimaryExpr
    : Oprand 
    | IndexExpr
    | ConversionExpr
;
Oprand
    : Literal
    | ID { int addr_= lookup_symbol($1); printf("IDENT (name=%s, address=%d)\n",$1,addr_);printf("%d\n",scopes_index[scope]); }
    | '(' Expression ')'
    | TRUE { printf("TRUE\n"); }
    | FALSE { printf("FALSE\n"); }
;
Literal
    : INT_LIT { printf("INT_LIT %d\n",$1); 
		if(flag==0)
		{
			print_type=1;
			conversion_t=1;
		}
	      }
    | FLOAT_LIT { printf("FLOAT_LIT %f\n",$1);  
		  if(flag==0)
		  {
		    print_type=2;
		    conversion_t=2;
   		  } 		
		  
                } 
    | BOOL_LIT { printf("BOOL_LIT %d\n",$1);  
                 if(flag==0)
		 {
		   print_type=0;
		   conversion_t=0;
		 }
 }
    | '"' STRING_LIT '"' { printf("STRING_LIT %s\n",$2);
                           if(flag==0)
			   {	 
			      print_type=3;
			      conversion_t=3;
			   }
}
;
IndexExpr
    : PrimaryExpr '[' Expression ']'
;
ConversionExpr
    : Type '(' Expression ')'{
				int t1=$1;
				char aa[3];
				char bb[3];
				if(t1==0) strcpy(aa,"I");
				else if(t1==1) strcpy(aa,"F");
				else if(t1==2) strcpy(aa,"S");
				else if(t1==3) strcpy(aa,"B");	
				if (conversion_t==0) strcpy(bb,"B");
				else if(conversion_t==1) strcpy(bb,"I");
				else if(conversion_t==2) strcpy(bb,"F");
   				else if(conversion_t==3) strcpy(bb,"S");
				printf("%s to %s\n",bb,aa);
			     }
;
%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }

    yylineno = 0;
    yyparse();

	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}

static void create_symbol(int s) {
	
}

static void insert_symbol(char* N , int type,int s) {
    
    if( type==0 )
	{ 
	    strcpy(symtable[s][ scopes_index[s] ].type,"int32");
	    strcpy(symtable[s][ scopes_index[s] ].element_type,"-");
	}
    else if( type==1 )
	{
	    strcpy(symtable[s][ scopes_index[s] ].type,"float32");
	    strcpy(symtable[s][ scopes_index[s] ].element_type,"-");
	}
    else if( type==2 )
	{
	    strcpy(symtable[s][ scopes_index[s] ].type,"string");
	    strcpy(symtable[s][ scopes_index[s] ].element_type,"-");
	}		
    else if ( type==3 )
        {
	    strcpy(symtable[s][ scopes_index[s] ].type,"bool");
	    strcpy(symtable[s][ scopes_index[s] ].element_type,"-");
	}
    else if ( type==4 )
	{
	    strcpy(symtable[s][ scopes_index[s] ].type,"array");
	    strcpy(symtable[s][ scopes_index[s] ].element_type,"int32");
	}
    else if ( type==5 )
	{
	    strcpy(symtable[s][ scopes_index[s] ].type,"array");
	    strcpy(symtable[s][ scopes_index[s] ].element_type,"float32");
	}
    else if ( type==6 )
	{
	    strcpy(symtable[s][ scopes_index[s] ].type,"array");
	    strcpy(symtable[s][ scopes_index[s] ].element_type,"string");
        }
    else
	{
 	    strcpy(symtable[s][ scopes_index[s] ].type,"array");
	    strcpy(symtable[s][ scopes_index[s] ].element_type,"bool");
	}
    strcpy(symtable[s][ scopes_index[s] ].name,N);
    symtable[s][ scopes_index[s] ].addr=address;
    symtable[s][ scopes_index[s] ].line=yylineno;

    address++;
    scopes_index[s]++;
    scopes_index_[s]=scopes_index[s];
    printf("> Insert {%s} into symbol table (scope level: %d)\n", N, scope);
}

int lookup_symbol(char *a) {
	for(int i=scope;i>=0;i--)
	{
	    for(int j=0;j<scopes_index[scope];j++)
		{
			if(strcmp(symtable[i][j].name,a)==0)
			{
				if(strcmp(symtable[i][j].type,"bool")==0)
					{
						 print_type=0;
						 conversion_t=0;
					}					
				else if(strcmp(symtable[i][j].type,"int32")==0)
					 {
						print_type=1;
						conversion_t=1;
					 }
				else if(strcmp(symtable[i][j].type,"float32")==0) 
					 {
						print_type=2;
						conversion_t=2;
					 }	
				else if(strcmp(symtable[i][j].type,"string")==0) 
					 {
						print_type=3;
						conversion_t=3;
					}

				return symtable[i][j].addr;
			}
			   
		}	
	}
}

static void dump_symbol(int s) {
    
    printf("> Dump symbol table (scope level: %d)\n", s);
   
    printf("%-10s%-10s%-10s%-10s%-10s%s\n",
           "Index", "Name", "Type", "Address", "Lineno", "Element type");
    for(int i=0;i<scopes_index_[s];i++)
    	printf("%-10d%-10s%-10s%-10d%-10d%s\n",
            i, symtable[s][i].name, symtable[s][i].type, symtable[s][i].addr, symtable[s][i].line, symtable[s][i].element_type );
    
}
