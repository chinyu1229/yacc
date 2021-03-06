/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 2 "compiler_hw2.y" /* yacc.c:339  */

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

#line 102 "y.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    VAR = 258,
    INT = 259,
    FLOAT = 260,
    BOOL = 261,
    STRING = 262,
    INC = 263,
    DEC = 264,
    GEQ = 265,
    LEQ = 266,
    EQL = 267,
    NEQ = 268,
    ADD_ASSIGN = 269,
    SUB_ASSIGN = 270,
    MUL_ASSIGN = 271,
    QUO_ASSIGN = 272,
    REM_ASSIGN = 273,
    LAND = 274,
    LOR = 275,
    NEWLINE = 276,
    PRINT = 277,
    PRINTLN = 278,
    IF = 279,
    ELSE = 280,
    FOR = 281,
    TRUE = 282,
    FALSE = 283,
    INT_LIT = 284,
    FLOAT_LIT = 285,
    STRING_LIT = 286,
    BOOL_LIT = 287,
    ID = 288
  };
#endif
/* Tokens.  */
#define VAR 258
#define INT 259
#define FLOAT 260
#define BOOL 261
#define STRING 262
#define INC 263
#define DEC 264
#define GEQ 265
#define LEQ 266
#define EQL 267
#define NEQ 268
#define ADD_ASSIGN 269
#define SUB_ASSIGN 270
#define MUL_ASSIGN 271
#define QUO_ASSIGN 272
#define REM_ASSIGN 273
#define LAND 274
#define LOR 275
#define NEWLINE 276
#define PRINT 277
#define PRINTLN 278
#define IF 279
#define ELSE 280
#define FOR 281
#define TRUE 282
#define FALSE 283
#define INT_LIT 284
#define FLOAT_LIT 285
#define STRING_LIT 286
#define BOOL_LIT 287
#define ID 288

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 43 "compiler_hw2.y" /* yacc.c:355  */

    int i_val;
    float f_val;
    char *s_val;
    char str[50];
    /* ... */

#line 216 "y.tab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 233 "y.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  65
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   225

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  51
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  38
/* YYNRULES -- Number of rules.  */
#define YYNRULES  87
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  136

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   288

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    49,    50,     2,     2,    48,     2,     2,
      40,    41,    46,    44,     2,    45,     2,    47,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    39,
      42,    14,    43,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    35,     2,    36,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    37,     2,    38,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    81,    81,    85,    86,    89,    90,    91,    92,    93,
      94,    95,    98,    99,   100,   103,   105,   109,   112,   113,
     114,   115,   116,   117,   121,   124,   125,   128,   129,   132,
     133,   134,   135,   138,   138,   141,   141,   144,   145,   146,
     149,   152,   153,   156,   159,   162,   165,   171,   180,   181,
     185,   186,   190,   191,   194,   195,   198,   199,   203,   204,
     214,   215,   216,   217,   218,   219,   222,   223,   226,   227,
     228,   231,   232,   233,   236,   237,   238,   241,   242,   243,
     244,   245,   248,   255,   263,   270,   279,   282
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "VAR", "INT", "FLOAT", "BOOL", "STRING",
  "INC", "DEC", "GEQ", "LEQ", "EQL", "NEQ", "'='", "ADD_ASSIGN",
  "SUB_ASSIGN", "MUL_ASSIGN", "QUO_ASSIGN", "REM_ASSIGN", "LAND", "LOR",
  "NEWLINE", "PRINT", "PRINTLN", "IF", "ELSE", "FOR", "TRUE", "FALSE",
  "INT_LIT", "FLOAT_LIT", "STRING_LIT", "BOOL_LIT", "ID", "'['", "']'",
  "'{'", "'}'", "';'", "'('", "')'", "'<'", "'>'", "'+'", "'-'", "'*'",
  "'/'", "'%'", "'!'", "'\"'", "$accept", "Program", "StatementList",
  "Statement", "SimpleStmt", "DeclarationStmt", "AssignmentStmt",
  "assign_op", "ExpressionStmt", "IncDecStmt", "Type", "TypeName",
  "ArrayType", "$@1", "Block", "$@2", "IfStmt", "Condition", "ForStmt",
  "ForClause", "InitStmt", "PostStmt", "PrintStmt", "Expression",
  "AExpression", "BExpr", "CExpr", "DExpr", "UnaryExpr", "cmp_op",
  "add_op", "mul_op", "unary_op", "PrimaryExpr", "Oprand", "Literal",
  "IndexExpr", "ConversionExpr", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,    61,   269,   270,   271,   272,   273,
     274,   275,   276,   277,   278,   279,   280,   281,   282,   283,
     284,   285,   286,   287,   288,    91,    93,   123,   125,    59,
      40,    41,    60,    62,    43,    45,    42,    47,    37,    33,
      34
};
# endif

#define YYPACT_NINF -49

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-49)))

#define YYTABLE_NINF -41

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     128,   -26,   -49,   -49,   -49,   -49,   -49,   -28,   -18,   175,
     175,   -49,   -49,   -49,   -49,   -49,   -49,   -49,   -49,   175,
     -49,   -49,   -49,     3,    40,   128,   -49,    19,    20,   -49,
     -49,   -49,     8,   -49,   -49,    21,    28,    29,    32,    48,
      16,     4,    -6,   -20,   -49,   175,    23,   -49,   -49,   -49,
     -49,    14,   175,   175,    18,   -49,   -49,    18,    18,    30,
      15,   175,   128,    27,     9,   -49,   -49,   -49,   -49,   175,
     -49,   -49,   -49,   -49,   -49,   -49,   -49,   -49,   -49,   -49,
     -49,   -49,   175,   175,   175,   -49,   -49,   -49,   -49,   -49,
     -49,   175,   -49,   -49,   175,   -49,   -49,   -49,   175,   -49,
     175,    56,    31,    35,    45,   -49,   -49,   175,    37,    80,
     -49,   -49,    36,   -49,     4,    58,    -6,   -20,   -49,    44,
     175,   -49,   -49,   -12,    42,    14,   -49,   -49,   -49,   -49,
     -49,   -49,   175,   -49,   -49,   -49
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,    29,    30,    32,    31,    11,     0,     0,     0,
       0,    80,    81,    82,    83,    84,    78,    33,    35,     0,
      71,    72,    73,     0,     0,     2,     4,     0,     0,    12,
      13,    14,     0,    27,    28,     0,     0,     0,     0,    24,
      49,    51,    53,    55,    57,     0,    58,    74,    77,    75,
      76,     0,     0,     0,     0,    40,    44,     0,     0,     0,
      24,     0,     0,     0,     0,     1,     3,     6,     5,     0,
       7,     8,     9,    10,    25,    26,    18,    19,    20,    21,
      22,    23,     0,     0,     0,    65,    63,    60,    61,    62,
      64,     0,    66,    67,     0,    68,    69,    70,     0,    59,
       0,    15,     0,     0,    37,    41,    42,     0,     0,     0,
      79,    85,     0,    17,    50,    48,    52,    54,    56,     0,
       0,    46,    47,     0,     0,     0,    36,    87,    86,    16,
      39,    38,     0,    34,    45,    43
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -49,   -49,    33,   -21,   -10,   -49,   -49,   -49,   -49,   -49,
     -46,   -49,   -49,   -49,   -48,   -49,   -41,    -7,   -49,   -49,
     -49,   -49,   -49,    -8,     5,     7,     0,    -1,   -38,   -49,
     -49,   -49,   -49,   -49,   -49,   -49,   -49,   -49
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    24,    25,    26,    27,    28,    29,    82,    30,    31,
      32,    33,    34,    61,    35,    62,    36,    54,    37,    58,
      59,   135,    38,    39,    40,    41,    42,    43,    44,    91,
      94,    98,    45,    46,    47,    48,    49,    50
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      56,    55,    60,    57,    66,   101,   104,    99,    51,   105,
     106,    63,    52,     9,    85,    86,    87,    88,     2,     3,
       4,     5,    53,    74,    75,    18,    95,    96,    97,    76,
      77,    78,    79,    80,    81,    64,    83,    84,    92,    93,
      65,    67,    68,    70,   102,   103,    89,    90,    69,    17,
      71,    72,   -40,   108,    73,    18,    74,    75,   100,   111,
     118,   112,    76,    77,    78,    79,    80,    81,   110,   107,
     120,   123,   121,   125,   113,   130,   122,   127,    83,   133,
     128,   132,   131,     1,     2,     3,     4,     5,    66,   115,
     114,   116,   119,   117,     0,   109,     0,     0,     0,    55,
     124,     0,     6,     7,     8,     9,     0,    10,    11,    12,
      13,    14,   129,    15,    16,    17,     0,    18,   126,     0,
      19,     0,   134,     0,    20,    21,     0,     0,     0,    22,
      23,     1,     2,     3,     4,     5,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       6,     7,     8,     9,     0,    10,    11,    12,    13,    14,
       0,    15,    16,    17,     0,    18,     0,     0,    19,     0,
       0,     0,    20,    21,     0,     0,     0,    22,    23,     2,
       3,     4,     5,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    11,    12,    13,    14,     0,    15,    16,
      17,     0,     0,     0,     0,    19,     0,     0,     0,    20,
      21,     0,     0,     0,    22,    23
};

static const yytype_int16 yycheck[] =
{
      10,     9,    10,    10,    25,    51,    54,    45,    34,    57,
      58,    19,    40,    25,    10,    11,    12,    13,     4,     5,
       6,     7,    40,     8,     9,    37,    46,    47,    48,    14,
      15,    16,    17,    18,    19,    32,    20,    21,    44,    45,
       0,    22,    22,    22,    52,    53,    42,    43,    40,    35,
      22,    22,    37,    61,    22,    37,     8,     9,    35,    50,
      98,    69,    14,    15,    16,    17,    18,    19,    41,    39,
      14,    26,    41,    36,    82,   123,    41,    41,    20,   125,
      36,    39,   123,     3,     4,     5,     6,     7,   109,    84,
      83,    91,   100,    94,    -1,    62,    -1,    -1,    -1,   107,
     107,    -1,    22,    23,    24,    25,    -1,    27,    28,    29,
      30,    31,   120,    33,    34,    35,    -1,    37,    38,    -1,
      40,    -1,   132,    -1,    44,    45,    -1,    -1,    -1,    49,
      50,     3,     4,     5,     6,     7,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      22,    23,    24,    25,    -1,    27,    28,    29,    30,    31,
      -1,    33,    34,    35,    -1,    37,    -1,    -1,    40,    -1,
      -1,    -1,    44,    45,    -1,    -1,    -1,    49,    50,     4,
       5,     6,     7,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    28,    29,    30,    31,    -1,    33,    34,
      35,    -1,    -1,    -1,    -1,    40,    -1,    -1,    -1,    44,
      45,    -1,    -1,    -1,    49,    50
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,     6,     7,    22,    23,    24,    25,
      27,    28,    29,    30,    31,    33,    34,    35,    37,    40,
      44,    45,    49,    50,    52,    53,    54,    55,    56,    57,
      59,    60,    61,    62,    63,    65,    67,    69,    73,    74,
      75,    76,    77,    78,    79,    83,    84,    85,    86,    87,
      88,    34,    40,    40,    68,    74,    55,    68,    70,    71,
      74,    64,    66,    74,    32,     0,    54,    22,    22,    40,
      22,    22,    22,    22,     8,     9,    14,    15,    16,    17,
      18,    19,    58,    20,    21,    10,    11,    12,    13,    42,
      43,    80,    44,    45,    81,    46,    47,    48,    82,    79,
      35,    61,    74,    74,    65,    65,    65,    39,    74,    53,
      41,    50,    74,    74,    76,    75,    77,    78,    79,    74,
      14,    41,    41,    26,    68,    36,    38,    41,    36,    74,
      65,    67,    39,    61,    55,    72
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    51,    52,    53,    53,    54,    54,    54,    54,    54,
      54,    54,    55,    55,    55,    56,    56,    57,    58,    58,
      58,    58,    58,    58,    59,    60,    60,    61,    61,    62,
      62,    62,    62,    64,    63,    66,    65,    67,    67,    67,
      68,    69,    69,    70,    71,    72,    73,    73,    74,    74,
      75,    75,    76,    76,    77,    77,    78,    78,    79,    79,
      80,    80,    80,    80,    80,    80,    81,    81,    82,    82,
      82,    83,    83,    83,    84,    84,    84,    85,    85,    85,
      85,    85,    86,    86,    86,    86,    87,    88
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     1,     2,     2,     2,     2,     2,
       2,     1,     1,     1,     1,     3,     5,     3,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     1,     1,     1,
       1,     1,     1,     0,     5,     0,     4,     3,     5,     5,
       1,     3,     3,     5,     1,     1,     4,     4,     3,     1,
       3,     1,     3,     1,     3,     1,     3,     1,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     3,
       1,     1,     1,     1,     1,     3,     4,     4
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 81 "compiler_hw2.y" /* yacc.c:1646  */
    { dump_symbol(scope);}
#line 1431 "y.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 103 "compiler_hw2.y" /* yacc.c:1646  */
    { insert_symbol((yyvsp[-1].str),(yyvsp[0].i_val),scope); 
		    }
#line 1438 "y.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 105 "compiler_hw2.y" /* yacc.c:1646  */
    { insert_symbol((yyvsp[-3].str),(yyvsp[-2].i_val),scope); 
		    }
#line 1445 "y.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 109 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("%s\n",(yyvsp[-1].str)); }
#line 1451 "y.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 112 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"ASSIGN"); }
#line 1457 "y.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 113 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"ADD_ASSIGN"); }
#line 1463 "y.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 114 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"SUB_ASSIGN"); }
#line 1469 "y.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 115 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"MUL_ASSIGN"); }
#line 1475 "y.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 116 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"QUO_ASSIGN"); }
#line 1481 "y.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 117 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"REM_ASSIGN"); }
#line 1487 "y.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 124 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("INC\n");}
#line 1493 "y.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 125 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("DEC\n"); }
#line 1499 "y.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 128 "compiler_hw2.y" /* yacc.c:1646  */
    {(yyval.i_val)=(yyvsp[0].i_val);}
#line 1505 "y.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 129 "compiler_hw2.y" /* yacc.c:1646  */
    {(yyval.i_val)=(yyvsp[0].i_val);}
#line 1511 "y.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 132 "compiler_hw2.y" /* yacc.c:1646  */
    { (yyval.i_val) = 0; }
#line 1517 "y.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 133 "compiler_hw2.y" /* yacc.c:1646  */
    { (yyval.i_val) = 1; }
#line 1523 "y.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 134 "compiler_hw2.y" /* yacc.c:1646  */
    { (yyval.i_val) = 2; }
#line 1529 "y.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 135 "compiler_hw2.y" /* yacc.c:1646  */
    {(yyval.i_val) = 3; }
#line 1535 "y.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 138 "compiler_hw2.y" /* yacc.c:1646  */
    { flag=1; }
#line 1541 "y.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 138 "compiler_hw2.y" /* yacc.c:1646  */
    { (yyval.i_val) = (yyvsp[0].i_val)+4; }
#line 1547 "y.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 141 "compiler_hw2.y" /* yacc.c:1646  */
    { scope++;  }
#line 1553 "y.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 141 "compiler_hw2.y" /* yacc.c:1646  */
    { dump_symbol(scope); if(scopes_index_[scope]>0) scopes_index_[scope]--; scope--; }
#line 1559 "y.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 165 "compiler_hw2.y" /* yacc.c:1646  */
    {
                                        if(print_type==0) printf("PRINT bool\n");
                                        else if(print_type==1) printf("PRINT int32\n");
                                        else if(print_type==2) printf("PRINT float32\n");
                                        else printf("PRINT string\n");
				 }
#line 1570 "y.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 171 "compiler_hw2.y" /* yacc.c:1646  */
    { 
					if(print_type==0) printf("PRINTLN bool\n");
					else if(print_type==1) printf("PRINTLN int32\n");
					else if(print_type==2) printf("PRINTLN float32\n");
					else printf("PRINTLN string\n");
					flag=0;
				 }
#line 1582 "y.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 180 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("LOR\n"); print_type=0; }
#line 1588 "y.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 185 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("LAND\n"); print_type=0; }
#line 1594 "y.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 190 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("%s\n",(yyvsp[-1].str));  }
#line 1600 "y.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 194 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("%s\n",(yyvsp[-1].str)); }
#line 1606 "y.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 198 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("%s\n",(yyvsp[-1].str)); }
#line 1612 "y.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 204 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("%s\n",(yyvsp[-1].str)); }
#line 1618 "y.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 214 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"EQL"); }
#line 1624 "y.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 215 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"NEQ"); }
#line 1630 "y.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 216 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"LSS"); }
#line 1636 "y.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 217 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"LEQ"); }
#line 1642 "y.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 218 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"GTR"); }
#line 1648 "y.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 219 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"GEQ"); }
#line 1654 "y.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 222 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"ADD"); print_type=1;}
#line 1660 "y.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 223 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"SUB"); print_type=1;}
#line 1666 "y.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 226 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"MUL"); }
#line 1672 "y.tab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 227 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"QUO"); print_type=2;}
#line 1678 "y.tab.c" /* yacc.c:1646  */
    break;

  case 70:
#line 228 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"REM"); print_type=2;}
#line 1684 "y.tab.c" /* yacc.c:1646  */
    break;

  case 71:
#line 231 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"POS"); }
#line 1690 "y.tab.c" /* yacc.c:1646  */
    break;

  case 72:
#line 232 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"NEG"); }
#line 1696 "y.tab.c" /* yacc.c:1646  */
    break;

  case 73:
#line 233 "compiler_hw2.y" /* yacc.c:1646  */
    { strcpy((yyval.str),"NOT"); }
#line 1702 "y.tab.c" /* yacc.c:1646  */
    break;

  case 78:
#line 242 "compiler_hw2.y" /* yacc.c:1646  */
    { int addr_= lookup_symbol((yyvsp[0].str)); printf("IDENT (name=%s, address=%d)\n",(yyvsp[0].str),addr_);printf("%d\n",scopes_index[scope]); }
#line 1708 "y.tab.c" /* yacc.c:1646  */
    break;

  case 80:
#line 244 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("TRUE\n"); }
#line 1714 "y.tab.c" /* yacc.c:1646  */
    break;

  case 81:
#line 245 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("FALSE\n"); }
#line 1720 "y.tab.c" /* yacc.c:1646  */
    break;

  case 82:
#line 248 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("INT_LIT %d\n",(yyvsp[0].i_val)); 
		if(flag==0)
		{
			print_type=1;
			conversion_t=1;
		}
	      }
#line 1732 "y.tab.c" /* yacc.c:1646  */
    break;

  case 83:
#line 255 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("FLOAT_LIT %f\n",(yyvsp[0].f_val));  
		  if(flag==0)
		  {
		    print_type=2;
		    conversion_t=2;
   		  } 		
		  
                }
#line 1745 "y.tab.c" /* yacc.c:1646  */
    break;

  case 84:
#line 263 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("BOOL_LIT %d\n",(yyvsp[0].i_val));  
                 if(flag==0)
		 {
		   print_type=0;
		   conversion_t=0;
		 }
 }
#line 1757 "y.tab.c" /* yacc.c:1646  */
    break;

  case 85:
#line 270 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("STRING_LIT %s\n",(yyvsp[-1].s_val));
                           if(flag==0)
			   {	 
			      print_type=3;
			      conversion_t=3;
			   }
}
#line 1769 "y.tab.c" /* yacc.c:1646  */
    break;

  case 87:
#line 282 "compiler_hw2.y" /* yacc.c:1646  */
    {
				int t1=(yyvsp[-3].i_val);
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
#line 1788 "y.tab.c" /* yacc.c:1646  */
    break;


#line 1792 "y.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 297 "compiler_hw2.y" /* yacc.c:1906  */


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
