#include "general.h"

#include "entry.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "types.h"

#include "vstring.h"
#include "routines.h"

/*
 * DATA DECLARATIONS
 */
typedef enum eKeywordId {
	KEYWORD_val, KEYWORD_var,
	KEYWORD_as,
	KEYWORD_typealias,
	KEYWORD_this, KEYWORD_super,
	KEYWORD_fun,
	KEYWORD_null,
	KEYWORD_true, KEYWORD_false,
	KEYWORD_is,
	KEYWORD_by,
	KEYWORD_in, KEYWORD_out,
	KEYWORD_throw, KEYWORD_return, KEYWORD_continue, KEYWORD_break,
	KEYWORD_object, KEYWORD_class, KEYWORD_interface, KEYWORD_annotation, KEYWORD_enum,
	KEYWORD_open, KEYWORD_final, KEYWORD_abstract, KEYWORD_override, KEYWORD_data,
	KEYWORD_if, KEYWORD_else, KEYWORD_when,
	KEYWORD_for, KEYWORD_do, KEYWORD_while,
	KEYWORD_typeof,
	KEYWORD_yield,
	KEYWORD_import, KEYWORD_package,
	KEYWORD_init, KEYWORD_companion, KEYWORD_get, KEYWORD_set, KEYWORD_constructor, KEYWORD_dynamic,
	KEYWORD_try, KEYWORD_catch, KEYWORD_finally,
	KEYWORD_public, KEYWORD_protedted, KEYWORD_private, KEYWORD_internal,
} KeywordId;

typedef enum eTokenType {
	TOKEN_undefined,
	TOKEN_eof,
	TOKEN_indetifier,
	TOKEN_keyword,
	TOKEN_digit,
	TOKEN_string,
	TOKEN_newline, 			/* \n	*/
	TOKEN_semicolon,			/* ;	*/
	TOKEN_exclamationMark, 	/* !	*/
	TOKEN_lParenthesis,		/* (	*/
	TOKEN_rParenthesis,		/* )	*/
	TOKEN_lCurlyBracket,		/* {	*/
	TOKEN_rCurlyBracket,		/* }	*/
	TOKEN_lSquareBracket,	/* [	*/
	TOKEN_rSquareBracket,	/* ]	*/
	TOKEN_verticalBar,		/*	|	*/
	TOKEN_backslash,			/* \	*/
	TOKEN_slach,				/* /	*/
	TOKEN_lessThanSign,		/* <	*/
	TOKEN_greaterThanSign,	/* >	*/
	TOKEN_equalSign,			/* =	*/
	TOKEN_questionMark,		/* ?	*/
	TOKEN_atSign,				/* @	*/
	TOKEN_comma,				/* ,	*/
	TOKEN_dot,					/* .	*/
	TOKEN_quotationMark,		/* "	*/
	TOKEN_apostrophe,			/* '	*/
	//TODO dodaj nove vrste zetonov
} TokenType;

typedef struct sTokenInfo {
		TokenType		type;
		KeywordId		keyword;
		vString *		scope;
		unsigned long	lineNumber;
		MIOPos			filePosition;
} TokenInfo;

/*
 * DATA DEFINITIONS
 */
static int Lang_kotlin;
static vString * scope;

static roleDesc KotlinPackageRoles [] = {
	{ TRUE, "imported", "imported package"	},
};

static kindOption KotlinKinds[] = {
	{ TRUE,	'a',	"annotations",	"annotations declarations"	},
	{ TRUE,	'c',	"class",			"classes"						},
	{ TRUE,	'e',	"enum",			"enum types"					},
	{ TRUE,	'v',	"field",			"fields"							},
	{ TRUE,	'i',	"interface",	"interfaces"					},
	{ FALSE,	'l',	"local",			"local varabiles"				},
	{ TRUE,	'f',	"function",		"functions"						},
	{ TRUE,	'p',	"package",		"packages"						},
};

static const keywordTable KotlinKeywordsTable [] = {
	{ "val",				KEYWORD_val				},
	{ "var",				KEYWORD_var				},
	{ "package",		KEYWORD_package		},
	{ "as",				KEYWORD_as				},
	{ "typealias",		KEYWORD_typealias		},
	{ "class",			KEYWORD_class			},
	{ "this",			KEYWORD_this			},
	{ "super",			KEYWORD_super			},
	{ "fun",				KEYWORD_fun				},
	{ "for",				KEYWORD_for				},
	{ "null",			KEYWORD_null			},
	{ "true",			KEYWORD_true			},
	{ "false",			KEYWORD_false			},
	{ "is",				KEYWORD_is				},
	{ "in",				KEYWORD_in				},
	{ "throw",			KEYWORD_throw			},
	{ "return",			KEYWORD_return			},
	{ "break",			KEYWORD_break			},
	{ "continue",		KEYWORD_continue		},
	{ "object",			KEYWORD_object			},
	{ "if",				KEYWORD_if				},
	{ "try",				KEYWORD_try				},
	{ "else",			KEYWORD_else			},
	{ "while",			KEYWORD_while			},
	{ "do",				KEYWORD_do				},
	{ "when",			KEYWORD_when			},
	{ "interface",		KEYWORD_interface		},
	{ "typeof",			KEYWORD_typeof			},
	{ "yield",			KEYWORD_yield			},
	{ "open",			KEYWORD_open			},
	{ "annotation",	KEYWORD_annotation	},
	{ "import",			KEYWORD_import			},
	{ "init",			KEYWORD_init			},
	{ "companion",		KEYWORD_companion		},
	{ "get",				KEYWORD_get				},
	{ "set",				KEYWORD_set				},
	{ "constructor",	KEYWORD_constructor	},
	{ "dynamic",		KEYWORD_dynamic		},
	{ "catch",			KEYWORD_catch			},
	{ "finally",		KEYWORD_finally		},
	{ "abstract",		KEYWORD_abstract		},
	{ "override",		KEYWORD_override		},
	{ "enum",			KEYWORD_enum			},
	{ "out",				KEYWORD_out				},
	{ "private",		KEYWORD_private		},
	{ "protected",		KEYWORD_protedted		},
	{ "public",			KEYWORD_public			},
	{ "internal",		KEYWORD_internal		},
	{ "by",				KEYWORD_by				},
	{ "data",			KEYWORD_data			},
};

/*
 * FUNCTION DEFINITIONS for TokenInfo
 */
static TokenInfo * newToken(void) {
	TokenInfo * const token = xMalloc(1, TokenInfo);
	token->type 			= TOKEN_undefined;
	token->keyword 		= KEYWORD_NONE;
	token->scope			= vStringNew();
	token->lineNumber		= getInputLineNumber();
	token->filePosition	= getInputFilePosition();
	return token;
}

static void deleteToken(TokenInfo * const token) {
	vStringDelete(token->scope);
	eFree(token);
}

static void copyToken(TokenInfo * const dest, TokenInfo * const src) {
	dest->type 				= src->type;
	dest->keyword			= src->keyword;
	dest->scope				= src->scope;
	dest->lineNumber		= src->lineNumber;
	dest->filePosition	= src->filePosition;
}

static boolean isEOF(TokenInfo * const token) {
	return token->type == TOKEN_eof;
}

/*
 * LEXER
 */
static void readToken(TokenInfo * const token) {
	token->type = TOKEN_undefined;
	token->keyword = KEYWORD_NONE;
	vStringClear(token->stirng);

	//TODO
}

/*
 * FUNCTION DEFINITIONS
 */
static void initialize(const langType language) {
	Lang_kotlin = language;
}

static void findKotlinTags(void) {
	TokenInfo * const token = newToken();
	parseKotlinFile(token);
	deleteToken(token);
}

static void parseKotlinFile(TokenInfo * const token) {
	do {
		readToken(token);
		//TODO
	} while (!isEOF(token));
}

/*
 * MAIN FUNCTION
 */
extern parserDefinition *KotlinParser (void) {
	static const char *const extensions[] = {"kt", NULL};
	parserDefinition *def = parserNew("Kotlin");
	def->initialize = initialize;
	def->extensions = extensions;
	def->kinds = KotlinKinds;
	def->kindCount = ARRAY_SIZE(KotlinKinds);
	def->parser = findKotlinTags;
	def->keywordTable = KotlinKeywordsTable;
	def->keywordCount = ARRAY_SIZE(KotlinKeywordsTable);
	return def;
}
