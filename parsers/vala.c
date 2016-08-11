/**
  * @author Klemen Berkovic
  */
#include "general.h"

#include "entry.h"
#include "parse.h"
#include "read.h"
#include "types.h"

#include "vstring.h"
#include "routines.h"

//DATA DECLARATIONS

typedef enum eKeywordId {
	KEYWORD_if,
	KEYWORD_else,
	KEYWORD_switch,
	KEYWORD_case,
	KEYWORD_default,
	KEYWORD_do,
	KEYWORD_while,
	KEYWORD_for,
	KEYWORD_foreach,
	KEYWORD_in,
	KEYWORD_break,
	KEYWORD_continue,
	KEYWORD_return,
	KEYWORD_try,
	KEYWORD_catch,
	KEYWORD_finally,
	KEYWORD_throw,
	KEYWORD_lock,
	KEYWORD_class,
	KEYWORD_interface,
	KEYWORD_struct,
	KEYWORD_enum,
	KEYWORD_delegate,
	KEYWORD_errordomain,
	KEYWORD_const,
	KEYWORD_weak,
	KEYWORD_unowned,
	KEYWORD_dynamic,
	KEYWORD_abstract,
	KEYWORD_virtual,
	KEYWORD_override,
	KEYWORD_signal,
	KEYWORD_extern,
	KEYWORD_static,
	KEYWORD_async,
	KEYWORD_inline,
	KEYWORD_new,
	KEYWORD_public,
	KEYWORD_private,
	KEYWORD_protected,
	KEYWORD_internal,
	KEYWORD_out,
	KEYWORD_ref,
	KEYWORD_throws,
	KEYWORD_requires,
	KEYWORD_ensures,
	KEYWORD_namespace,
	KEYWORD_using,
	KEYWORD_as,
	KEYWORD_is,
	KEYWORD_in,
	KEYWORD_delete,
	KEYWORD_sizeof,
	KEYWORD_typeof,
	KEYWORD_this,
	KEYWORD_base,
	KEYWORD_null,
	KEYWORD_true,
	KEYWORD_false,
	KEYWORD_get,
	KEYWORD_set,
	KEYWORD_construct,
	KEYWORD_default,
	KEYWORD_value,
	KEYWORD_void,
	KEYWORD_var,
	KEYWORD_yield,
	KEYWORD_global,
	KEYWORD_owned,
} keywordId;

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
} TokenType;

//DATA DEFINITIONS

static int Lang_vala;

typedef enum eValaKind {
	K_package,
	K_annotation,
	K_class,
	K_interface,
	K_enum,
	K_constructors,
	K_field,
	K_function,
} ValaKind;

static kindOption ValaKinds[] = {
	{ TRUE,		'p',		"package",			"packages"		},
	{ TRUE,		'a',		"annotation",		"annotations"	},
	{ TRUE,		's',		"struct",			"stuctus"		},
	{ TRUE,		'c',		"class",				"classes"		},
	{ TRUE,		'i',		"interface",		"interfaces"	},
	{ TRUE,		'e',		"enum",				"enums"			},
	{ TRUE,		'c',		"constructor",		"constructos"	},
	{ TRUE,		'v',		"field",				"fields"			},
	{ TRUE,		'f',		"function",			"functions"		},
};

static const keywordTable ValaKeywordsTable[] = {
	{ "if",					KEYWORD_if				},
	{ "else",				KEYWORD_else			},
	{ "switch",				KEYWORD_switch			},
	{ "case",				KEYWORD_case			},
	{ "default",			KEYWORD_default		},
	{ "do",					KEYWORD_do				},
	{ "while",				KEYWORD_while			},
	{ "for",					KEYWORD_for				},
	{ "foreach",			KEYWORD_foreach		},
	{ "in",					KEYWORD_in				},
	{ "break",				KEYWORD_break			},
	{ "continue",			KEYWORD_continue		},
	{ "return",				KEYWORD_return			},
	{ "try",					KEYWORD_try				},
	{ "catch",				KEYWORD_catch			},
	{ "finally",			KEYWORD_finally		},
	{ "throw",				KEYWORD_throw			},
	{ "lock",				KEYWORD_lock			},
	{ "class",				KEYWORD_class			},
	{ "interface",			KEYWORD_interface		},
	{ "struct",				KEYWORD_struct			},
	{ "enum",				KEYWORD_enum			},
	{ "delegate",			KEYWORD_delegate		},
	{ "errordomain",		KEYWORD_errordomain	},
	{ "const",				KEYWORD_const			},
	{ "weak",				KEYWORD_weak			},
	{ "unowned",			KEYWORD_unowned		},
	{ "dynamic",			KEYWORD_dynamic		},
	{ "abstract",			KEYWORD_abstract		},
	{ "virtual",			KEYWORD_virtual		},
	{ "override",			KEYWORD_override		},
	{ "signal",				KEYWORD_signal			},
	{ "extern",				KEYWORD_extern			},
	{ "static",				KEYWORD_static			},
	{ "async",				KEYWORD_async			},
	{ "inline",				KEYWORD_inline			},
	{ "new",					KEYWORD_new				},
	{ "public",				KEYWORD_public			},
	{ "private",			KEYWORD_private		},
	{ "protected",			KEYWORD_protected		},
	{ "internal",			KEYWORD_internal		},
	{ "out",					KEYWORD_out				},
	{ "ref",					KEYWORD_ref				},
	{ "throws",				KEYWORD_throws			},
	{ "requires",			KEYWORD_requires		},
	{ "ensures",			KEYWORD_ensures		},
	{ "namespace",			KEYWORD_namespace		},
	{ "using",				KEYWORD_using			},
	{ "as",					KEYWORD_as				},
	{ "is",					KEYWORD_is				},
	{ "in",					KEYWORD_in				},
	{ "delete",				KEYWORD_delete			},
	{ "sizeof",				KEYWORD_sizeof			},
	{ "typeof",				KEYWORD_typeof			},
	{ "this",				KEYWORD_this			},
	{ "base",				KEYWORD_base			},
	{ "null",				KEYWORD_null			},
	{ "true",				KEYWORD_true			},
	{ "false",				KEYWORD_false			},
	{ "get",					KEYWORD_get				},
	{ "set",					KEYWORD_set				},
	{ "construct",			KEYWORD_construct		},
	{ "default",			KEYWORD_default		},
	{ "value",				KEYWORD_value			},
	{ "void",				KEYWORD_void			},
	{ "var",					KEYWORD_var				},
	{ "yield",				KEYWORD_yield			},
	{ "global",				KEYWORD_global			},
	{ "owned",				KEYWORD_owned			},
};

// Lexer


// MAIN FUNCTIONS

/**
 * @brief initialize
 * @param language
 */
static void initialize(const langType language) {
	Lang_vala = language;
}

/**
 * @brief findTagsVala
 */
static void findTagsVala(void) {

}

// PARSER CREATION

extern parserDefinition * ValaParser(void) {
	static const char * const extensions[] = 	{"vala", "vapi", NULL};
	parserDefinition * def = parserNew("Vala");
	def->initialize = initialize;
	def->extensions = extensions;
	def->kinds = ValaKinds;
	def->kindCount = ARRAY_SIZE(ValaKinds);
	def->parser = findTagsVala;
	def->keywordTable = ValaKeywordsTable;
	def->keywordCount = ARRAY_SIZE(ValaKeywordsTable);
	def->useCork = TRUE;
	return def;
}
