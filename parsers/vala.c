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
	// Selection
	KEYWORD_if, KEYWORD_else, KEYWORD_switch, KEYWORD_case, KEYWORD_default,
	// Iteration
	KEYWORD_do, KEYWORD_while, KEYWORD_for, KEYWORD_foreach, KEYWORD_in,
	// Jumping
	KEYWORD_break, KEYWORD_continue, KEYWORD_return,
	// Exception Handling
	KEYWORD_try, KEYWORD_catch, KEYWORD_finally, KEYWORD_throw,
	// Synchronization
	KEYWORD_lock,
	// Type declaration
	KEYWORD_class, KEYWORD_interface, KEYWORD_struct, KEYWORD_enum, KEYWORD_delegate, KEYWORD_errordomain,
	// Type Modifiers
	KEYWORD_const, KEYWORD_weak, KEYWORD_unowned, KEYWORD_dynamic,
	// Modifiers
	KEYWORD_abstract, KEYWORD_virtual, KEYWORD_override, KEYWORD_signal, KEYWORD_extern, KEYWORD_static, KEYWORD_async, KEYWORD_inline, KEYWORD_new,
	// Access Modifiers
	KEYWORD_public, KEYWORD_private, KEYWORD_protected, KEYWORD_internal,
	// Method Parameters
	KEYWORD_out, KEYWORD_ref,
	// Method Contract Keywords
	KEYWORD_throws, KEYWORD_requires, KEYWORD_ensures,
	// Namespaces
	KEYWORD_namespace, KEYWORD_using,
	// Operator Keywords
	KEYWORD_as, KEYWORD_is, KEYWORD_in, KEYWORD_delete, KEYWORD_sizeof, KEYWORD_typeof,
	// Access Keywords
	KEYWORD_this, KEYWORD_base,
	// Literal Keywords
	KEYWORD_null, KEYWORD_true, KEYWORD_false,
	// Property Context
	KEYWORD_get, KEYWORD_set, KEYWORD_construct, KEYWORD_default, KEYWORD_value,
	// Other
	KEYWORD_void, KEYWORD_var, KEYWORD_yield, KEYWORD_global, KEYWORD_owned,
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
	{ TRUE,		'c',		"class",				"classes"		},
	{ TRUE,		'i',		"interface",		"interfaces"	},
	{ TRUE,		'e',		"enum",				"enums"			},
	{ TRUE,		'c',		"constructor",		"constructos"	},
	{ TRUE,		'v',		"field",				"fields"			},
	{ TRUE,		'f',		"function",			"functions"		},
};

static const keywordTable ValaKeywordsTable[] = {
	{ "if",				KEYWORD_if			},
	{ "else",			KEYWORD_else		},
	{ "switch",			KEYWORD_switch		},
	{ "case",			KEYWORD_case		},
	{ "default",		KEYWORD_default	},
	{ "do",				KEYWORD_do			},
	{ "while",			KEYWORD_while		},
	{ "for",				KEYWORD_for			},
	{ "foreach",		KEYWORD_foreach	},
	{ "in",				KEYWORD_in			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
	{ "for",				KEYWORD_for			},
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
