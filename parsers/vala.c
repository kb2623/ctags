/*
*   Copyright (c) 2005 - 2015, The GNOME Project
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for property list defined
*   in https://wiki.gnome.org/Projects/Vala/Hacking.
*/

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
 * @brief findTagsVala ::= using_directive* namespace_member*
 */
static void findTagsVala(void) {

}

// PARSING FUNCTIONS

/**
 * @brief using_directive ::= "using" symbol [ "," symbol ]* ";"
 */
static void using_directive(void) {
	//TODO
}

/**
 * @brief symbol ::= symbol_part [ "." symbol_part ]*
 */
static void symbol(void) {
	//TODO
}

/**
 * @brief symbol_part ::= ( "global::" identifier ) | identifier
 */
static void symbol_part(void) {
	//TODO
}

/**
 * @brief namespace_member ::= [ attributes ]
 *                      ( namespace_declaration |
 *                        class_declaration |
 *                        interface_declaration |
 *                        struct_declaration |
 *                        enum_declaration |
 *                        errordomain_declaration |
 *                        method_declaration |
 *                        field_declaration |
 *                        constant_declaration )
 */
static void namespace_member(void) {
	//TODO
}

/**
 * @brief attributes ::= attribute*
 */
static void attributes(void) {
	//TODO
}

/**
 * @brief attribute ::= "[" identifier [ attribute_arguments ] "]"
 */
static void attribute_vala(void) {
	//TODO
}

/**
 * @brief attribute_arguments ::= "(" attribute_argument [ "," attribute_argument ]* ")"
 */
static void attribute_arguments(void) {
	//TODO
}

/**
 * @brief attribute_argument ::= identifier "=" expression
 */
static void attribute_argument(void) {
	//TODO
}

/**
 * @brief expression lambda_expression | ( conditional_expression [ assignment_operator expression ] )
 */
static void expression(void) {
	//TODO
}

/**
 * @brief assignment_operator ::= "=" | "+=" | "-=" | "|=" | "&=" | "^=" | "/=" | "*=" | "%=" | "<<=" | ">>="
 */
static void assignment_operator(void) {
	//TODO
}

/**
 * @brief conditional_expression ::= coalescing_expression [ "?" expression ":" expression ]
 */
static void conditional_expression(void) {
	//TODO
}

/**
 * @brief coalescing_expression ::= conditional_or_expression [ "??" coalescing_expression ]
 */
static void coalescing_expression(void) {
	//TODO
}

/**
 * @brief conditional_or_expression conditional_and_expression [ "||" conditional_and_expression ]
 */
static void conditional_or_expression(void) {
	//TODO
}

/**
 * @brief conditional_and_expression ::= in_expression [ "&&" in_expression ]
 */
static void conditional_and_expression(void) {
	//TODO
}

/**
 * @brief in_expression ::= inclusive_or_expression [ "in" inclusive_or_expression ]
 */
static void in_expression(void) {
	//TODO
}

/**
 * @brief inclusive_or_expression ::= exclusive_or_expression [ "|" exclusive_or_expression ]
 */
static void inclusive_or_expression(void) {
	//TODO
}

/**
 * @brief exclusive_or_expression ::= and_expression [ "^" and_expression ]
 */
static void exclusive_or_expression(void) {
	//TODO
}

/**
 * @brief and_expression ::= equality_expression [ "&" equality_expression ]
 */
static void and_expression(void) {
	//TODO
}

/**
 * @brief equality_expression ::= relational_expression [ ( "==" | "!=" ) relational_expression ]*
 */
static void equality_expression(void) {
	//TODO
}

/**
 * @brief relational_expression ::= shift_expression [ ( "<" | "<=" | ">" | ">=" ) shift_expression ) | ( "is" type ) | ( "as" type ) ]*
 */
static void relational_expression(void) {
	//TODO
}

/**
 * @brief type ::= ( "void" [ "*" ]* ) | ( [ "dynamic" ] [ "unowned" ] symbol [ type_arguments ] [ "*" ]* [ "?" ] array_type* )
 */
static void type(void) {
	//TODO
}

/**
 * @brief type_weak ::= ( "void" [ "*" ]* ) | ( [ "dynamic" ] [ "unowned" | "weak" ] symbol [ type_arguments ] [ "*" ]* [ "?" ] array_type* )
 */
static void type_weak(void) {
	//TODO
}

/**
 * @brief array_type ::= "[" array_size "]" [ "?" ]
 */
static void array_type(void) {
	//TODO
}

/**
 * @brief shift_expression ::= additive_expression [ ( "<<" | ">>" ) additive_expression ]*
 */
static void shift_expression(void) {
	//TODO
}

/**
 * @brief additive_expression multiplicative_expression [ ( "+" | "-" ) multiplicative_expression ]*
 */
static void additive_expression(void) {
	//TODO
}

/**
 * @brief multiplicative_expression unary_expression [ ( "*" | "/" | "%" ) unary_expression ]*
 */
static void multiplicative_expression(void) {
	//TODO
}

/**
 * @brief unary_expression ( unary_operator unary_expression ) | ( "(" ( "owned" | "void" | "dynamic" | "!" | type ) ")" unary_expression ) | primary_expression
 */
static void unary_expression(void) {
	//TODO
}

/**
 * @brief unary_operator ::= "+" | "-" | "!" | "~" | "++" | "--" | "*" | "&" | "(owned)" | "(void)" | "(dynamic)" | "(!)"
 */
static void unary_operator(void) {
	//TODO
}

/**
 * @brief primary_expression ::= ( literal | initializer | tuple | template | open_regex_literal | this_access | base_access | object_or_array_creation_expression | yield_expression | sizeof_expression | typeof_expression | simple_name ) [ member_access | pointer_member_access | method_call | element_access | post_increment_expression | post_decrement_expression ]*
 */
static void primary_expression(void) {
	//TODO
}

/**
 * @brief literal ::= "true" | "false" | "null" | integer_literal | real_literal | character_literal | regex_literal | string_literal | template_string_literal | verbatim_string_literal
 */
static void literal(void) {
	//TODO
}

/**
 * @brief initializer ::= "{" argument [ "," argument ]* "}"
 */
static void initializer(void) {
	//TODO
}

/**
 * @brief arguments ::= argument [ "," argument ]*
 */
static void arguments(void) {
	//TODO
}

/**
 * @brief argument ::= "ref" expression | "out" expression | expression | identifier [ ":" expression ]
 */
static void argument(void) {
	//TODO
}

/**
 * @brief tuple ::= "(" expression [ "," expression ]* ")"
 */
static void tuple(void) {
	//TODO
}

/**
 * @brief _template ::= '@"' [ expression "," ]* '"'
 */
static void _template(void) {
	//TODO
}

/**
 * @brief open_regex_literal ::= "/" literal
 */
static void open_regex_literal(void) {
	//TODO
}

/**
 * @brief this_access ::= "this"
 */
static void this_access(void) {
	//TODO
}

/**
 * @brief base_access ::= "base"
 */
static void base_access(void) {
	//TODO
}

/**
 * @brief object_or_array_creation_expression := "new" member ( object_creation_expression | array_creation_expression )
 */
static void object_or_array_creation_expression(void) {
	//TODO
}

/**
 * @brief object_creation_expression ::= "(" [ arguments ] ")" [ object_initializer ]
 */
static void object_creation_expression(void) {
	//TODO
}

/**
 * @brief object_initializer ::= "{" member_initializer [ "," member_initializer ] "}"
 */
static void object_initializer(void) {
	//TODO
}

/**
 * @brief member_initializer ::= identifier "=" expression
 */
static void member_initializer(void) {
	//TODO
}

/**
 * @brief array_creation_expression ::= [ "[" "]" ]* [ "[" [ array_size ] "]" ] [ initializer ]
 */
static void array_creation_expression(void) {
	//TODO
}

/**
 * @brief array_size ::= expression [ "," expression ]*
 */
static void array_size(void) {
	//TODO
}

/**
 * @brief member ::= member_part [ "." member_part ]*
 */
static void member(void) {
	//TODO
}

/**
 * @brief member_part ::= ( "global::" identifier || identifier ) [ type_arguments ]
 */
static void member_part(void) {
	//TODO
}

/**
 * @brief type_arguments ::= "<" type [ "," type ]* ">"
 */
static void type_arguments(void) {
	//TODO
}

/**
 * @brief yield_expression ::= "yield" [ base_access "." ] member method_call
 */
static void yield_expression(void) {
	//TODO
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
