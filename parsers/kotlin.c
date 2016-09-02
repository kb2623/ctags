/*
*   Copyright (c) 2000 - 2016, JetBrains
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for property list defined
*   in http://kotlinlang.org/docs/reference/grammar.html.
*/
/**
 * @author Klemen Berkovic
 */
#include "general.h"

#include "entry.h"
#include "parse.h"
#include "read.h"
#include "types.h"
#include "nestlevel.h"

#include "vstring.h"
#include "routines.h"

// DATA DECLARATIONS
typedef enum eKeywordId {
	KEYWORD_val,
	KEYWORD_var,
	KEYWORD_as,
	KEYWORD_typealias,
	KEYWORD_this,
	KEYWORD_super,
	KEYWORD_fun,
	KEYWORD_null,
	KEYWORD_true,
	KEYWORD_false,
	KEYWORD_is,
	KEYWORD_by,
	KEYWORD_in,
	KEYWORD_out,
	KEYWORD_throw,
	KEYWORD_return,
	KEYWORD_continue,
	KEYWORD_break,
	KEYWORD_object,
	KEYWORD_class,
	KEYWORD_interface,
	KEYWORD_annotation,
	KEYWORD_enum,
	KEYWORD_open,
	KEYWORD_final,
	KEYWORD_abstract,
	KEYWORD_override,
	KEYWORD_data,
	KEYWORD_if,
	KEYWORD_else,
	KEYWORD_when,
	KEYWORD_for,
	KEYWORD_do,
	KEYWORD_while,
	KEYWORD_typeof,
	KEYWORD_yield,
	KEYWORD_import,
	KEYWORD_package,
	KEYWORD_init,
	KEYWORD_companion,
	KEYWORD_get,
	KEYWORD_set,
	KEYWORD_constructor,
	KEYWORD_dynamic,
	KEYWORD_try,
	KEYWORD_catch,
	KEYWORD_finally,
	KEYWORD_public,
	KEYWORD_protedted,
	KEYWORD_private,
	KEYWORD_internal,
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
} TokenType;

// DATA DEFINITIONS
static int Lang_kotlin;
static NestingLevels * nesting = NULL;

typedef enum {
	K_package,
	K_annotation,
	K_class,
	K_object,
	K_interface,
	K_enum,
	K_constructors,
	K_field,
	K_function,
} KotlinKind;

static kindOption KotlinKinds[] = {
	{ TRUE,		'p',		"package",			"packages"		},
	{ TRUE,		'a',		"annotation",		"annotations"	},
	{ TRUE,		'c',		"class",				"classes"		},
	{ TRUE,		'o',		"object",			"objects"		},
	{ TRUE,		'i',		"interface",		"interfaces"	},
	{ TRUE,		'e',		"enum",				"enums"			},
	{ TRUE,		'c',		"constructor",		"constructos"	},
	{ TRUE,		'v',		"field",				"fields"			},
	{ TRUE,		'f',		"function",			"functions"		},
};

static const keywordTable KotlinKeywordsTable[] = {
	{ "val",				KEYWORD_val				},
	{ "var",				KEYWORD_var				},
	{ "as",				KEYWORD_as				},
	{ "typealias",		KEYWORD_typealias		},
	{ "this",			KEYWORD_this			},
	{ "super",			KEYWORD_super			},
	{ "fun",				KEYWORD_fun				},
	{ "null",			KEYWORD_null			},
	{ "true",			KEYWORD_true			},
	{ "false",			KEYWORD_false			},
	{ "is",				KEYWORD_is				},
	{ "by",				KEYWORD_by				},
	{ "in",				KEYWORD_in				},
	{ "out",				KEYWORD_out				},
	{ "throw",			KEYWORD_throw			},
	{ "return",			KEYWORD_return			},
	{ "continue",		KEYWORD_continue		},
	{ "break",			KEYWORD_break			},
	{ "object",			KEYWORD_object			},
	{ "class",			KEYWORD_class			},
	{ "interface",		KEYWORD_interface		},
	{ "annotation",	KEYWORD_annotation	},
	{ "enum",			KEYWORD_enum			},
	{ "open",			KEYWORD_open			},
	{ "final",			KEYWORD_final			},
	{ "abstract",		KEYWORD_abstract		},
	{ "override",		KEYWORD_override		},
	{ "data",			KEYWORD_data			},
	{ "if",				KEYWORD_if				},
	{ "else",			KEYWORD_else			},
	{ "when",			KEYWORD_when			},
	{ "for",				KEYWORD_for				},
	{ "do",				KEYWORD_do				},
	{ "while",			KEYWORD_while			},
	{ "typeof",			KEYWORD_typeof			},
	{ "yield",			KEYWORD_yield			},
	{ "import",			KEYWORD_import			},
	{ "package",		KEYWORD_package		},
	{ "init",			KEYWORD_init			},
	{ "companion",		KEYWORD_companion		},
	{ "get",				KEYWORD_get				},
	{ "set",				KEYWORD_set				},
	{ "constructor",	KEYWORD_constructor	},
	{ "dynamic",		KEYWORD_dynamic		},
	{ "try",				KEYWORD_try				},
	{ "catch",			KEYWORD_catch			},
	{ "finally",		KEYWORD_finally		},
	{ "public",			KEYWORD_public			},
	{ "private",		KEYWORD_private		},
	{ "protected",		KEYWORD_protedted		},
	{ "internal",		KEYWORD_internal		},
};

// Lexer

/**
 * @brief Digit (used by IntergerLiteral, HexDigit)
 * 	: ["0".."9"]
 * 	;
 * @return Full number
 */
static unsigned int Digit(void) {
	//TODO
	return 0;
}

/**
 * @brief IntergerLiteral (used by literalConstant)
 * 	: Digit+?
 * 	;
 * @return Integer
 */
static int IntergerLiteral(void) {
	//TODO
	return 0;
}

/**
 * @brief FloatLiteral (used by literalConstant)
 * 	: <Java double literal>
 * 	;
 * @return Float number
 */
static double FloatLiteral(void) {
	//TODO
	return 0;
}

/**
 * @brief HexDigit (used by RegularStringPart, HexdecimalLiteral)
 * 	: Digit | ["A".."F", "a".."f"]
 * 	;
 * @return Digit in hex
 */
static char * HexDigit(void) {
	//TODO
	return NULL;
}

/**
 * @brief HexdecimalLiteral (used by literalConstant)
 * 	: "0x" HexDigit+
 * 	;
 * @return  Digit in hex
 */
static char * HexdecimalLiteral(void) {
	//TODO
	return NULL;
}

/**
 * @brief CharacterLiteral (used by literalConstant)
 * 	: <character as in Java>
 * 	;
 * @return Character
 */
static char CharacterLiteral(void) {
	//TODO
	return 'a';
}

/**
 * @brief NoEscapeString (used by literalConstant)
 * 	: <"""-quoted string>
 * 	;
 * @return String
 */
static vString * NoEscapeString(void) {
	//TODO
	return NULL;
}

/**
 * @brief RegularStringPart (used by stringTemplateElement)
 * 	: <any character other than backslash, quote, $ or newline>
 * 	: ShortTemplateEntryStart
 * 	: "$" EscapeSequence
 * 	: UnicodeEscapeSequence | RegularEscapeSequence
 * 	: UnicodeEscapeSequence
  : "\u" HexDigit{4}
RegularEscapeSequence:
  : "\" <any character other than newline>
 * @return String
 */
static vString * RegularStringPart(void) {
	//TODO
	return NULL;
}

/**
 * @brief SimpleName (used by typeParameter, catchBlock, simpleUserType, atomicExpression, LabelName, package, packageHeader, _class, object, functionLiteral, infixFunctionCall, function, parameter, callableReference, FieldName, variableDeclarationEntry, stringTemplateElement, setter, enumEntry, import, valueArguments, unescapedAnnotation, typeConstraint)
 * 	: <Java identifiers>
 * 	: "'" <Java identifiers "'"
 * 	;
 * @return String
 */
static char * SimpleName(void) {
	//TODO
	return NULL;
}

// MAIN FUNCTIONS

static void preamble(void);
static void topLevelObject(void);
static void expression(void);

/**
 * @brief initialize
 * @param language
 */
static void initialize(const langType language) {
	Lang_kotlin = language;
	nesting = nestingLevelsNew(0);
}

/**
 * @brief findTagsKotlinFile
 * start
 * kotlinFile
 * 	: preamble topLevelObject*
 * 	;
 */
static void kotlinFile(void) {
	preamble();
	topLevelObject();
}

/**
 * @brief findTagsKotlinScript
 * start
 * script
 * 	: preamble expresion*
 * 	;
 */
static void script(void) {
	preamble();
	//TODO
	expression();//0..N
}

/* PARSING FUNCTIONS
 *
 * EBNF expressions
 * 	Operator | denotes alternative.
 * 	Operator * denotes iteration (zero or more).
 * 	Operator + denotes iteration (one or more).
 * 	Operator ? denotes option (zero or one).
 * 	Alpha { Beta } denotes a nonempty beta-separated list of alpha's.
 * 	Operator ``++’’ means that no space or comment allowed between operands.
 *
 * Semicolons
 * 	Kotlin provides “semicolon inference”: syntactically, subsentences
 * 	(e.g., statements, declarations etc) are separated by the pseudo-token SEMI,
 * 	which stands for “semicolon or newline”. In most cases,
 * 	there’s no need for semicolons in Kotlin code.
 */

/**
 * @brief preamble (used by script, kotlinFile)
 * 	: fileAnnotations? packageHeader? import*
 * 	;
 */
static void preamble(void) {
	//TODO
}

/**
 * @brief fileAnnotations (used by preamble)
 * 	: fileAnnotation*
 * 	;
 */
static void fileAnnotations(void) {
	//TODO
}

/**
 * @brief fileAnnotation (used by fileAnnotations)
 * 	: "@" "file" ":" ("[" annotationEntry+ "]" | annotationEntry)
 * 	;
 */
static void fileAnnotation(void) {
	//TODO
}

/**
 * @brief packageHeader (used by preamble)
 * 	: modifiers "package" SimpleName{"."} SEMI?
 * 	;
 */
static void packageHeader(void) {
	//TODO
}

/**
 * @brief import (used by preamble, package)
 * 	: "import" SimpleName{"."} ("." "*" | "as" SimpleName)? SEMI?
 * 	;
 */
static void import(void) {
	//TODO
}

/**
 * @brief topLevelObject (used by package, kotlinFile)
 * 	: package
 * 	: _class
 * 	: object
 * 	: function
 * 	: property
 * 	;
 */
static void topLevelObject(void) {
	//TODO
}

/**
 * @brief package (used by topLevelObject)
 * 	: "package" SimpleName{"."} "{"
 * 			import*
 * 			topLevelObject*
 * 		"}"
 * 	;
 */
static void package(void) {
	//TODO
}

//Classes

/**
 * @brief _class (used by memberDeclaration, declaration, topLevelObject)
 * 	: modifiers ("class" | "interface") SimpleName
 * 		typeParameters?
 * 		primaryConstructor?
 * 		(":" annotations delgationSpecifier{","})?
 * 		typeConstraints
 * 		(classBody? | enumClassBody)
 * 	;
 */
static void _class(void) {
	//TODO
}

/**
 * @brief primaryConstructor (used by _class, object)
 * 	: (modifiers "constructor")? ("(" functionParameter{","} ")")
 * 	;
 */
static void primaryConstructor(void) {
	//TODO
}

/**
 * @brief classBody (used by objectLiteral, enumEntry, _class, object)
 * 	: ("{" members "}")
 * 	;
 */
static void classBody(void) {
	//TODO
}

/**
 * @brief members (used by enumClassBody, classBody)
 * 	: memberDeclaration*
 * 	;
 */
static void members(void) {
	//TODO
}

/**
 * @brief delegationSpecifier (used by delegationSpecifier)
 * 	: constructorInvocation
 * 	: userType
 * 	: explicitDelegation
 * 	;
 */
static void delegationSpecifier(void) {
	//TODO
}

/**
 * @brief explicitDelegation (used by delegationSpecifier)
 * 	: userType "by" expresion
 * 	;
 */
static void explicitDelegation(void) {
	//TODO
}

/**
 * @brief typeParameters (used by _class, property, function)
 * 	: "<" typeParameter{","} ">"
 * 	;
 */
static void typeParameters(void) {
	//TODO
}

/**
 * @brief typeParameter (used by typeParameters)
 * 	: modifiers SimpleName (":" userType)?
 * 	;
 */
static void typeParameter(void) {
	//TODO
}

/**
 * @brief typeConstraints (used by _class, property, function)
 * 	: ("where" typeConstraint{","})?
 * 	;
 */
static void typeConstraints(void) {
	//TODO
}

/**
 * @brief typeConstraint (used by typeConstraints)
 * 	: annotations SimpleName ":" type
 * 	;
 */
static void typeConstraint(void) {
	//TODO
}

//Class members

/**
 * @brief memberDeclaration (used by members)
 * 	: companionObject
 * 	: object
 * 	: function
 * 	: property
 * 	: _class
 * 	: typeAlias
 * 	: anonymousInitializer
 * 	: secondaryConstructor
 * 	;
 */
static void memberDeclaration(void) {
	//TODO
}

/**
 * @brief anonymousInitializer (used by memberDeclaration)
 * 	: "init" block
 * 	;
 */
static void anonymousInitializer(void) {
	//TODO
}

/**
 * @brief companionObject (used by memberDeclaration)
 * 	: modifiers "companion" "object"
 * 	;
 */
static void companionObject(void) {
	//TODO
}

/**
 * @brief valueParameters (used by secondaryConstructor, function)
 * 	: "(" functionParameter{","}? ")"
 * 	;
 */
static void valueParameters(void) {
	//TODO
}

/**
 * @brief functionParameter (used by valueParameters, primaryConstructor)
 * 	: modifiers ("val" | "var"}? parameter ("=" expresion)?
 * 	;
 */
static void functionParameter(void) {
	//TODO
}

/**
 * @brief initializer (used by enumEntry)
 * 	: annotations constructorInvocation
 * 	;
 */
static void initializer(void) {
	//TODO
}

/**
 * @brief block (used by catchBlock, anonymousInitializer, secondaryConstructor, functionBody, try, finallyBlock)
 * 	: "{" statements "}"
 * 	;
 */
static void block(void) {
	//TODO
}

/**
 * @brief function (used by memberDeclaration, declaration, topLevelObject)
 * 	: modifiers "fun" typeParameters?
 * 		(type "." | annotations)?
 * 		SimpleName
 * 		typeParameters? valueParameters (":" type)?
 * 		typeConstraints
 * 		functionBody?
 * 	;
 */
static void function(void) {
	//TODO
}

/**
 * @brief functionBody (used by getter, setter, function)
 * 	: block
 * 	: "=" expresion
 * 	;
 */
static void functionBody(void) {
	//TODO
}

/**
 * @brief variableDeclarationEntry (used by for, property, multipleVarabileDeclarations)
 * 	: SimpleName (":" type)?
 * 	;
 */
static void variableDeclarationEntry(void) {
	//TODO
}

/**
 * @brief multipleVarabileDeclarations (used by for, property)
 * 	: "(" variableDeclarationEntry{","} ")"
 * 	;
 */
static void multipleVarabileDeclarations(void) {
	//TODO
}

/**
 * @brief property (used by memberDeclaration, declaration, topLevelObject)
 * 	: modifiers ("val" | "var")
 * 		typeParameters? (type "," | annotations)?
 * 		(multipleVarabileDeclarations | variableDeclarationEntry)
 * 		typeConstraints
 * 		("by" | "=" expresion SEMI?)?
 * 		(getter? setter? | setter? getter?) SEMI?
 * 	;
 */
static void property(void) {
	//TODO
}

/**
 * @brief getter (used by property)
 * 	: modifiers "get"
 * 	: modifiers "get" "(" ")" (":" type)? functionBody
 * 	;
 */
static void getter(void) {
	//TODO
}

/**
 * @brief setter (used by property)
 * 	: modifiers "set"
 * 	: modifiers "set" "(" modifiers (SimpleName | parameter) ")" functionBody
 * 	;
 */
static void setter(void) {
	//TODO
}

/**
 * @brief parameter (used by functionType, setter, functionParameter)
 * 	: SimpleName ":" type
 * 	;
 */
static void parameter(void) {
	//TODO
}

/**
 * @brief object (used by memberDeclaration, declaration, topLevelObject)
 * 	: "object" SimpleName primaryConstructor? (":" delegationSpecifier{","})? classBody?
 * 	;
 */
static void object(void) {
	//TODO
}

/**
 * @brief secondaryConstructor (used by memberDeclaration)
 * 	: modifiers "constructor" valueParameters (":" constructorDelegationCall)? block
 * 	;
 */
static void secondaryConstructor(void) {
	//TODO
}

/**
 * @brief constructorDelegationCall (used by secondaryConstructor)
 * 	: "this" valueArguments
 * 	: "super" valueArguments
 * 	;
 */
static void constructorDelegationCall(void) {
	//TODO
}

//Enum classes

/**
 * @brief enumClassBody (used by _class)
 * 	: "{" enumEntries (";" members)? "}"
 * 	;
 */
static void enumClassBody(void) {
	//TODO
}

/**
 * @brief enumEntries (used by enumClassBody)
 * 	: enumEntry*
 * 	: (enumEntry ","?)?
 * 	;
 */
static void enumEntries(void) {
	//TODO
}

/**
 * @brief enumEntry (used by enumEntries)
 * 	: modifiers SimpleName ((":" initializer) | ("(" arguments ")")? classBody?
 * 	;
 */
static void enumEntry(void) {
	//TODO
}

//Types

/**
 * @brief type (used by isRHS, simpleUserType, parameter, functionType, atomicExpression, getter, variableDeclarationEntry, property, typeArguments, typeConstraint, function)
 * 	: annotation typeDescriptor
 * 	;
 */
static void type(void) {
	//TODO
}

/**
 * @brief typeDescriptor (used by nullableType, typeDescriptor, type)
 * 	: "(" typeDescriptor ")"
 * 	: functionType
 * 	: nullableType
 * 	: "dynamic"
 * 	;
 */
static void typeDescriptor(void) {
	//TODO
}

/**
 * @brief nullableType (used by typeDescriptor)
 * 	: typeDescriptor "?"
 * 	;
 */
static void nullableType(void) {
	//TODO
}

/**
 * @brief userType (used by typeParameter, catchBlock, callableReference, typeDescriptor, delegationSpecifier, constructorInvocation, explicitDelegation)
 * 	: ("package" ".")? simpleUserType{"."}
 * 	;
 */
static void userType(void) {
	//TODO
}

/**
 * @brief simpleUserType (used by userType)
 * 	: SimpleName ("<" (optionalProjection type | "*"){","} ">")?
 * 	;
 */
static void simpleUserType(void) {
	//TODO
}

/**
 * @brief optionalProjection (used by simpleUserType)
 * 	: varianceAnnotation
 * 	;
 */
static void optionalProjection(void) {
	//TODO
}

/**
 * @brief functionType (used by typeDescriptor)
 * 	: (type ".")? "(" (parameter | modifiers type){","} ")" "->" type?
 * 	;
 */
static void functionType(void) {
	//TODO
}

// Control structures

/**
 * @brief kif (used by atomicExpression)
 * 	: "if" "(" expresion ")" expresion SEMI? ("else" expresion)?
 * 	;
 */
static void _if(void) {
	//TODO
}

/**
 * @brief ktry (used by atomicExpression)
 * 	: "try" block catchBlock* finallyBlock?
 * 	;
 */
static void _try(void) {
	//TODO
}

/**
 * @brief catchBlock (used by ktry)
 * 	: "catch" "(" annotations SimpleName ":" userType ")" block
 * 	;
 */
static void catchBlock(void) {
	//TODO
}

/**
 * @brief finallyBlock (used by ktry)
 * 	: "finally" block
 * 	;
 */
static void finallyBlock(void) {
	//TODO
}

/**
 * @brief loop (used by atomicExpression)
 * 	: kfor
 * 	: kwhile
 * 	: dowhile
 * 	;
 */
static void loop(void) {
	//TODO
}

/**
 * @brief kfor (used by loop)
 * 	: "for" "(" annotations (multipleVarabileDeclarations | variableDeclarationEntry) "in" expresion ")" expresion
 * 	;
 */
static void kfor(void) {
	//TODO
}

/**
 * @brief kwhile (used by loop)
 * 	: "while" "(" expresion ")" expresion
 * 	;
 */
static void kwhile(void) {
	//TODO
}

/**
 * @brief dowhile (used by loop)
 * 	: "do" expresion "while" "(" expresion ")"
 * 	;
 */
static void dowhile(void) {
	//TODO
}

// Expressions

/**
 * @brief expression (used by kfor, atomicExpression, longTemplate, whenCondition, functionBody, dowhile, property, script, explicitDelegation, jump, kwhile, whenEntry, arrayAccess, statement, kfi, when, valueArguments, functionParameter)
 * 	: disjuction (assignmentOperator disjuction)*
 * 	;
 */
static void expression(void) {
	//TODO
}

/**
 * @brief disjuction (used by expresion)
 * 	: conjuction ("||" conjuction)*
 * 	;
 */
static void disjuction(void) {
	//TODO
}

/**
 * @brief conjuction (used by disjuction)
 * 	: equalityComparison ("&&" equalityComparison)*
 * 	;
 */
static void conjuction(void) {
	//TODO
}

/**
 * @brief equalityComparison (used by equalityComparison)
 * 	: compariosn (equalityOperation companion)*
 * 	;
 */
static void equalityComparison(void) {
	//TODO
}

/**
 * @brief companion (used by equalityComparison)
 * 	: namedInfix (companionOperation namedInfix)*
 * 	;
 */
static void companion(void) {
	//TODO
}

/**
 * @brief namedInfix (used by companion)
 * 	: elvisExpression (inOperation elvisExpression)*
 * 	: elvisExpression (inOperation isRHS)?
 * 	;
 */
static void namedInfix(void) {
	//TODO
}

/**
 * @brief elvisExpression (used by namedInfix)
 * 	: infixFunctionCall ("?:" infixFunctionCall)*
 * 	;
 */
static void elvisExpression(void) {
	//TODO
}

/**
 * @brief infixFunctionCall (used by elvisExpression)
 * 	: rangeExpression (SimpleName rangeExpression)*
 * 	;
 */
static void infixFunctionCall(void) {
	//TODO
}

/**
 * @brief rangeExpression (used by infixFunctionCall)
 * 	: additiveExpression (".." additiveExpression)*
 * 	;
 */
static void rangeExpression(void) {
	//TODO
}

/**
 * @brief additiveExpression (used by rangeExpression)
 * 	: multiplicativeExpression (additiveOperation multiplicativeExpression)*
 * 	;
 */
static void additiveExpression(void) {
	//TODO
}

/**
 * @brief multiplicativeExpression (used by additiveExpression)
 * 	: typeRHS (multiplicativeOperation typeRHS)*
 * 	;
 */
static void multiplicativeExpression(void) {
	//TODO
}

/**
 * @brief typeRHS (used by multiplicativeExpression)
 * 	: prefixUnaryExpression (typeOperation prefixUnaryExpression)*
 * 	;
 */
static void typeRHS(void) {
	//TODO
}

/**
 * @brief prefixUnaryExpression (used by typeRHS)
 * 	: prefixUnaryOperation* postfixUnaryExpression
 * 	;
 */
static void prefixUnaryExpression(void) {
	//TODO
}

/**
 * @brief postfixUnaryExpression (used by prefixUnaryExpression, postfixUnaryExpression)
 * 	: atomicExpression postfixUnaryOperation*
 * 	: callableReference postfixUnaryOperation*
 * 	;
 */
static void postfixUnaryExpression(void) {
	//TODO
}

/**
 * @brief callableReference (used by postfixUnaryExpression)
 * 	: (userType "?"*)? "::" SimpleName typeArguments?
 * 	;
 */
static void callableReference(void) {
	//TODO
}

/**
 * @brief atomicExpression (used by postfixUnaryExpression)
 * 	: "(" expresion ")"
 * 	: literalConstant
 * 	: functionLiteral
 * 	: "this" labelReference?
 * 	: "super" ("<" type ">")? labelReference?
 * 	: _if
 * 	: when
 * 	: _try
 * 	: objectLiteral
 * 	: jump
 * 	: loop
 * 	: SimpleName
 * 	: FieldName
 * 	: "package"
 * 	;
 */
static void atomicExpression(void) {
	//TODO
}

/**
 * @brief labelReference (used by atomicExpression, jump)
 * 	: "@" ++ LabelName
 * 	;
 */
static void labelReference(void) {
	//TODO
}

/**
 * @brief labelDefinition (used by prefixUnaryExpression, annotatedLambda)
 * 	: LabelName ++ "@"
 * 	;
 */
static void labelDefinition(void) {
	//TODO
}

/**
 * @brief literalConstant (used by atomicExpression)
 * 	: "true" | "false"
 * 	: stringTemplate
 * 	: NoEscapeString
 * 	: IntergerLiteral
 * 	: CharacterLiteral
 * 	: FloatLiteral
 * 	: "null"
 * 	;
 */
static void literalConstant(void) {
	//TODO
}

/**
 * @brief stringTemplate (used by literalConstant)
 * 	: "\"" stringTemplateElement "\""
 * 	;
 */
static void stringTemplate(void) {
	//TODO
}

/**
 * @brief stringTemplateElement (used by stringTemplate)
 * 	: RegularStringPart
 * 	: ShortTemplateEntryStart (SimpleName | "this")
 * 	: EscapeSequence
 * 	: longTemplate
 * 	;
 */
static void stringTemplateElement(void) {
	//TODO
}

/**
 * @brief longTemplate (used by stringTemplateElement)
 * 	: "${" expresion "}"
 * 	;
 */
static void longTemplate(void) {
	//TODO
}

/**
 * @brief isRHS (used by namedInfix, whenCondition)
 * 	: type
 * 	;
 */
static void isRHS(void) {
	//TODO
}

/**
 * @brief declaration (used by statement)
 * 	: function
 * 	: property
 * 	: _class
 * 	: object
 * 	;
 */
static void declaration(void) {
	//TODO
}

/**
 * @brief statement (used by statements)
 * 	: declaration
 * 	: expresion
 * 	;
 */
static void statement(void) {
	//TODO
}

/**
 * @brief multiplicativeOperation (used by multiplicativeExpression)
 * 	: "*"
 * 	: "/"
 * 	: "%"
 * 	;
 */
static void multiplicativeOperation(void) {
	//TODO
}

/**
 * @brief additiveOperation (used by additiveExpression)
 * 	: "+"
 * 	: "-"
 * 	;
 */
static void additiveOperation(void) {
	//TODO
}

/**
 * @brief typeOperation (used by typeRHS)
 * 	: "as"
 * 	: "as?"
 * 	: ":"
 * 	;
 */
static void typeOperation(void) {
	//TODO
}

/**
 * @brief isOperation (used by namedInfix)
 * 	: "is"
 * 	: "!is"
 * 	;
 */
static void isOperation(void) {
	//TODO
}

/**
 * @brief companionOperation (used by compariosn)
 * 	: "<"
 * 	: ">"
 * 	: ">="
 * 	: "<="
 * 	;
 */
static void companionOperation(void) {
	//TODO
}

/**
 * @brief equalityOperation (used by equalityComparison)
 * 	: "=="
 * 	: "!="
 * 	;
 */
static void equalityOperation(void) {
	//TODO
}

/**
 * @brief assignmentOperation (used by expresion)
 * 	: "="
 * 	: "+="
 * 	: "-="
 * 	: "*="
 * 	: "/="
 * 	: "%="
 * 	;
 */
static void assignmentOperation(void) {
	//TODO
}

/**
 * @brief prefixUnaryOperation (used by prefixUnaryExpression)
 *		: "-"
 * 	: "+"
 * 	: "++"
 * 	: "--"
 * 	: "!"
 * 	: annotations
 * 	: labelDefinition
 * 	;
 */
static void prefixUnaryOperation(void) {
	//TODO
}

/**
 * @brief postfixUnaryOperation (used by postfixUnaryExpression)
 * 	: "++"
 * 	: "--"
 * 	: "!!"
 * 	: callSuffix
 * 	: arrayAccess
 * 	: memberAccessOperation postfixUnaryExpression
 * 	;
 */
static void postfixUnaryOperation(void) {
	//TODO
}

/**
 * @brief callSuffix (used by constructorInvocation, postfixUnaryOperation)
 * 	: typeArguments? valueArguments annotatedLambda
 * 	: typeArguments annotatedLambda
 * 	;
 */
static void callSuffix(void) {
	//TODO
}

/**
 * @brief annotatedLambda (used by callSuffix)
 * 	: ("@" annotationEntry)* labelDefinition? functionLiteral
 * 	;
 */
static void annotatedLambda(void) {
	//TODO
}

/**
 * @brief memberAccessOperation (used by postfixUnaryOperation)
 * 	: "."
 * 	: "?."
 * 	: "?"
 * 	;
 */
static void memberAccessOperation(void) {
	//TODO
}

/**
 * @brief valueArguments (used by callSuffix, constructorDelegationCall, unescapedAnnotation)
 * 	: "(" (SimpleName "=")? "*"? expresion{","} ")"
 * 	;
 */
static void valueArguments(void) {
	//TODO
}

/**
 * @brief jump (used by atomicExpression)
 * 	: "throw" expresion
 * 	: "return" ++ labelReference? expresion?
 * 	: "continue" ++ labelReference?
 * 	: "break" ++ labelReference?
 * 	;
 */
static void jump(void) {
	//TODO
}

/**
 * @brief functionLiteral (used by atomicExpression, annotatedLambda)
 * 	: "{" statements "}"
 * 	: "{" (modifiers SimpleName){","} "->" statements "}"
 * 	;
 */
static void functionLiteral(void) {
	//TODO
}

/**
 * @brief statements (used by block, functionLiteral)
 * 	: SEMI* statement{SEMI+} SEMI*
 * 	;
 */
static void statements(void) {
	//TODO
}

/**
 * @brief constructorInvocation (used by delegationSpecifier, initializer)
 * 	: userType callSuffix
 * 	;
 */
static void constructorInvocation(void) {
	//TODO
}

/**
 * @brief arrayAccess (used by postfixUnaryOperation)
 * 	: "[" expresion{","} "]"
 * 	;
 */
static void arrayAccess(void) {
	//TODO
}

/**
 * @brief objectLiteral (used by atomicExpression)
 * 	: "object" (":" delegationSpecifier{","})? classBody
 * 	;
 */
static void objectLiteral(void) {
	//TODO
}

// Pattern matching

/**
 * @brief when (used by atomicExpression)
 * 	: "when" ("(" expresion ")")? "{"
 * 			whenEntry*
 * 		"}"
 * 	;
 */
static void when(void) {
	//TODO
}

/**
 * @brief whenEntry (used by when)
 * 	: whenCondition{","} "->" expression SEMI
 * 	: "else" "->" expression SEMI
 * 	;
 */
static void whenEntry(void) {
	//TODO
}

/**
 * @brief whenCondition (used by whenEntry)
 * 	: expression
 * 	: ("in" | "!in") expression
 * 	: ("is" | "!is") isRHS
 * 	;
 */
static void whenCondition(void) {
	//TODO
}

// Modifiers

/**
 * @brief modifiers (used by typeParameter, getter, packageHeader, _class, property, functionLiteral, function, functionType, secondaryConstructor, setter, enumEntry, companionObject, primaryConstructor, functionParameter)
 * 	: modifier*
 * 	;
 */
static void modifiers(void) {
	//TODO
}

/**
 * @brief modifier (used by modifiers)
 * 	: modifierKeywoard
 */
static void modifier(void) {
	//TODO
}

/**
 * @brief modifierKeywoard (used by modifier)
 * 	: classModifier
 * 	: accessModifier
 * 	: varianceAnnotation
 * 	: memberModifier
 * 	: annotations
 * 	;
 */
static void modifierKeywoard(void) {
	//TODO
}

/**
 * @brief classModifier (used by modifierKeywoard)
 * 	: "abstract"
 * 	: "final"
 * 	: "enum"
 * 	: "open"
 * 	: "annotation"
 * 	;
 */
static void classModifier(void) {
	//TODO
}

/**
 * @brief memberModifier (used by modifierKeywoard)
 * 	: "override"
 * 	: "open"
 * 	: "final"
 * 	: "abstract"
 * 	;
 */
static void memberModifier(void) {
	//TODO
}

/**
 * @brief accessModifier (used by modifierKeywoard)
 * 	: "private"
 * 	: "protected"
 * 	: "public"
 * 	: "internal"
 * 	;
 */
static void accessModifier(void) {
	//TODO
}

/**
 * @brief varianceAnnotation (used by modifierKeywoard, optionalProjection)
 * 	: "in"
 * 	: "out"
 * 	;
 */
static void varianceAnnotation(void) {
	//TODO
}

// Annotations

/**
 * @brief annotations (used by catchBlock, prefixUnaryOperation, _for, modifierKeywoard, _class, property, type, typeConstraint, function, initializer)
 * 	: (annotation | annotationList)*
 * 	;
 */
static void annotations(void) {
	//TODO
}

/**
 * @brief annotation (used by annotations)
 * 	: "@" (annotationUserSiteTarget ":")? unescapedAnnotation
 * 	;
 */
static void annotation(void) {
	//TODO
}

/**
 * @brief annotationList (used by annotation)
 * 	: "@" (annotationUserSiteTarget ":")? "[" unescapedAnnotation+ "]"
 * 	;
 */
static void annotationList(void) {
	//TODO
}

/**
 * @brief annotationUserSiteTarget (used by annotation, annotationList)
 * 	: "file"
 * 	: "field"
 * 	: "property"
 * 	: "get"
 * 	: "set"
 * 	: "param"
 * 	: "sparam"
 * 	;
 */
static void annotationUserSiteTarget(void) {
	//TODO
}

/**
 * @brief unescapedAnnotation (used by annotation, annotationList)
 * 	: SimpleName{"."} typeArguments? valueArguments?
 * 	;
 */
static void unescapedAnnotation(void) {
	//TODO
}

// PARSER CREATORS
/**
 * @brief KotlinFileParser
 * @return Parser for creating tags found in Kotlin file
 */
extern parserDefinition * KotlinFileParser(void) {
	static const char * const extensions[] = {"kt", NULL};
	parserDefinition * def = parserNew("Kotlin");
	def->initialize = initialize;
	def->extensions = extensions;
	def->kinds = KotlinKinds;
	def->kindCount = ARRAY_SIZE(KotlinKinds);
	def->parser = kotlinFile;
	def->keywordTable = KotlinKeywordsTable;
	def->keywordCount = ARRAY_SIZE(KotlinKeywordsTable);
	def->useCork = TRUE;
	return def;
}

/**
 * @brief KotlinScriptParser
 * @return Parser for creating tags found in Kotlin scripts
 */
extern parserDefinition * KotlinScriptParser(void) {
	static const char * const extensions[] = {"kts", NULL};
	parserDefinition * def = parserNew("Kotlin");
	def->initialize = initialize;
	def->extensions = extensions;
	def->kinds = KotlinKinds;
	def->kindCount = ARRAY_SIZE(KotlinKinds);
	def->parser = script;
	def->keywordTable = KotlinKeywordsTable;
	def->keywordCount = ARRAY_SIZE(KotlinKeywordsTable);
	def->useCork = TRUE;
	return def;
}
