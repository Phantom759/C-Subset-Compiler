%{
#include <stdio.h>
#include <stdlib.h>
#include "compile.tab.h"
extern int yy_flex_debug;
%}
%option noyywrap yylineno
%x Start 
%x Star
INT 	"int"
CHAR 	"char"
VOID	"void"
EXTERN	"extern"
DEQUAL	"=="
NEQUAL	"!="
GTE		">="
LTE 	"<="
AND 	"&&"
OR  	"||"
IF 		"if"
FOR 	"for"
WHILE 	"while"
RETURN 	"return"
ELSE 	"else"
LETTER	[a-zA-z]
DIGIT	[0-9]
ID 		([[:alpha:]]([[:alnum:]]|"_")*)
INTCON	[0-9]+
CHARCON	"'"[^[:blank:]\']"'"|"'\\n'"|"'\\0'"
STRINGCON 	\"([[:print:]]{-}[\n\"])*\"
CATCH	[[:print:]]{-}[[:space:]]	
NEWLINE \n		
SPACE 	[[:space:]]								
%%
<INITIAL>"/*" 		BEGIN(Start);
<Start>[^*] ;
<Start>"*"	BEGIN(Star);
<Star>[^*/]	BEGIN(Start);
<Star>"*" 	;
<Star>"/"	BEGIN(INITIAL);
{INT}		return INT;
{CHAR}		return CHAR;
{VOID}		return VOID;
{FOR} 		return FOR;
{WHILE}		return WHILE;
{IF} 		return IF;
{ELSE} 		return ELSE;
{RETURN} 	return RETURN;
{EXTERN}	return EXTERN;
{ID}		{yylval.idName = strdup(yytext); return ID;}
{INTCON}	{yylval.iVal = atoi(yytext); return INTCON;}
{CHARCON}	{yylval.cVal = yytext[0]; return CHARCON;}
{DEQUAL}	return DEQUAL;
{NEQUAL}	return NEQUAL;
{GTE}		return GTE;
{LTE} 		return LTE;
{AND}		return AND;
{OR}		return OR;
{NEWLINE} 	;
{SPACE} 	;
{STRINGCON} {yylval.sVal = strdup(yytext); return STRINGCON;}
{CATCH}		return yytext[0];
%{yy_flex_debug=1;
%}
%%

