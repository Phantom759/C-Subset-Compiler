

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "compile.tab.h"
extern int yylineno;
extern char *yytext;
int exitStatus=0;

typedef enum {INTEGER, CHARACTER, INTARR, CHARARR, BOOL, VOID1, FUNC, INTC, CHARC, STRINGC} Type;
typedef enum {FUNC1, IF1, WHILE1, FOR1, RETURN1} stmtType;
typedef enum {ADD, SUB, MUL, DIV, ID2, NEG, ASSG, CONST, FUNCCALL} Operation; 				// add comparative values later for if, while, for.
typedef struct symTable{																	// Struct for symbol table, able to hold variables
	Type type; /*int=1, char=2, chararr=3, intarr=4*/
	int defined;
	char *id;
	int iVal;
	char cVal;
	char *strVal;
	int offset;
	int isPar;
	struct symTable *next;
} symTabNode;

typedef struct threeAddrInstr{																// Three address instructions, used for translating C code to MIPS
	Type type;																				// contains an operation type, two source addresses, and a destination address
	Operation op;
	struct symTable *dest;
	struct symTable *src1;
	struct symTable *src2;
	struct parameter *params;
	struct threeAddrInstr *next;
	int iVal;
	char cVal;
	char *strVal;
} instruction;

typedef struct symFTable{																	// Function symbol table, keeps track of valid function names that have been declared
	Type type; /*int=0, char=1, intarr=2, chararr=3, void=5*/
	char *id;
	int external;
	int defined;
	int proto;
	struct threeAddrInstr *instrs;
	struct parameter *param;
	struct symFTable *next;
} fTabNode;

typedef struct expressionNode{																// Expression node, created and added to tree when an expression is evaluated
 	char *name;
	int type;
	int iVal;
	char cVal;
	char *strVal;
	Operation op;
	struct threeAddrInstr *code;
	struct expressionNode *src1; 	//for operations
	struct expressionNode *src2;	//for operations
	struct symTable *src;			//points to appropriate node in local/global/function symbol table
	struct expressionNode *left; 	//for operations
	struct expressionNode *right;	//for operations
	struct expressionNode *next;	//points to next expression, after the current one is done
	struct parameter *parameters; 	//for functions. Points to the function's list of parameters
} exprNode;

typedef struct statementNode{																// Statement node, created and added to tree when a statement is evaluated
 	char *name;
	Type type;
	stmtType sType;
	int iVal;
	char cVal;
	char *strVal;
	struct symTable *src;
	struct expressionNode *exprs;
	struct statementNode *next;
} stmtNode;


typedef struct parameter{																	// Parameter, specifically for use in functions (not valid outside of scope)
	exprNode *param;
	int isGlobal;
	struct parameter *next;
} paramNode;

symTabNode *table;
symTabNode *gTable;
fTabNode *fTable;
symTabNode *addedGVars; 
int local=0;
Type curType;
Type funcType;
Type returnType;
int tmpNum=0;
int offSetNeg=0;
int offSetPos=8;
int isParam=0;
exprNode *curFunc;
instruction *instructionList;
paramNode *tempParamList;

void printParams(instruction *instr);
int countLocVars();
void printGVar(symTabNode *node);
void addToInstrList(instruction *instr);
void printInstructions(char *fName);
void printInstrSet(instruction *instr);
exprNode *makeAssgNode(exprNode *node, char *id);
void printSyntaxTree(exprNode *node, int depth);
void printexprNode(exprNode *node);
instruction *newInstr(Operation op, exprNode *dest, exprNode *src1, exprNode *src2);
void addInstrListToFunc(char *fname, instruction *instr);
exprNode *makeMathNode(Type type, Operation op, exprNode *src1, exprNode *src2);
symTabNode *newTemp(Type type);
instruction *newConstInstr(Operation op, symTabNode *place, exprNode *expr);
void makeStmtNode(char *id, stmtType sType);
void createTempGTable();
void setAddedVarTypes();
void printLocVars(char *id);
void setDefined(char *id);
symTabNode *addToTable(char *name, Type type, int local);
void setVarValue(char *name, exprNode *expr);
void setCurType(Type type);
void setCurArrType();
void setScope(int x);
void freeTable(symTabNode *node);
void createTable();
void checkIfBool(exprNode *expr);
void setArrayValue(char *name, exprNode *index, exprNode *expr);
void setFuncType(Type type);
void addFunction(char *name, Type type);
void addExternalProto(char *name, Type type);
void addProto(char *name, Type type);
void addExternalFunction(char *name, Type type);
void checkReturn();
void checkFuncType(char *id, Type type);
void addParams(exprNode *function, paramNode *list);
void freeFunctions(fTabNode *node); 					/*maybe make into two different functions? One for parameters and one for function?*/
int checkTable(symTabNode *node, char *id);
int checkFTable(fTabNode *table, char *id);
exprNode *makeexprNode(Type type, Operation op, int iVal, char cVal, char *strVal);
exprNode *idToexprNode(char *name, Type type);
exprNode *idToArrexprNode(char *name);
exprNode *calculate( exprNode *expr1, exprNode *expr2, char op);
void addParam(exprNode *param);
%}

%union{
	int iVal;
	char cVal;
	char *sVal;
	char *idName;
	int expr;
	struct expressionNode *calc;
}

%expect 1
%token <idName> ID
%token <iVal> INTCON
%token <cVal> CHARCON
%token <sVal> STRINGCON
%token <idName> INT
%token <idName> CHAR
%token VOID
%token EXTERN
%token DEQUAL
%token NEQUAL
%token GTE
%token LTE
%token AND
%token OR
%token RETURN
%token FOR
%token IF
%token WHILE
%token NEWLINE
%token ELSE


%type <expr> type
%type <calc> expr
%type <calc> stmt
%type <calc> assg

%left '='
%left '!'
%left DEQUAL NEQUAL GTE LTE '>' '<'
%left AND OR
%left '+' '-'
%left '/' '*'
%right UMINUS


%%

prog 	:	prog dcl 																												// Starting point for program
		|	prog func {tmpNum=0;offSetNeg=0;offSetPos=8;}
		|
		|	prog error dcl {yyerrok;}
		|	prog error func {yyerrok;}
		;

dcl1	:	type ID setscope1 '(' parm_types ')' {addProto($2, $1); setScope(0);}													// Declaration block, evaluates all forms of valid declarations
		|	EXTERN type ID setscope1 '(' parm_types ')' {addExternalProto($3, $2); setScope(0);}
		|	dcl1 ',' ID '(' parm_types ')' 
		;

dcl2	:	VOID ID setscope1 '(' parm_types ')' {addProto($2, VOID);setScope(0);}
		|	EXTERN VOID ID setscope1 '(' parm_types ')' {addExternalProto($3, VOID); setScope(0);}
		|	dcl2 ',' ID '(' parm_types ')' 
		;

dcl 	: 	type multiVarDecl ';' {curType=$1; setScope(0); setAddedVarTypes(); freeTable(addedGVars); createTempGTable();}
		|	EXTERN type multiVarDecl ';' {curType=$2; setScope(0); setAddedVarTypes(); freeTable(addedGVars); createTempGTable();}
 		| 	dcl1 ';' {if(table!=NULL) createTable();}
 		| 	dcl2 ';' {if(table!=NULL) createTable();}
		| 	dcl1 error ';' {yyerrok;}
 		| 	dcl2 error ';' {yyerrok;}
 		;

var_decl 	: 	ID 							{addToTable($1, curType, local);}														// Identifies a variable that is declared, but not defined
			| 	ID '[' INTCON ']' 			{setCurArrType(); addToTable($1, curType, local);}
			;

type 	: 	CHAR 		{$$=CHARACTER;}																								// Identifies types for variables
 		|	INT 		{$$=INTEGER;}
 		;

idBrackets 	:	type ID '[' ']' {setScope(1); setCurType($1); setCurArrType(); isParam=1; addToTable($2, curType, local); isParam=0; setDefined($2);}						// Identifies variables used as function parameters (not declared)
			|	idBrackets ',' type ID '[' ']' {setScope(1); setCurType($3); setCurArrType(); isParam=1; addToTable($4, curType, local); isParam=0; setDefined($4);}
			|	type ID {setScope(1); isParam=1; addToTable($2, $1, local); isParam=0; setDefined($2);}
			|	idBrackets ',' type ID {setScope(1); isParam=1; addToTable($4, $3, local); isParam=0; setDefined($4);}
			|	type ID '[' error ']' {yyerrok;}
			|	idBrackets ',' error type ID '[' ']' {yyerrok;}
			|	type ID error {yyerrok;}
			|	idBrackets ',' error type ID {yyerrok;}
			;

parm_types 	: 	VOID 																												// Identifies parameter types
 			|	idBrackets
 			;

multiVarDecl	:	var_decl 																										// Multiple variable declaration block, identifies multiple variables declared at once
				|	multiVarDecl ',' var_decl
				;

multiTypeVarDecl	:	type {setCurType($1);} multiVarDecl ';'			
					|	multiTypeVarDecl type {setCurType($2);} multiVarDecl ';'
					;

multiStmt	:	stmt { printSyntaxTree( $1, 0 ); printexprNode($1); }																// Identifies multiple statements made at once
			|	multiStmt stmt	{ printSyntaxTree( $2, 0 ); printexprNode($2);} 
			;

																																	// Function block, identifies all valid definitions of functions

func 	: 	type ID setscope1 '(' parm_types ')' '{' '}' {printf(".text\n"); setScope(0); /*printLocVars($2);*/ addFunction($2, $1); funcType=$1; checkReturn(); returnType=VOID; addInstrListToFunc($2, instructionList); printInstructions($2); createTable();}
		| 	type ID setscope1 '(' parm_types ')' '{' multiStmt '}' {printf(".text\n"); setScope(0); /*printLocVars($2);*/ addFunction($2, $1); funcType=$1; checkReturn(); returnType=VOID; addInstrListToFunc($2, instructionList); printInstructions($2); createTable();}
		|	type ID setscope1 '(' parm_types ')' '{' multiTypeVarDecl '}' {printf(".text\n"); setScope(0); /*printLocVars($2);*/ addFunction($2, $1); funcType=$1; checkReturn(); returnType=VOID; addInstrListToFunc($2, instructionList); printInstructions($2); createTable();}
		| 	type ID setscope1 '(' parm_types ')' '{' multiTypeVarDecl multiStmt '}' {printf(".text\n"); setScope(0); /*printLocVars($2);*/ addFunction($2, $1); funcType=$1; checkReturn(); returnType=VOID; addInstrListToFunc($2, instructionList); printInstructions($2); createTable();}
		| 	VOID ID setscope1 '(' parm_types ')' '{''}' {printf(".text\n"); setScope(0); /*printLocVars($2);*/ addFunction($2, VOID); funcType=VOID; checkReturn(); returnType=VOID; addInstrListToFunc($2, instructionList); printInstructions($2); createTable();}
		| 	VOID ID setscope1 '(' parm_types ')' '{' multiStmt '}' {printf(".text\n"); setScope(0); /*printLocVars($2);*/ addFunction($2, VOID); funcType=VOID; checkReturn(); returnType=VOID; addInstrListToFunc($2, instructionList); printInstructions($2); createTable();}
		|	VOID ID setscope1 '(' parm_types ')' '{' multiTypeVarDecl '}' {printf(".text\n"); setScope(0); /*printLocVars($2);*/ addFunction($2, VOID); funcType=VOID; checkReturn(); returnType=VOID; addInstrListToFunc($2, instructionList); printInstructions($2); createTable();}
		|	VOID ID setscope1 '(' parm_types ')' '{' multiTypeVarDecl multiStmt '}' {printf(".text\n"); setScope(0); /*printLocVars($2);*/ addFunction($2, VOID); funcType=VOID; checkReturn(); returnType=VOID; addInstrListToFunc($2, instructionList); printInstructions($2); createTable();}
		| 	error ';' {yyerrok;}
		;

multiExpr	:	expr { printSyntaxTree( $1, 0 ); printexprNode($1); /*printf("printed expr node for %s\n", $1->name);*/}			// Identifies multiple expressions made at once
			|	multiExpr ',' expr { printSyntaxTree( $3, 0 ); printexprNode($3); /*printf("printed expr node for %s\n", $3->name);*/}
			|	expr error {yyerrok;}
			|	multiExpr ',' error expr {yyerrok;}
			;
 									/* I=Int or char operation, B=boolean operation, C=comparison operation */ 						// Identifies all valid forms of expressions
expr 	: 	'!' expr 				{$$=NULL;}//{ $$ = calculate( $2, $2, '!'); } /* send same expr as both exprNode arguments, with Boolean symbol */ 
 		| 	expr '+' expr 			{$$ = makeMathNode(INTEGER, ADD, $1, $3); if(isParam==1)addParam($$);}
 		| 	expr '-' expr 			{$$ = makeMathNode(INTEGER, SUB, $1, $3); if(isParam==1)addParam($$);}
 		| 	expr '*' expr 			{$$ = makeMathNode(INTEGER, MUL, $1, $3); if(isParam==1)addParam($$);}
 		| 	expr '/' expr 			{$$ = makeMathNode(INTEGER, DIV, $1, $3); if(isParam==1)addParam($$);}
 		| 	expr DEQUAL expr 		//{ $$ = calculate( $1, $3, 'D'); }
 		| 	expr NEQUAL expr 		//{ $$ = calculate( $1, $3, 'N'); }
 		| 	expr LTE expr 			//{ $$ = calculate( $1, $3, 'L'); }
 		| 	expr GTE expr			//{ $$ = calculate( $1, $3, 'G'); }
 		| 	expr '>' expr 			//{ $$ = calculate( $1, $3, '>'); }
 		| 	expr '<' expr 			//{ $$ = calculate( $1, $3, '<'); }
 		| 	expr AND expr 			//{ $$ = calculate( $1, $3, '&'); }
 		| 	expr OR expr 			//{ $$ = calculate( $1, $3, '|'); }
 		| 	ID 						{ $$ = idToexprNode($1, ID2); if(isParam==1){/*printf("Adding param\n");*/ addParam($$);}}
 		| 	ID'[' expr ']'		{ $$ = idToArrexprNode($1); if(isParam==1)  addParam($$);}
 		| 	ID '(' params multiExpr ')'	{/*printf("EXPR funccall\n");*/ $$ = idToexprNode($1, $$->type); addParams($$, tempParamList); isParam=0;}
 		|	ID 	'(' ')' 			{/*printf("EXPR funccall\n");*/ $$ = idToexprNode($1, $$->type);}
 		| 	'(' expr ')'			{ $$ = $2; /*printf("Single expression!\n");*/ if(isParam==1) addParam($$);}
 		| 	'-' expr %prec UMINUS 	{$$=NULL;}//{ $$ = calculate( $2, $2, 'M'); } /* send same expr as both exprNode arguments, with Integer symbol */
 		| 	INTCON 			{ $$ = makeexprNode( INTEGER, CONST, $1, 0, ""); if(isParam==1) addParam($$);}
		|	CHARCON 		{ $$ = makeexprNode( CHARACTER, CONST, 0, $1, ""); if(isParam==1) addParam($$);}
		|	STRINGCON 		{ $$ = makeexprNode( CHARARR, CONST, 0, 0, $1); if(isParam==1) addParam($$);}
		;

stmt 	: 	IF '(' expr ')' stmt				{checkIfBool($3);} 																	// Identifies all valid forms of statements
		|	IF '(' expr ')' stmt ELSE stmt 		{checkIfBool($3);}
 		| 	WHILE '(' expr ')' stmt 			{checkIfBool($3);}
 		| 	FOR '(' ';' ';' ')' stmt
 		| 	FOR '(' assg ';' ';' ')' stmt
 		| 	FOR '(' ';' expr ';' ')' stmt 		{checkIfBool($4);}
 		| 	FOR '(' ';' ';' assg ')' stmt
 		| 	FOR '(' assg ';' expr ';' ')' stmt 	{checkIfBool($5);}
 		| 	FOR '(' assg ';' ';' assg ')' stmt
 		| 	FOR '(' ';' expr ';' assg ')' stmt {checkIfBool($4);}
 		| 	FOR '(' assg ';' expr ';' assg ')' stmt 	{checkIfBool($5);}
 		| 	RETURN ';' 							{returnType=VOID;}
 		|	RETURN expr ';' 					{returnType=$2->type;}
 		| 	assg ';' 														// Already covered?
 		|	ID '(' ')' ';' 						{ $$ = idToexprNode($1, VOID); /*addParams($$, tempParamList);*/ isParam=0;}
 		|	ID '(' params multiExpr ')' ';' 			{ $$ = idToexprNode($1, VOID); addParams($$, tempParamList); isParam=0;} // Just make a different but similar stmt node?
 		| 	'{' multiStmt '}'
 		|	'{' '}'
 		| 	';'
 		;

assg 	: 	ID '=' expr 				{$$=makeAssgNode($3, $1), printSyntaxTree($$, 0); setVarValue($1, $3);} 					// Identifies all valid forms of assignments
		|	ID '[' expr ']' '=' expr	{setArrayValue($1, $3, $6);}
		|	ID error '=' expr {yyerrok;}
		|	ID '[' expr error ']' '=' expr {yyerrok;}
		;

setscope1 	:	{setScope(1);} 																										// Sets appropriate scope (global or local) for a variable when it is created
			;
params 		:	{isParam=1; /*printf("isParam=1\n");*/} 																			// Sets a parameter tag on certain variables used only as parameters
			;
%%
 yyerror(){
 if( yytext[0] == 0){
fprintf(stderr, "Line %d: End of File found too early\n", yylineno);
 }
 
 else{
fprintf(stderr, "Line %d: Syntax Error, unexpected '%s'\n", yylineno, yytext);
 }
 exitStatus = 1;
} 
main()
{
	printf("#####################################################\n");
	printf("print_int:\n");
    printf("li   $v0, 1  # escape code for printint\n");
    printf("lw   $a0, 0($sp)\n");
    printf("syscall      # print it\n");
    printf("jr $ra\n");
printf("#####################################################\n");
printf("print_string:\n");
    printf("li   $v0, 4 # escape code for print string\n");
    printf("lw   $a0, 0($sp)\n");
    printf("syscall     # print it\n");
    printf("jr $ra\n");
printf("#####################################################\n\n");

printf(".data\n\n");

//printf("_x:  .space 4\n");

	returnType=VOID;
	yyparse();
	table = malloc(sizeof(symTabNode));
	table->next = NULL;
	table->defined = 0;
	table->type = INTEGER;
	table->id = "$";
	gTable = malloc(sizeof(symTabNode));
	gTable->next = NULL;
	gTable->type = INTEGER;
	gTable->id = "$";
	gTable->defined=0;
	fTable = malloc(sizeof(fTabNode));
	fTable->next = NULL;
	fTable->type = INTEGER;
	fTable->id = "$";
	fTable->defined=0;
	fTable->proto=0;
	fTable->external = 0;
	fTable->param = NULL;
	//printf("%d\n", exitStatus);
	return exitStatus;
} 

void makeStmtNode(char *id, stmtType sType){
														// TODO:
}

void addParam(exprNode *param){
	paramNode *temp;
	temp = malloc(sizeof(paramNode));
	temp->param=param;
	temp->isGlobal=0;
	symTabNode *globalPtr=gTable;
	while(globalPtr!=NULL){
		if(strcmp(globalPtr->id, param->src->id)==0){
		//printf("Global param match\n");
		temp->isGlobal=1;
		break;
		}
		globalPtr=globalPtr->next;
	}
	temp->next = tempParamList;
	tempParamList=temp;
	param->src->isPar=1;
	//printf("Param %s added to parameter list!\n", param->src->id);
}

void addParams(exprNode *function, paramNode *list){
	paramNode *temp;
	temp=malloc(sizeof(paramNode));
	temp = tempParamList; 
	function->parameters=temp;
	tempParamList=NULL;
	//tempParamList = malloc(sizeof(paramNode));
	//printf("Made parameters starting with %s for function %s successfully!\n",function->parameters->param->src->id, function->name);
}




void setDefined(char *id){
	symTabNode *node=table;
	while(node!=NULL){
		if(strcmp(node->id, id)==0){
			node->defined=1;
			return;
			}
		node=node->next;
		}
		node=gTable;
	while(node!=NULL){
		if(strcmp(node->id, id)==0){
			node->defined=1;
			return;
			}
		node=node->next;
		}
}

void setAddedVarTypes(){
//printf("Set added var types!\n");
	symTabNode *node;
	symTabNode *gNode;
	node=addedGVars;
	gNode=gTable;
	while( node != NULL ){
		while(gNode!=NULL){
		if( strcmp(node->id, gNode->id) == 0){
			//printf("Global %s changed from type %d to type %d\n", gNode->id, gNode->type, curType);
			gNode->type=curType;
			printGVar(gNode);
		break;
		}
		gNode=gNode->next;
		}
		node = node->next;
	}
	return;
}

void printLocVars(char *func){
printf("Vars in symTable %s: ", func);
	symTabNode *node;
	node=table;
	while( node != NULL ){
		if(node->type==INTEGER)
			printf("%s=%d, ", node->id, node->iVal);
		else if(node->type==CHARACTER)
			printf("%s=%c, ", node->id, node->cVal);
		else if(node->type==CHARARR)
			printf("%s=%s, ", node->id, node->strVal);
		else if(node->type==INTARR)
			printf("%s, ", node->id);
		node = node->next;
		}
		printf("\n");
}


void addFunction(char *name, Type type){
//printf("Added function %s\n", name);
 	fTabNode *tmp;
 	tmp=fTable;
 	if(checkFTable(fTable, name)==1)
		return;
	tmp=malloc(sizeof(fTabNode));
	tmp->type = type;
	tmp->external = 0;
	tmp->id = strdup(name);
	tmp->defined=1;
	tmp->next = fTable;
	fTable = tmp;
	//printf("Function: %s\n", fTable->id);

}

void addExternalFunction(char *name, Type type){
 	fTabNode *tmp;
 	tmp=fTable;
 	if(checkFTable(fTable, name)==1)
		return;
	tmp=malloc(sizeof(fTabNode));
	tmp->type = type;
	tmp->defined=1;
	tmp->external = 1;
	tmp->id = strdup(name);
	tmp->next = fTable;
	fTable = tmp;
}

 void addProto(char *name, Type type){
//printf("Added prototype %s\n", name);
 	fTabNode *tmp;
 	tmp=fTable;
 	if(checkFTable(fTable, name)==1)
		return;
	tmp=malloc(sizeof(fTabNode));
	tmp->type = type;
	tmp->external = 0;
	tmp->proto = 1;
	tmp->id = strdup(name);
	tmp->next = fTable;
	fTable = tmp;
}

void addExternalProto(char *name, Type type){
 	fTabNode *tmp;
 	tmp=fTable;
 	if(checkFTable(fTable, name)==1)
		return;
	tmp=malloc(sizeof(fTabNode));
	tmp->type = type;
	tmp->proto = 1;
	tmp->external = 1;
	tmp->id = strdup(name);
	tmp->next = fTable;
	fTable = tmp;
}


void setCurType(Type type){
	curType=type;
}

void setCurArrType(){
	if(curType==CHARACTER)
		curType=CHARARR;
	else
		curType=INTARR;
}

void setFuncType(Type type){
	funcType=type;
}

void setScope(int x){
	local=x;				/*1 means local scope, 0 means global scope*/
}

void freeTable(symTabNode *node){
	symTabNode *start=node;
	if(node->next!=NULL){
		start=node->next;
		freeTable(start);
		}
		free(node);
}

void freeFunctions(fTabNode *node){
	fTabNode *start=node;
	paramNode *pStart=node->param;
	paramNode *pTmp=node->param;
	if(node->next!=NULL){
		start=node->next;
		freeFunctions(start);
		}
		while(pStart->next!=NULL){
			pTmp=pStart->next;
			free(pStart);
			pStart=pTmp;
		}
		free(pStart);
		free(node);
}

void createTable(){
	table = malloc(sizeof(symTabNode));
	table->next = NULL;
	table->defined = 0;
	table->type = INTEGER;
	table->id = "$";
	table->isPar=0;
}

void createTempGTable(){
	addedGVars = malloc(sizeof(symTabNode));
	addedGVars->next = NULL;
	addedGVars->defined = 0;
	addedGVars->type = INTEGER;
	addedGVars->id = "$";
}

int checkTable(symTabNode *table, char *id){
	symTabNode *node=table;
	while( node != NULL ){
		if( strcmp(node->id, id) == 0){
			fprintf(stderr, "Error: Extra declaration of %s on line %d\n", id, yylineno);
			exitStatus=1;
			return 1;
		}
		node = node->next;
	}
	return 0;
}

int checkFTable(fTabNode *table, char *id){
	fTabNode *node=fTable;
	while( node != NULL ){
		if( strcmp(node->id, id) == 0){
			if(node->defined==1){
			fprintf(stderr, "Error: Extra declaration of function %s on line %d\n", id, yylineno);
			exitStatus=1;
			return 1;
			}
			else return 0;
		}
		node = node->next;
	}
	return 0;
}

void checkFuncType(char *id, Type type){
	fTabNode *node=fTable;
	while( node != NULL ){
		if( strcmp(node->id, id) == 0){
			if(node->type!=type){
			fprintf(stderr, "Error: function %s on line %d must return VOID\n", id, yylineno);
			exitStatus=1;
			}
			return;
		}
		node = node->next;
	}
	//fprintf(stderr, "Error: function %s undeclared on line %d\n", id, yylineno);
	exitStatus=1;
	return;
}

void checkReturn(){
	if(returnType!=funcType){
		fprintf(stderr, "Error: Incorrect return type on line %d, expected %d but was %d\n", yylineno, funcType, returnType);
		exitStatus=1;
		}
}

exprNode *makeexprNode(Type type, Operation op, int iVal, char cVal, char *strVal){
	exprNode *tmp;
	tmp = malloc(sizeof(exprNode));
	tmp->type = type;
	tmp->op = op;
	
	if(op == CONST){
		tmp->src=newTemp(type);
		tmpNum=tmpNum+1;
		tmp->iVal = iVal;
		tmp->cVal = cVal;
		tmp->strVal=strVal;
	
	}
	else if(op == ID2){
	tmp->iVal = iVal;
	tmp->cVal = cVal;
	tmp->strVal=strVal;
	
	}
	return tmp;
}

exprNode *makeMathNode(Type type, Operation op, exprNode *src1, exprNode *src2){
	exprNode *tmp;
	tmp = malloc(sizeof(exprNode));
	tmp->type = type;
	tmp->op = op;
	tmp->left=src1;
	tmp->right=src2;
	tmp->src=newTemp(type);
	tmpNum=tmpNum+1;
	return tmp;
}





symTabNode *newTemp(Type type){
	symTabNode *tmp;
	tmp = malloc(sizeof(symTabNode));
	char name[7];
	sprintf(name, "$tmp%d", tmpNum);
	return addToTable(name, type, 1);

}

void printGVar(symTabNode *node){
	switch(node->type){
	case INTEGER:
		printf("_%s:  .space 4\n", node->id);
		break;
	case CHARACTER:
		printf("_%s:  .space 1\n", node->id);
		break;
	case CHARARR:
		printf("_%s:  .space 100\n", node->id);
		break;
	
	}
}

exprNode *idToexprNode(char *name, Type type){
	symTabNode *symTabPtr;
	symTabPtr = table;
	fTabNode *fTabPtr;
	fTabPtr = fTable;
	while(symTabPtr != NULL ){
		if(strcmp(symTabPtr->id, name) == 0){
			if(symTabPtr->defined == 0){
				//fprintf(stderr, "Error: Undefined variable, %s used in expression on line %d\n", name, yylineno);
				exitStatus=1;
			}
			exprNode *tmp;
			tmp=makeexprNode(symTabPtr->type, ID2, symTabPtr->iVal, symTabPtr->cVal, symTabPtr->strVal);
			tmp->op=ID2;
			tmp->src=symTabPtr;
			tmp->name=name;
			return tmp;
		}
		symTabPtr = symTabPtr->next;
	}
	symTabPtr = gTable;
	while(symTabPtr != NULL ){
		if(strcmp(symTabPtr->id, name) == 0){
			if(symTabPtr->defined == 0){
				//fprintf(stderr, "Error: Undefined variable, %s used in expression on line %d\n", name, yylineno);
				exitStatus=1;
			}
			exprNode *tmp;
			tmp=makeexprNode(symTabPtr->type, ID2, symTabPtr->iVal, symTabPtr->cVal, symTabPtr->strVal);
			tmp->op=ID2;
			tmp->name=name;
			tmp->src=symTabPtr;
			return tmp;
		}
		symTabPtr = symTabPtr->next;
	}
	while(fTabPtr != NULL ){
	//printf("%s\n", fTabPtr->id);
		if(strcmp(fTabPtr->id, name) == 0){
			if(fTabPtr->defined == 0 && fTabPtr->proto == 0){
				//fprintf(stderr, "Error: Undefined variable, %s used in expression on line %d\n", name, yylineno);
				exitStatus=1;
			}
			exprNode *tmp;
			tmp=makeexprNode(type, FUNCCALL, 0, 0, "");
			tmp->op=FUNCCALL;
			tmp->name=name;
			tmp->src=(symTabNode *)fTabPtr;
			//printf("Made %s an expr node\n", name);
			return tmp;
		}
		fTabPtr = fTabPtr->next;
	}
	//fprintf(stderr, "Error: Undeclared id %s used in expression on line %d\n", name, yylineno);
	exitStatus=1;
	return makeexprNode(INTEGER, ID2, 0, 0, "");
}

exprNode *idToArrexprNode(char *name){

	symTabNode *symTabPtr;
	symTabPtr = table;
	while(symTabPtr != NULL ){
		if(strcmp(symTabPtr->id, name) == 0){
			if(symTabPtr->defined == 0){
				fprintf(stderr, "Error: Undefined variable, %s used in expression on line %d\n", name, yylineno);
				exitStatus=1;
			}
			if(symTabPtr->type==INTARR){
				exprNode *tmp;
			tmp=makeexprNode(symTabPtr->type, ID2, symTabPtr->iVal, symTabPtr->cVal, symTabPtr->strVal);
			tmp->op=ID2;
			tmp->name=name;
			return tmp;																		// arrays are always of value 0.
			}
			else if(symTabPtr->type==CHARARR){
				exprNode *tmp;
			tmp=makeexprNode(symTabPtr->type, ID2, symTabPtr->iVal, symTabPtr->cVal, symTabPtr->strVal);
			tmp->op=ID2;
			tmp->name=name;
			return tmp;

			}
			exprNode *tmp;
			tmp=makeexprNode(symTabPtr->type, ID2, symTabPtr->iVal, symTabPtr->cVal, symTabPtr->strVal);
			tmp->op=ID2;
			tmp->name=name;
			return tmp;
		}
		symTabPtr = symTabPtr->next;
	}

	//fprintf(stderr, "Error: Undeclared variable %s used in expression on line %d\n", name, yylineno);
	exitStatus=1;
	return makeexprNode(INTEGER, ID2, 0, 0, "");
}






exprNode *makeAssgNode( exprNode *node, char *name ){
//printf("Making Assg Node\n");
	symTabNode *tmpSrc=table;
	while(strcmp(tmpSrc->id, name)!=0){
	tmpSrc=tmpSrc->next;
	if(tmpSrc==NULL)
	break;
	}
	if(tmpSrc==NULL){
		tmpSrc=gTable;
		while(strcmp(tmpSrc->id, name)!=0){
			tmpSrc=tmpSrc->next;
			}
	}
	exprNode *tmp;
	tmp = makeexprNode( INTEGER, ASSG, 0, 0, "");
	tmp->name = name;
	tmp->op = ASSG;
	tmp->src = tmpSrc;
	tmp->left = node;
	tmp->type=node->type;
	//printf("assigned node\n");
	return tmp;
}

void printSyntaxTree( exprNode *node, int depth ){
	int i;
	for( i = 0; i < depth; i++ ){
		//printf(" ");
	}

	switch( node->op ){
	case ADD:
		//printf("+{\n");
		//printf("%d linked to %s\n", node->iVal, node->src->id);
		//printf("%s = %s + %s\n", node->src->id, node->left->src->id, node->right->src->id);
		printSyntaxTree( node->left, depth + 1);
		printSyntaxTree( node->right, depth + 1);
		node->code=newInstr(ADD, node, node->left, node->right);
		addToInstrList(node->code);
		for( i = 0; i < depth; i++ ){
			//printf(" ");
		}
		//printf("}\n");
		break;
	case SUB:
		//printf("-{\n");
		//printf("%s = %s - %s\n", node->src->id, node->left->src->id, node->right->src->id);
		//printf("%d linked to %s\n", node->iVal, node->src->id);
		printSyntaxTree( node->left, depth + 1);
		printSyntaxTree( node->right, depth + 1);
		node->code=newInstr(SUB, node, node->left, node->right);
		addToInstrList(node->code);
		for( i = 0; i < depth; i++ ){
			//printf(" ");
		}
		//printf("}\n");
		break;
	case MUL:
		//printf("*{\n");
		//printf("%s = %s * %s\n", node->src->id, node->left->src->id, node->right->src->id);
		//printf("%d linked to %s\n", node->iVal, node->src->id);
		printSyntaxTree( node->left, depth + 1);
		printSyntaxTree( node->right, depth + 1);
		node->code=newInstr(MUL, node, node->left, node->right);
		addToInstrList(node->code);
		for( i = 0; i < depth; i++ ){
			//printf(" ");
		}
		//printf("}\n");
		break;
	case DIV:
		//printf("/{\n");
		//printf("%s = %s / %s\n", node->src->id, node->left->src->id, node->right->src->id);
		//printf("%d linked to %s\n", node->iVal, node->src->id);
		printSyntaxTree( node->left, depth + 1);
		printSyntaxTree( node->right, depth + 1);
		node->code=newInstr(DIV, node, node->left, node->right);
		addToInstrList(node->code);
		for( i = 0; i < depth; i++ ){
			//printf(" ");
		}
		//printf("}\n");
		break;
	case NEG:
		//printf("-{\n");
		printSyntaxTree( node->left, depth + 1);
		for( i = 0; i < depth; i++ ){
			//printf(" ");
		}
		//printf("}\n");
		break;
	case CONST:
		if( node->type == INTEGER ){
			node->code=newConstInstr(ASSG, node->src, node);
			addToInstrList(node->code);
			//printf("%d linked to %s\n", node->iVal, node->src->id);
		}
		else if(node->type == CHARACTER) {
			node->code=newConstInstr(ASSG, node->src, node);
			addToInstrList(node->code);
			//printf("%c linked to %s\n", node->cVal, node->src->id);
		}
		else if(node->type == CHARARR) {
			node->code=newConstInstr(ASSG, node->src, node);
			addToInstrList(node->code);
			//printf("%s linked to %s\n", node->strVal, node->src->id);
		}
		break;
	case ID2:
		//printf("%s\n", node->name);
		break;
	case ASSG:
		//printf(" = {\n");
		//printf("%s linked to %s \n", node->name, node->src->id);
		printSyntaxTree( node->left, depth + 1);
		for( i = 0; i < depth; i++ ){
			//printf(" ");
		}
		//printf("}\n");
		node->code=newInstr(ASSG, node, node->left, NULL);
		addToInstrList(node->code);
		break;
	case FUNCCALL:
		//printf("Function %s instruction made\n", node->name);
		node->code=newInstr(FUNCCALL, node, NULL, NULL);
		node->code->params=node->parameters;
		//printf("%s is param\n", node->code->params->param->src->id);
		addToInstrList(node->code);
		break;
	default:
		printf("# Unsupported syntax operation\n");
		break;
	}
}


instruction *newInstr(Operation op, exprNode *dest, exprNode *src1, exprNode *src2){
	instruction *instr;
	instr=malloc(sizeof(instruction));
	instr->dest=dest->src;
	instr->type=dest->type;
	instr->op=op;
	instr->next=NULL;
	switch (op){
	case ASSG:
		instr->src1=src1->src;
		break;
	case ADD:
		instr->src1=src1->src;
		instr->src2=src2->src;
		break;
	case SUB:
		instr->src1=src1->src;
		instr->src2=src2->src;
		break;
	case MUL:
		instr->src1=src1->src;
		instr->src2=src2->src;
		break;
	case DIV:
		instr->src1=src1->src;
		instr->src2=src2->src;
		break;
	case FUNCCALL:
		break;
	default:
		printf("# Unsupported operation\n");
		break;
	}
	//printf("New instruction made\n");
	return instr;
}


void printInstructions(char *fName){

	fTabNode *fTabPtr=fTable;
	instruction *instrPtr;
	while(strcmp(fTabPtr->id, fName)!=0){
	
		fTabPtr = fTabPtr->next;
	}
	printf("# Enter %s\n", fTabPtr->id);
	if(strcmp(fTabPtr->id, "main")!=0)
	printf("_");
printf("%s:\n", fTabPtr->id);
	printf("# START PROLOGUE ###########################################\n");
	printf("la $sp, -8($sp)  # allocate space for old $fp and $ra\n");
	printf("sw $fp, 4($sp)   # Save old frame pointer\n");
	printf("sw $ra, 0($sp)   # Save return Address\n");
	printf("la $fp, 0($sp)   # set up frame pointer\n");
	printf("la $sp, -%d($sp)  # allocate stack frame\n", countLocVars()*4);
	printf("# END PROLOGUE ##############################################\n");
	instrPtr=fTabPtr->instrs;
	printInstrSet(instrPtr);
	printf("# Start EPILOGUE ##############################################\n");
	printf("la    $sp, 0($fp)    # deallocate locals\n");
	printf("lw    $ra, 0($sp)    # restore return address\n");
	printf("lw    $fp, 4($sp)    # restore frame pointer\n");
	printf("la    $sp, 8($sp)    # restore stack pointer\n");
	printf("jr    $ra            # Return to caller\n");
	printf("#	 END EPILOGUE #################################################\n");
	printf("# Exit %s\n\n", fTabPtr->id);
}

int countLocVars(){
	int j;
	int loc=-4;
	int par=4;
	symTabNode *symTabPtr;
	symTabPtr = table;
	while( symTabPtr != NULL ){ 
		//printf("%s, ", symTabPtr->id);
		if(symTabPtr->isPar==1){
		symTabPtr->offset=par;
		//printf("%s is a param, offset %d\n", symTabPtr->id, symTabPtr->offset);
		par+=4;
		}
		else{
		symTabPtr->offset=loc;
		//printf("%s is local, offset %d\n", symTabPtr->id, symTabPtr->offset);
		loc-=4;
		}
		j++;
		symTabPtr=symTabPtr->next;
	}
	return j;
}

void printInstrSet(instruction *instr){
	while(instr!=NULL){
//	if(instr->next==NULL)
//	printf("Next is null\n");
//	return;
	
	//else
	//printf("Next is %d src %s\n", instr->next->type, instr->next->dest->id);
		switch (instr->op){
		case ASSG:
			if(instr->type==INTC){
			printf("# %s = %d\n", instr->dest->id, instr->iVal);
			printf("li $t2, %d\n",instr->iVal);
			printf("sw $t2, %d($fp)\n\n", instr->dest->offset);
			instr=instr->next;
			}
			else if(instr->type==CHARC){
			printf("# %s = %c\n", instr->dest->id, instr->cVal);
			printf("li $t2, %d\n",instr->cVal);
			printf("sw $t2, %d($fp)\n\n", instr->dest->offset);
			instr=instr->next;
			}
			else if(instr->type==STRINGC){
			printf("# %s = %s\n\n", instr->dest->id, instr->strVal);
			//printf("	%s=%s\n", instr->dest->id, instr->strVal);
			instr=instr->next;
			}
			else{
			printf("# %s = %s\n", instr->dest->id, instr->src1->id);
			printf("lw $t2, %d($fp)\n", instr->src1->offset);
			printf("sw $t2, %d($fp)\n\n", instr->dest->offset);
			instr=instr->next;
			}
			break;
		case ADD:
			printf("# %s = %s + %s\n", instr->dest->id, instr->src1->id, instr->src2->id);
			printf("lw  $t0, %d($fp)\n", instr->src1->offset);
			printf("lw  $t1, %d($fp)\n", instr->src2->offset);
			printf("add  $t2, $t1, $t0\n");
			printf("sw  $t2, %d($fp)\n\n", instr->dest->offset);
			instr=instr->next;
			break;
		case SUB:
			printf("# %s = %s - %s\n", instr->dest->id, instr->src1->id, instr->src2->id);
			printf("lw  $t0, %d($fp)\n", instr->src1->offset);
			printf("lw  $t1, %d($fp)\n", instr->src2->offset);
			printf("sub  $t2, $t1, $t0\n");
			printf("sw  $t2, %d($fp)\n\n", instr->dest->offset);
			instr=instr->next;
			break;
		case MUL:
			printf("# %s = %s * %s\n", instr->dest->id, instr->src1->id, instr->src2->id);
			printf("lw  $t0, %d($fp)\n", instr->src1->offset);
			printf("lw  $t1, %d($fp)\n", instr->src2->offset);
			printf("mul  $t2, $t1, $t0\n");
			printf("sw  $t2, %d($fp)\n\n", instr->dest->offset);
			instr=instr->next;
			break;
		case DIV:
			printf("# %s = %s / %s\n", instr->dest->id, instr->src1->id, instr->src2->id);
			printf("lw  $t0, %d($fp)\n", instr->src1->offset);
			printf("lw  $t1, %d($fp)\n", instr->src2->offset);
			printf("div  $t2, $t1, $t0\n");
			printf("sw  $t2, %d($fp)\n\n", instr->dest->offset);
			instr=instr->next;
			break;
		case FUNCCALL:
			printf("# Calling function %s\n", instr->dest->id);
			printParams(instr);
			printf("jal %s\n", instr->dest->id);
			printf("la $sp, 4($sp)\n");
			instr=instr->next;
			break;
		default:
			printf("#	uncovered instruction\n");
			instr=instr->next;
			break;
		}
	}
}

void printParams(instruction *instr){
	int numOfParams=0;
	paramNode *paramPtr=instr->params;
	if(paramPtr==NULL)
	printf("Params are null!\n");
	while(paramPtr!=NULL){
		printf("# parameter %s\n", paramPtr->param->src->id);
		if(paramPtr->isGlobal==1){
		printf("la $t1, _%s\n", paramPtr->param->src->id);
		printf("lw $t0, 0($t1)\n");
		}
		else{
		printf("lw $t0, %d($fp)\n", paramPtr->param->src->offset);
		}
		printf("la $sp, -4($sp)\n");
		printf("sw $t0, 0($sp)\n\n");
		//if(paramPtr->next!=NULL)
		//printf("next is %s\n", paramPtr->next->param->src->id);
		//else printf("next is Null\n");
		if(paramPtr->next!=NULL)
		paramPtr=paramPtr->next;
		else
		break;
	}
	printf("# End of parameters\n\n");
}
instruction *newConstInstr(Operation op, symTabNode *place, exprNode *expr){
	instruction *instr;
	instr=malloc(sizeof(instruction));
	instr->dest=expr->src;
	instr->op=op;
	instr->next=NULL;
	if(expr->type==INTEGER){
		instr->iVal=expr->iVal;
		instr->type=INTC;
		}
	else if(expr->type==CHARACTER){
		instr->cVal=expr->cVal;
		instr->type=CHARC;
		}
	else if(expr->type==CHARARR){
		instr->strVal=expr->strVal;
		instr->type=STRINGC;
		}
	//printf("Made constant instruction\n");
	return instr;
}


void addToInstrList(instruction *instr){
	instruction *instrPtr;
	instrPtr=instructionList;
	if(instrPtr==NULL){
		//printf("first instr in list\n");
		instructionList=instr;
		return;
	}

	while(instrPtr->next!=NULL){
		//printf("next instr\n");
		instrPtr=instrPtr->next;
	}
	instrPtr->next=malloc(sizeof(instruction));
	instrPtr->next->dest=instr->dest;
	instrPtr->next->src1=instr->src1;
	instrPtr->next->src2=instr->src2;
	instrPtr->next->op=instr->op;
	instrPtr->next->type=instr->type;
	instrPtr->next->iVal=instr->iVal;
	instrPtr->next->cVal=instr->cVal;
	instrPtr->next->strVal=instr->strVal;
	instrPtr->next->params=instr->params;
	//if(instrPtr->next->dest==NULL)
	//printf("next instr has null destination\n");
	//else
	//printf("Added instr, op is %d, dest is %s\n", instrPtr->next->op, instrPtr->next->dest->id);

}

void addInstrListToFunc(char *fname, instruction *instr){
	fTabNode *fTabPtr=fTable;
	instruction *instrPtr;
	while(fTabPtr != NULL ){
	//printf("%s\n", fTabPtr->id);
		if(strcmp(fTabPtr->id, fname) == 0){
			instrPtr=fTabPtr->instrs;
			while(instrPtr!=NULL){
				instrPtr=instrPtr->next;
				//printf("next instruction\n");
			}
			//printf("Found blank spot for instruction\n");
			//instrPtr=malloc(sizeof(instruction));					??
			fTabPtr->instrs=instr;
		}
		fTabPtr = fTabPtr->next;
	}
	instructionList=malloc(sizeof(instruction));
	instructionList=NULL;
}

void printexprNode(exprNode *node){
	
//printf("%s is of type %d\n", node->name, node->type);
	if( node->type == INTEGER){
		//printf( "%d\n", node->iVal);
	}
	else if(node->type == CHARACTER) {
			//printf("%c\n", node->cVal);
		}
	else if(node->type == CHARARR) {
			//printf("%s\n", node->strVal);
		}
		//return;
}

void checkIfBool(exprNode *expr){
	if(expr->type==BOOL)
	return;
	else {
	fprintf(stderr,"Error: Conditional statement on line %d must be of type Boolean\n", yylineno);
	exitStatus=1;
	}
}


 symTabNode *addToTable(char *name, Type type, int scope){
	symTabNode *tmpNode;
	if(scope==1){
	//printf("Added local variable %s\n", name);
		if(checkTable(table, name)==1)
			return NULL;
		tmpNode = malloc(sizeof(symTabNode));
		tmpNode->type = type;
		tmpNode->defined = 0;
		tmpNode->id = strdup(name);
		tmpNode->next = table;
		table = tmpNode;
		if(isParam==1)
		tmpNode->isPar=1;
	}
	else{
	//printf("Added global variable %s of type %d\n", name, type);
		if(checkTable(gTable, name)==1)
			return NULL;
		symTabNode *tmpNode;
		tmpNode = malloc(sizeof(symTabNode));
		tmpNode->type = type;
		tmpNode->defined = 0;
		tmpNode->id = strdup(name);
		tmpNode->next = gTable;
		gTable = tmpNode;

		symTabNode *addedG;
		addedG = malloc(sizeof(symTabNode));
		addedG->type = type;
		addedG->defined = 0;
		addedG->id = strdup(name);
		addedG->next = addedGVars;
		addedGVars=addedG;
	}
			return tmpNode;
} 

void setVarValue(char *name, exprNode *expr){
	symTabNode *symTabPtr;
	symTabPtr = table;
	while( symTabPtr != NULL ){ 								/*Check local variables*/
		if( strcmp(symTabPtr->id, name) == 0){
			if(symTabPtr->type==expr->type){
				symTabPtr->defined = 1;
				symTabPtr->iVal=expr->iVal;
				symTabPtr->cVal=expr->cVal;
				return;
			}
			else if((symTabPtr->type==0)&&(expr->type==1)){
				symTabPtr->defined = 1;
				symTabPtr->iVal=expr->cVal;
				return;
			}
			else if((symTabPtr->type==1)&&(expr->type==0)){
				symTabPtr->defined = 1;
				symTabPtr->cVal=expr->iVal;
				return;
			}
			else
			//fprintf(stderr, "Error: tried to assign improper type to %s on line %d\n", name, yylineno);
			symTabPtr->type=expr->type;
			exitStatus=1;
			return;
		}
		symTabPtr = symTabPtr->next;
	}
	symTabPtr = gTable; 										/*Check global variables*/
	while( symTabPtr != NULL ){
		if( strcmp(symTabPtr->id, name) == 0){
			if(symTabPtr->type==expr->type){
				symTabPtr->defined = 1;
				symTabPtr->cVal=expr->cVal;
				symTabPtr->iVal=expr->iVal;
				return;
			}
			else if((symTabPtr->type==0)&&(expr->type==1)){
				symTabPtr->defined = 1;
				symTabPtr->iVal=expr->cVal;
				return;
			}
			else if((symTabPtr->type==1)&&(expr->type==0)){
				symTabPtr->defined = 1;
				symTabPtr->cVal=expr->iVal;
				return;
			}

			else
			//fprintf(stderr, "Error: tried to assign improper type to %s on line %d, expected %d, got %d\n", name, yylineno, symTabPtr->type, expr->type);
			symTabPtr->type=expr->type;
			exitStatus=1;
			return;
		}
		symTabPtr = symTabPtr->next;
	}
	
	//fprintf(stderr, "Error: Undeclared variable %s assigned to on line %d\n", name, yylineno);
	exitStatus=1;
}

void setArrayValue(char *name, exprNode *index, exprNode *expr){
	symTabNode *symTabPtr;
	symTabPtr = table;
	if(index->type!=INTEGER){
	fprintf(stderr,"Error: %s on line %d requires int as an index\n", name, yylineno);
	exitStatus=1;
	}
	while( symTabPtr != NULL ){
		if( strcmp(symTabPtr->id, name) == 0){
			if((expr->type==INTEGER)||(expr->type==CHARACTER)){
				symTabPtr->defined = 1;
																					
				return;
			}
			else
			fprintf(stderr, "Error: tried to assign improper type to %s on line %d\n", name, yylineno);
			exitStatus=1;
			return;
		}
		symTabPtr = symTabPtr->next;
	}
	//fprintf(stderr, "Error: Undeclared variable assigned to on line %d\n", yylineno);
	exitStatus=1;
}

int getIdType(char *name){
	symTabNode *symTabPtr;
	symTabPtr = table;
	while( symTabPtr != NULL ){
		if( strcmp(symTabPtr->id, name) == 0){
			if( symTabPtr->defined == 0 ){
				fprintf(stderr, "Error: Undefined variable, %s used in expression on line %d\n", name, yylineno);
				exitStatus=1;
			}
			return symTabPtr->type;
		}
		symTabPtr = symTabPtr->next;
	}
	
	//fprintf(stderr, "Error: Undeclared variable used in expression on line %d\n", yylineno);
	exitStatus=1;
	return 0;
}



