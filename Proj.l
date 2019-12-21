%option noyywrap
%{
	#include <stdio.h>
	#include <string.h>
	#include "y.tab.h"
	void yyerror (char *s){
		fprintf(stderr,"Erreur syntaxique!\n");
	}
	int puissance(int a, int n)
	{
		int res=1,i;
		for(i=1;i<=n;i++)
		{
		  res=res*a;
		}
		return res;
	}
%}

ELEMENT ([1-2]?[0-9]|30)
ENSEMBLE [A-Z]
VARIABLE [a-z]
UNION UN
INTERSECTION IN
CARDINALE card
COMPLEMENTAIRE comp
BLANC [ \t\n]+
%%

[+\-*/(),=;{}\\] {
			//printf("%c",yytext[0]);
			return yytext[0];
		 }
{ENSEMBLE}	 {
			//printf("%s",yytext); 
			yylval.car=yytext[0];
			return Ens;
		 }
{VARIABLE}	 {
			//printf("%s",yytext); 
			yylval.car=yytext[0];
			return Id;
		 }
{UNION}		 {
			//printf("%s",yytext); 
			return Un;
		 }
{INTERSECTION}   {
			//printf("%s",yytext);
			return In;
		 }
{CARDINALE}	 {
			//printf("%s",yytext);
			return Card;
		 }
{COMPLEMENTAIRE} {
			//printf("%s",yytext);
			return Comp;
		 }
{ELEMENT}	 { 
			yylval.valeur=puissance(2,atoi(yytext)-1);
			//printf("%d",yylval.valeur);
			return Nbr;
		 }
{BLANC}		 {printf(" ");}
. {printf("Erreur lexical!\n");}

%%
/*int main(int argc, char** argv)
{
	if(argc>=1){
		yyin=fopen("lecture.txt","r");
		yyout=fopen("ecriture.txt","w");
		yylex();
	}else{
		printf("Aucun fichier donné");
	}
}*/