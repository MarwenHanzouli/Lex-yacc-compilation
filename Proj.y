%{
	#include<stdio.h>
	#include<string.h>
	#include<stdlib.h>
	extern FILE *yyin;
	struct Ensemble{
	  char id;
	  int valeur;
	  char binaire[31];
	};
	struct Variable{
	  char id;
	  int valeur;
	};
	struct Ensemble ens[100];
	int tailleEns=0;
	struct Variable vars[100];
	int tailleVars=0;
	int card(char ch[31])
	{
	  int i,nbr=0;
	  for(i=0;i<strlen(ch);i++)
	  {
	    if(ch[i]=='1')
	    {
	      nbr++;
	    }
	  }
	  return nbr;
	}
	int existeVar(char c)
	{
	  int i=0;
	  int stop=0;
	  while((i<tailleVars)&&(stop==0))
	  {
	     if((char)vars[i].id==(char)c)
	        stop=1;
	     else
	        i++;
	  }
	  if(!stop)
	    return -1;
	  else
	    return i;
	}
	int existeEns(char c)
	{
	  int i=0;
	  int stop=0;
	  while((i<tailleEns)&&(stop==0))
	  {
	     if((char)ens[i].id==(char)c)
	        stop=1;
	     else
	        i++;
	  }
	  if(!stop)
	    return -1;
	  else
	    return i;
	}
	int binaireVersDecimal(char ch[31])
	{
	  int i,res=0,bitD;
	  char bitB[2];
	  for(i=0;i<strlen(ch);i++)
	  {
	    bitB[0]=ch[i];
	    bitB[2]='\0';
	    bitD=atoi(bitB);
	    res=res+bitD*puissance(2,strlen(ch)-1-i);
	  }
	  return res;
	}
	int prive(int A,int B)
	{
	  char binA[31],binB[31];
	  itoa (A,binA,2);
	  itoa (B,binB,2);
	  int longA=strlen(binA),i=0;
	  int longB=strlen(binB),j=0;
	  char binRes[31];
	  while(i<longA)
	  {
		if(i<longB)
		{
			if(binA[strlen(binA)-1-i]=='1' && binB[strlen(binB)-1-i]=='0')
			{
				binRes[strlen(binA)-1-i]='1';
			}
			else
			{
				binRes[strlen(binA)-1-i]='0';
			}
		}
		else
		{
			binRes[strlen(binA)-1-i]=binA[strlen(binA)-1-i];
		}
		i++;
	  }
	  binRes[i]='\0';
	  return binaireVersDecimal(binRes);
	}
	char*  ensemble(int valeur)
	{
		if(valeur==0)
		{
			return "{VIDE}";
		}
		else
		{
			int i,car;
			char *res=NULL;
			char binaire[31],concat[3];
			itoa (valeur,binaire,2);
			int longBin=strlen(binaire);
			res=malloc(100*sizeof(char));
			strcpy(res,"{");
			int t=1;
			for(i=0;i<longBin;i++)
			{
				if(binaire[longBin-1-i]=='1')
				{
					car=i+1;
					if(car >=10)
					{
						res[t]=(car/10)+48;
						t++;
						res[t]=(car%10)+48;
						t++;
						
					}
					else
					{
						res[t]=car+48;
						t++;
					}
					res[t]=',';
					t++;
				}
			}
			res[t-1]='}';
			res[t]='\0';
			return res;
		}
	}

	int comp(int A)
	{
	   char univers[]="111111111111111111111111111111";
	   return prive(binaireVersDecimal(univers),A);
	}
%}
%union { int valeur; char car;}
%token Un
%token In
%token Card
%token Comp
%token <car> Ens
%token <car> Id
%token <valeur> Nbr
%type  <valeur> ELEMS ENSEMBLE EXP_ENS COMP AFFECT_ENS AFFECT_NB EXP_NUM TERME FACTEUR CARD
%%
BLOC       : INSTR 
	   | BLOC INSTR
	   ;
INSTR      : AFFECT_NB ';'
           | AFFECT_ENS ';'
	   | AFFICHE		
           ;
AFFICHE    : Id '=''='		{
				  if(existeVar($1)!=-1)
				  {
				    printf("%d\n",vars[existeVar($1)].valeur);
				  } 	
				  else
				    printf("Identificateur inconnu!\n");
				}
	   | Ens '=''='		{
				  if(existeEns($1)!=-1)
				  {
				    printf("%s\n",ensemble(ens[existeEns($1)].valeur));
				  } 	
				  else
				    printf("Identificateur inconnu!\n");
				}
           ;
AFFECT_ENS : Ens '=' EXP_ENS    {  
				     if($3!=NULL)
				     {
					if(existeEns($1)!=-1)
					 {
					   ens[existeEns($1)].valeur=$3;
					   itoa ($3,ens[existeEns($1)].binaire,2);
					 }
					 else
					 {
					   ens[tailleEns].id=(char)$1;
					   ens[tailleEns].valeur=$3;
					   itoa ($3,ens[tailleEns].binaire,2);
					   tailleEns++;
					 }
					$$=$3;
				      }
				      else
					 printf("Ensemble inconnu!\n");
				}
	   ;
EXP_ENS	   :'(' EXP_ENS ')'		{  
					   $$=$2;
					}
	   | ENSEMBLE			{  
					   $$=$1;
					}
	   | EXP_ENS Un EXP_ENS		{
					   if( $1==NULL || $3==NULL)
						$$=NULL;
					   else
						$$=$1|$3;
					}
	   | EXP_ENS In EXP_ENS		{
					   if( $1==NULL || $3==NULL)
						$$=NULL;
					   else
						$$=$1&$3;
					}
	   | EXP_ENS '\\' ENSEMBLE	{  
					   if( $1==NULL || $3==NULL)
						$$=NULL;
					   else
						$$=prive($1,$3);
					}
	   | COMP
	   ;
COMP	   : Comp '(' EXP_ENS ')'	{  
					   if($3==NULL)
						$$=NULL;
					   else
						$$=comp($3);
					}
	   ;
ENSEMBLE   : '{' '}'			{  $$=0; }
           | '{' ELEMS '}'		{  
					   $$=$2;
					}
	   | Ens			{  
					  if(existeEns($1)!=-1)
					  {
					    $$=ens[existeEns($1)].valeur;
					  }
					  else
					  {
					    $$=NULL;
					    printf("Ensemble inconnu!\n");
					  }
					}
           ;
ELEMS      : Nbr		  {  $$=$1;  }
           | Nbr ',' ELEMS        {  $$=$1|$3;  }
           ;
AFFECT_NB  : Id '=' EXP_NUM	{  
					if($3!=NULL)
					{
					  if(existeVar($1)!=-1)
					 {
					   vars[existeVar($1)].valeur=$3;
					 }
					 else
					 {
					   vars[tailleVars].id=(char)$1;
					   vars[tailleVars].valeur=$3;
					   tailleVars++;
					 }
					 $$=$3;
					}
					else
					 printf("Identificateur inconnu!\n");
					
				}
	   ;
EXP_NUM    : FACTEUR			 
	   | EXP_NUM '+' FACTEUR	{  
					   if( $1==NULL || $3==NULL)
						$$=NULL;
					   else
						$$=$1+$3;
					}
	   | EXP_NUM '-' FACTEUR	{  
					   if( $1==NULL || $3==NULL)
						$$=NULL;
					   else
						$$=$1-$3;
					}
	   | '(' EXP_NUM ')'		{  $$=$2;  }
	   ;
FACTEUR    : TERME		
	   | FACTEUR '*' TERME		{  
					   if( $1==NULL || $3==NULL)
						$$=NULL;
					   else
						$$=$1*$3;
					}
	   | FACTEUR '/' TERME		{  
					   if( $1==NULL || $3==NULL)
						$$=NULL;
					   else
						$$=$1/$3;
					}
	   | '(' FACTEUR ')'		{  
					   $$=$2;
					}
	   ;
TERME      : Nbr		{  
				   char binaire[31];
				   itoa ($1,binaire,2);
				   $$=strlen(binaire);
				}
	   | Id			{  
				   if(existeVar($1)!=-1)
				   {
				     $$=vars[existeVar($1)].valeur;
				   }
				   else
				   {
				     $$=NULL;
				     printf("Identificateur Inconnu!\n");
				   }
				}
	   | CARD
	   ;
CARD	   : Card '(' EXP_ENS ')'	{  
					  char binaire[31];
					  itoa ($3,binaire,2);
					  $$=card(binaire);
					}
	   ;
 
%%

int main()
{	
  yyin=fopen("lecture.txt","r");
  //yyout=fopen("ecriture.txt","w");
  yyparse();
}