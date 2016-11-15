#include <stdlib.h>
#include <stdio.h>
#include <string.h>


int Push(int **St, int *top, int a);

int Pop(int **St, int *top);

int main()
{
    struct operation
    {
        char label[23];
        char name[5];
        char arg[9];
    };
    typedef struct operation oper;
    int Memory[10000];
    int tp=-1,stc=0,*Stack;
    oper op;
    FILE *f;

    f = fopen("input.txt", "r");
    int sc=0,cr=50;
    oper* code;
    code = (oper*) malloc(cr * sizeof(oper));
    Stack = (int*) malloc(10000 * sizeof(int));

    //-----------PARSER------------
    char st[123];
    while (fgets(st,123,f)){
        int j=0;
        memset(op.label, '\0' ,sizeof (op.label));
        memset(op.name, '\0' ,sizeof (op.name));
        memset(op.arg, '\0' ,sizeof (op.arg));
        short ff=0,fl=0,fj=0,fa=0;
        for (j=0;j<=strlen(st);j++){
            if (st[j]==':'){
                strncpy(op.label,st,j);
                fj=j+1;
            }
            if (st[j]==' '){
                strncpy(op.name,st+fj,j-fj);
                fl=j+1;
            }
            if (st[j]==';') {
                ff=j+1;
                break;
            }
        }
        if (ff)
        {
            char c[123];
            memset(c, '\0' ,sizeof (c));
            strncpy(c,st,ff-1);
            strcpy(st,c);
        }
        if (strrchr(st, '\n')!=NULL)
            fa=1;
        if (fl)
            strncpy(op.arg,st+fl,strlen(st)-fl-fa);
        else
            strncpy(op.name,st+fj,strlen(st)-fa);

        if ((strlen(op.label)>0)||((strlen(op.name)>0)||((strlen(op.arg)>0)))){
            sc++;
            if (sc>cr)
            {
                cr+=50;
                code = (oper*) realloc(code,cr * sizeof (oper));
            }
            strcpy(code[sc-1].label,op.label);
            strcpy(code[sc-1].name,op.name);
            strcpy(code[sc-1].arg,op.arg);
        }

    }

    int i;
    for (i=0;i<sc;i++)
        if (strlen(code[i].label)>0)
            printf("%s:%s %s\n", code[i].label, code[i].name, code[i].arg);
        else
            printf("%s %s\n", code[i].name, code[i].arg);

    i=0;
    short b=1;
    while ((b)&&(i<sc)) {
        short upd=0;
        if (!strcmp(code[i].name,"ld"))
        {
            int a = atoi(code[i].arg);
            stc++;
            Push(&Stack, &tp, Memory[a]);
        }

        if (!strcmp(code[i].name,"st"))
        {
            int a = atoi(code[i].arg);
            Memory[a] = Pop(&Stack,&tp);
            stc--;
        }

        if (!strcmp(code[i].name,"ldc"))
        {
            int a = atoi(code[i].arg);
            stc++;
            Push(&Stack, &tp, a);
        }

        if (!strcmp(code[i].name,"add"))
        {
            int a = Pop(&Stack,&tp);
            int b = Pop(&Stack,&tp);
            stc--;
            Push(&Stack, &tp, a+b);
        }

        if (!strcmp(code[i].name,"sub"))
        {
            int a = Pop(&Stack,&tp);
            int b = Pop(&Stack,&tp);
            stc--;
            Push(&Stack, &tp, a-b);
        }

        if (!strcmp(code[i].name,"cmp"))
        {
            int a = Pop(&Stack,&tp);
            int b = Pop(&Stack,&tp);
            stc--;
            if (a==b)
                Push(&Stack, &tp, 0);
            if (a>b)
                Push(&Stack, &tp, 1);
            if (a<b)
                Push(&Stack, &tp, -1);
        }

        if (!strcmp(code[i].name,"jmp"))
        {
            int j=0;
            while (strcmp(code[j].label,code[i].arg)&&(j<sc))
                j++;
            upd=j;
        }

        if (!strcmp(code[i].name,"br"))
        {
            int a = Pop(&Stack,&tp);
            if (a!=0){
                int j=0;
                while (strcmp(code[j].label,code[i].arg)&&(j<sc))
                    j++;
                upd=j;
            }
            Push(&Stack,&tp,a);
        }

        if (!strcmp(code[i].name,"ret"))
            b=0;

        if (!upd)
            i++;
        else i=upd;
    }
    printf("\nFinal result: %d\n",Pop(&Stack,&tp));
    getchar();
    fclose(f);
    return 0;
}

int Push(int **St, int *top, int a)
{
    (*top)++;
    (*St)[(*top)]=a;
    return 0;
}

int Pop(int **St, int *top)
{
    int k = (*top);
    (*top)= (*top) - 1;
    return (*St)[k];
}
