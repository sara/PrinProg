
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "InstrUtils.h"
#include "Utils.h"
#include "InstrUtils.c"


void getCritical(Instruction* head)
{
	int i = 0;
	int numInstr = 0;
	while (head->next!=NULL)
	{
		head->critical = 0;
		numInstr++;
		head = head->next;
	}
	int** critical = (int**)malloc(sizeof(int*)*numInstr);
	for (i=0; i<numInstr; i++)
	{
		critical[i] = (int*)malloc(sizeof(int)*numInstr);	
	}

	while (head!=NULL)
	{
		switch (head->opcode)
		{
			case OUTPUTAI:
				head-> critical = 1;
				critical[head->field1][head->field2]++;
				critical[head->field1][0]++;
				break;
			case DIV:
			case MUL:
			case SUB:
			case ADD:
				if (critical[head->field3][0] > 0)
				{
					head->critical = 1;
					critical[head->field3][0] --;
					critical[head->field1][0]++;
					critical[head->field2][0]++;
				}
				break;
			case STOREAI:
				if ((head->field3==0 && critical[head->field2][head->field3]>1) || ( head->field3!=0 && critical[head->field2][head->field3] >0))
				{
					head->critical = 1;
					critical[head->field2][head->field3]--;
					critical[head->field1][0]++;
					if (critical[head->field2][0]==0)
					{
						critical[head->field2][0]++;
					}
				}
				break;
			case LOADAI:
				if(critical[head->field3][0] > 0)
				{
					head->critical = 1;
					critical[head->field1][head->field2]++;
					critical[head->field1][0] ++;
					critical[head->field3][0]--;
				}
				break;
			case LOADI:
				if(critical[head->field2][0]>0)
				{
					critical[head->field2][0]--;
					head->critical = 1;
				}
				break;
		}
		head = head->prev;
	}
	
}
Instruction* optimize(Instruction* head)
{
	Instruction* getHead = head;
	while (head!=NULL && head->critical!=1)
	{
		Instruction* temp = head;
		head = head->next;
		getHead = head;
		if (getHead!=NULL)
				getHead->prev = NULL;
		free(temp);
	}
	while(head!=NULL)
	{
		if (head->critical!=1)
		{
			Instruction* temp = head;	
			if (head->prev!=NULL)
			{
				head->prev->next = head->next;
			}
			if (head->next!=NULL)
			{
				head->next->prev = head->prev;
			}
			free(temp);
		}
			head = head->next;
	}
	return getHead;
}
int main()
{
	Instruction *head;

	head = ReadInstructionList(stdin);
	if (!head) {
		WARNING("No instructions\n");
		exit(EXIT_FAILURE);
	}
	getCritical(head);
	
	head = optimize(head);
	if (head) 
		PrintInstructionList(stdout, head);
	return EXIT_SUCCESS;
}


