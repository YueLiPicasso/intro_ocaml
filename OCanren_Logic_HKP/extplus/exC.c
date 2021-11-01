#define CAML_NAME_SPACE
#include <stdio.h>
#include "caml/mlvalues.h"

CAMLprim value plus_native (value x1, value x2, value x3, value x4, value x5, value x6)
{
  printf("<< NATIVE PLUS >>\n");
  fflush(stdout);
  return Val_long (Long_val(x1) + Long_val(x2) + Long_val(x3)
		   + Long_val(x4) + Long_val(x5) + Long_val(x6)) ;
}


CAMLprim value plus_bytecode (value * tab_val, int num_val)
{
  int i;
  long res;
  printf("<< BYTECODE PLUS >>\n");
  fflush(stdout);
  for (i=0,res=0;i<num_val;i++) res += Long_val(tab_val[i]) ;
  return Val_long(res) ;
}

CAMLprim value inspect (value v)
{
  if (Is_long(v))
    printf ("v is an integer (%ld) : %ld\n\n", (long)v, Long_val(v));
  else if (Is_block(v))
    printf("v is a pointer: %ld\n\n", (long)v);
  else printf("neither integer nor pointer\n\n");
  fflush(stdout);
  return v;
}


void margin (int n)
{ while (n-- > 0) printf("."); return; }


void print_block (value v, int m)
{
  long size, i;
  margin(m);
  if (Is_long(v))
    { printf("Immediate value (%ld)\n", Long_val(v)); return; }
  printf ("memory block: size=%ld  -  ", size=Wosize_val(v));
  switch (Tag_val(v))
    {
    case Closure_tag :
      printf("Closure with %ld free variables\n", size-1);
      margin(m+4); printf("code pointer: %p\n", Code_val(v));
      for (i=1; i<size;i++)
	if (Is_long(Field(v,i))) print_block(Field(v,i), m+4);
      break;
    case String_tag :
      printf("string: %s (%s)\n", String_val(v), (char *) v);
      break;
    case Double_tag :
      printf ("float: %g\n", Double_val(v));
      break;
    case Double_array_tag :
      printf ("float array: ");
      for (i=0;i<size/Double_wosize;i++) printf("  %g", Double_field(v,i));
      printf("\n");
      break;
    case Abstract_tag : printf("abstract type"); break;
    default:
      if (Tag_val(v) >= No_scan_tag) {printf("unkonwn tag"); break;};
      printf("structured block (tag=%d):\n", Tag_val(v));
      for (i=0;i<size;i++) print_block(Field(v,i),m+4);
    }
  return;
}

value inspect_block (value v)
{print_block(v,4); fflush(stdout); return v;}

value explore_string (value v)
{
  char *s;
  long int i,j, size, vs, ws;
  s = (char *) v;
  ws = Wosize_val(v);
  vs = sizeof(value);  
  printf("string of %ld words:\n", ws);
  for (j=0;j<ws;j++){
    for (i=0;i<vs;i++)
      {
	char p = s[j*vs+i];
	if ((p>31) && (p<128)) printf("%c",p); else printf("(#%u)",p);
      }
    printf("\n");
  }
  fflush(stdout);
  return v;
}
