datatype a = A;
datatype b = B;

signature G = sig datatype d = C of a * b | D end;
signature L = sig datatype d = C of (a * b) | D end;

functor L = functor (L:L)=>
  L:G;
functor LL = functor(LL:functor(L:L)->L)=>
  LL:functor(L:L)->G;
functor GL = functor(GL:functor(L:G)->L)=>
  GL:functor(L:L)->L;
functor YL = functor(YL:sig structure Y:L end)=>
  YL:sig structure Y: G end;
functor L_YL = functor(L_YL:functor(L:L)->sig structure Y:L end)=>
  L_YL:functor(L:L)->sig structure Y: G end;
functor YG_L = functor(YG_L:functor(YG:sig structure Y:G end)->L)=>
  YG_L:functor(YL:sig structure Y: L end)->L;
functor L_YG_L = functor(L_YG_L:functor(L:L)->functor(YG:sig structure Y:G end)->L)=>
  L_YG_L:functor(L:L)->functor(YL:sig structure Y:L end)->L;
functor FLYGL = functor(FLYGL:sig functor F: functor(L:L)->functor(YG:sig structure Y:G end)->L end)=>
  FLYGL:sig functor F: functor(L:L)->functor(YL:sig structure Y:L end)->L end;


signature G = sig datatype d = C of {a:a} | D end;
signature L = sig datatype d = C of ({a:a}) | D end;

functor L = functor (L:L)=>
  L:G;
functor LL = functor(LL:functor(L:L)->L)=>
  LL:functor(L:L)->G;
functor GL = functor(GL:functor(L:G)->L)=>
  GL:functor(L:L)->L;
functor YL = functor(YL:sig structure Y:L end)=>
  YL:sig structure Y: G end;
functor L_YL = functor(L_YL:functor(L:L)->sig structure Y:L end)=>
  L_YL:functor(L:L)->sig structure Y: G end;
functor YG_L = functor(YG_L:functor(YG:sig structure Y:G end)->L)=>
  YG_L:functor(YL:sig structure Y: L end)->L;
functor L_YG_L = functor(L_YG_L:functor(L:L)->functor(YG:sig structure Y:G end)->L)=>
  L_YG_L:functor(L:L)->functor(YL:sig structure Y:L end)->L;
functor FLYGL = functor(FLYGL:sig functor F: functor(L:L)->functor(YG:sig structure Y:G end)->L end)=>
  FLYGL:sig functor F: functor(L:L)->functor(YL:sig structure Y:L end)->L end;


signature G = sig datatype d = C of {a:a,b:b} | D end;
signature L = sig datatype d = C of ({a:a,b:b}) | D end;

functor L = functor (L:L)=>
  L:G;
functor LL = functor(LL:functor(L:L)->L)=>
  LL:functor(L:L)->G;
functor GL = functor(GL:functor(L:G)->L)=>
  GL:functor(L:L)->L;
functor YL = functor(YL:sig structure Y:L end)=>
  YL:sig structure Y: G end;
functor L_YL = functor(L_YL:functor(L:L)->sig structure Y:L end)=>
  L_YL:functor(L:L)->sig structure Y: G end;
functor YG_L = functor(YG_L:functor(YG:sig structure Y:G end)->L)=>
  YG_L:functor(YL:sig structure Y: L end)->L;
functor L_YG_L = functor(L_YG_L:functor(L:L)->functor(YG:sig structure Y:G end)->L)=>
  L_YG_L:functor(L:L)->functor(YL:sig structure Y:L end)->L;
functor FLYGL = functor(FLYGL:sig functor F: functor(L:L)->functor(YG:sig structure Y:G end)->L end)=>
  FLYGL:sig functor F: functor(L:L)->functor(YL:sig structure Y:L end)->L end;

