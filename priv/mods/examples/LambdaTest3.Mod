MODULE LambdaTest3; (*$MAIN*)
IMPORT C := Console, P := Platform, S := SYSTEM;

TYPE 

*)
*)

  (* однонаправленный список *)
  (* итем, наследником которого должны быть пользовательские *)


	Item = POINTER TO ItemRec;
  ItemRec = RECORD
  END;


	List = POINTER TO ListItemRec;
  ListItem = POINTER TO ListItemRec;
  ListItemRec = RECORD
    el:Item;
    next:ListItem;
  END;

	(* лямбда у которой на входе Item и на выходе Item *)
  lambda_it2it = PROCEDURE (item:Item):Item;
	(* лямбда у которой на входе Item и на выходе Item *)
  lambda_it2bool = PROCEDURE (item:Item):BOOLEAN;
	(* лямбда у которой на входе Item и на выходе Item *)
  lambda_2it2bool = PROCEDURE (item1,item2:Item):BOOLEAN;

  (* кастомные определения *)
	Name = ARRAY 12 OF CHAR;
  Human = POINTER TO HumanRec;
  HumanRec = RECORD(ItemRec)
    id:INTEGER;
    age: INTEGER;
    name: Name;
    dep: INTEGER;
  END;

VAR
  list, tlist:List;

(* общие функции для выноса в отдельную библиотеку *)
(* применяет лямбда функцию ко всем элементам*)
PROCEDURE ListAdd(li:List; el:Item):List;
VAR ptr:ListItem;
BEGIN
  NEW(ptr);
  ptr.el:=el; ptr.next:=li;
  RETURN ptr;
END ListAdd;

PROCEDURE ListReverse(li:List):List;
VAR ptr, ptr_new:List; lres:Item;
BEGIN
  ptr:=li; ptr_new:=NIL;
  WHILE (ptr#NIL) DO
    ptr_new:=ListAdd(ptr_new, ptr.el);
    ptr:=ptr.next;
  END;
  RETURN ptr_new;
END ListReverse;

(* функции высшего порядка *)
(* применяет функцию лямбда к каждому элементу, 
   в результате должен получиться тоже какой-то элемент или NIL,
   возвращает массив из результатов *)
PROCEDURE ListMap(li:List; fn:lambda_it2it):List;
VAR ptr, ptr_new:List; lres:Item;
BEGIN
  ptr:=li; ptr_new:=NIL;
  WHILE (ptr#NIL) DO
    lres:=fn(ptr.el); 
    ptr_new:=ListAdd(ptr_new, lres);
    ptr:=ptr.next;
  END;
  RETURN ListReverse(ptr_new); (* чтобы не переворачивался список после Map *)
END ListMap;

(* фильтрует список по функции лямбда, в результате, в списке 
   остаются только те элементы, по которым лямбда вернула TRUE *)
PROCEDURE ListFilter(li:List; fn:lambda_it2bool):List;
VAR ptr, ptr_new:List; lres:Item;
BEGIN
  ptr:=li; ptr_new:=NIL;
  WHILE (ptr#NIL) DO
    IF (fn(ptr.el)) THEN
      ptr_new:=ListAdd(ptr_new, ptr.el);
    END;
    ptr:=ptr.next;
  END;
  RETURN ListReverse(ptr_new); (* чтобы не переворачивался список после Map *)
END ListFilter;

(* сортирует список по функции лямбда,
   лямбда должна вернуть TRUE, если первый элемент больше второго,
   в результате получается отсортированный список*)
PROCEDURE ListSort(li:List; fn:lambda_2it2bool):List;
VAR ptr, ptr_new, ptr_tmp:List; a, b:Item; f:BOOLEAN;
BEGIN
  f:=TRUE; ptr_new:=li;
  WHILE (f) DO
    f:=FALSE; ptr:=ptr_new; ptr_tmp:=NIL;
    WHILE (ptr#NIL) DO
      IF(ptr.next=NIL) THEN
        ptr_tmp:=ListAdd(ptr_tmp, ptr.el);
        ptr:=NIL;
      ELSE
        IF(fn(ptr.el, ptr.next.el)) THEN
          ptr_tmp:=ListAdd(ptr_tmp, ptr.el);
          ptr:=ptr.next;
        ELSE
          ptr_tmp:=ListAdd(ptr_tmp, ptr.next.el);
          ptr_tmp:=ListAdd(ptr_tmp, ptr.el);
          ptr:=ptr.next.next; f:=TRUE;
        END;
      END      
    END;
    ptr_new:=ListReverse(ptr_tmp);
	END;
	
  RETURN (ptr_new);
END ListSort;

(* кастомные определения *)
PROCEDURE NewHuman(id:INTEGER;IN name:Name; age, dep: INTEGER):Item;
VAR ptr:Human; item:Item;
BEGIN
  NEW(ptr);
  ptr.id:=id; ptr.age:=age; ptr.dep:= dep; ptr.name:=name;
  RETURN ptr;
END NewHuman;

(* это лямбды для HumanItem*)
PROCEDURE (item:Item) ShowHuman;
VAR ptr:Human;
BEGIN
  ptr:=S.VAL(Human, item);
  C.WriteStr("id=");C.WriteInt(ptr.id);
  C.WriteStr(" name=");C.WriteStr(ptr.name);
  C.WriteStr(" age=");C.WriteInt(ptr.age);
  C.WriteStr(" ceh=");C.WriteInt(ptr.dep);
  C.WriteLn;
END ShowHuman;

(* lambda = PROCEDURE (item:Item):Item; 
  для функции MAP, но только для вывода*)
PROCEDURE ShowHuman(item:Item):Item;
VAR ptr:Human;
BEGIN
  item.ShowHuman();
  RETURN item; (* сам итем не меняем *)
END ShowHuman;

(* lambda = PROCEDURE (item:Item):Bool; 
  для функции Filter, для фильтрации*)
PROCEDURE over35yr(item:Item):BOOLEAN;
VAR ptr:Human;
BEGIN
  ptr:=S.VAL(Human, item);
  RETURN ptr.age >= 35;
END over35yr;

PROCEDURE only1ceh(item:Item):BOOLEAN;
VAR ptr:Human;
BEGIN
  ptr:=S.VAL(Human, item);
  RETURN ptr.dep = 1;
END only1ceh;

(* первый меньше второго, чтобы по возрастанию *)
PROCEDURE Sort_by_id(item1, item2:Item):BOOLEAN;
VAR ptr1, ptr2:Human;
BEGIN
  ptr1:=S.VAL(Human, item1); ptr2:=S.VAL(Human, item2);
  RETURN ptr1.id < ptr2.id;
END Sort_by_id;

PROCEDURE Sort_by_name(item1, item2:Item):BOOLEAN;
VAR ptr1, ptr2:Human;
BEGIN
  ptr1:=S.VAL(Human, item1); ptr2:=S.VAL(Human, item2);
  RETURN ptr1.name < ptr2.name;
END Sort_by_name;


BEGIN
  C.Clear(C.Black);
  C.SetColors(C.LWhiteOnBlack);
  C.WriteStrLn("Linked lists and labda's tests:");C.WriteLn;
  
	list:= ListAdd(NIL,  NewHuman(1, "Anna", 25, 1));
	list:= ListAdd(list, NewHuman(4, "Clye", 32, 2));
	list:= ListAdd(list, NewHuman(3, "Dame", 36, 1));
	list:= ListAdd(list, NewHuman(5, "Bell", 40, 1));
	list:= ListAdd(list, NewHuman(2, "Xenn", 45, 2));
	(*
  C.WriteLn;C.WriteStrLn("> Show ALL:");
	tlist:=ListMap(list, ShowHuman);
	
  C.WriteLn;C.WriteStrLn("> Show ALL (reversed):");
	tlist:=ListReverse(list);
	tlist:=ListMap(tlist, ShowHuman);*)

(*
  C.WriteLn;C.WriteStrLn("> Show ALL:");
	tlist:=ListReverse(list);
	tlist:=ListMap(tlist, ShowHuman);

  C.WriteLn;C.WriteStrLn("> Show over 35 years old:");
	tlist:=ListReverse(list);
	tlist:=ListFilter(tlist, over35yr);
	tlist:=ListMap(tlist, ShowHuman);
	
  C.WriteLn;C.WriteStrLn("> Over 35 yrs in ceh=1:");
	tlist:=ListReverse(list);
	tlist:=ListFilter(tlist, only1ceh);
	tlist:=ListFilter(tlist, over35yr);
	tlist:=ListMap(tlist, ShowHuman);*)
	
  C.WriteLn;C.WriteStrLn("> Show ALL:");
	tlist:=ListReverse(list);
	tlist:=ListMap(tlist, ShowHuman);
	
  C.WriteLn;C.WriteStrLn("> Sort by id:");
	tlist:=ListReverse(list);
	(* tlist:=ListSort(tlist, Sort_by_id); *)
	tlist:=ListMap(tlist, ShowHuman);
	
	(*
  C.WriteLn;C.WriteStrLn("> Sort by Sort_by_name:");
	tlist:=ListReverse(list);
	tlist:=ListSort(tlist, Sort_by_name);
	tlist:=ListMap(tlist, ShowHuman); *)

END LambdaTest3.
