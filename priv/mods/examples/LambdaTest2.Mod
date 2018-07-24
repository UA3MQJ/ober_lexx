MODULE LambdaTest2; (*$MAIN*)
IMPORT C := Console, P := Platform, S := SYSTEM;

TYPE 
  (* однонаправленный список *)
  ItemList = POINTER TO Item;
  ItemPtr = POINTER TO Item;
  Item = RECORD
    next: ItemPtr;
  END;

  lambda = PROCEDURE (item:ItemPtr):ItemPtr;

  Name = ARRAY 12 OF CHAR;
	
	HumanList = POINTER TO Human;
  HumanPtr = POINTER TO Human;
  Human = RECORD(Item)
    id:INTEGER;
    age: INTEGER;
    name: Name;
    working_years: INTEGER;
  END;


VAR
  human, human2: HumanPtr;
  item: ItemPtr;
  hlist: HumanList;
  list, list2: ItemList;

(* общие функции для выноса в отдельную библиотеку *)
(* применяет лямбда функцию ко всем элементам*)
PROCEDURE EnumMap(li:ItemList; fn:lambda):ItemList;
VAR ptr, res:ItemPtr;
BEGIN
  ptr:=li;
  WHILE (ptr#NIL) DO
    res:=fn(ptr);
    ptr:=ptr.next;
  END;
  RETURN li;
END EnumMap;


(* работа с item типа человек *)
(* создание item *)
PROCEDURE NewHuman(id:INTEGER;IN name:Name; age, work_y: INTEGER):ItemPtr;
VAR ptr:HumanPtr; item:ItemPtr;
BEGIN
  NEW(ptr);
  ptr.id:=id; ptr.age:=age; ptr.working_years:= work_y; ptr.name:=name;
  ptr.next:=NIL;
  item:=S.VAL(ItemPtr, ptr);
  RETURN item; (* хак для указателя, как на итем *)
END NewHuman;


PROCEDURE ListAdd(li:ItemList; el:ItemPtr):ItemList;
BEGIN
  IF (li=NIL) THEN
    RETURN el
  END;
  el.next:=li;
  RETURN el;
END ListAdd;

(* это лямбды для HumanItem*)
PROCEDURE (item:ItemPtr) ShowHuman;
VAR ptr:HumanPtr;
BEGIN
  ptr:=S.VAL(HumanPtr, item);
  C.WriteStr("id=");C.WriteInt(ptr.id);
  C.WriteStr(" name=");C.WriteStr(ptr.name);
  C.WriteStr(" age=");C.WriteInt(ptr.age);
  C.WriteStr(" wrk=");C.WriteInt(ptr.working_years);
  C.WriteLn;
END ShowHuman;

PROCEDURE ShowHuman(item:ItemPtr):ItemPtr;
BEGIN
  item.ShowHuman();
  RETURN item;
END ShowHuman;
  
BEGIN
  C.Clear(C.Black);
  C.SetColors(C.LWhiteOnBlack);
  C.WriteStrLn("Linked lists and labda's tests:");C.WriteLn;
  
(*
  C.WriteStrLn("New HumanItem, human.ShowId():");
	item:=NewHuman(1, "Lia", 25, 4);
	item.ShowId();
  C.WriteLn; *)


	list:= ListAdd(NIL,  NewHuman(1, "Lena", 25, 4));
	list:= ListAdd(list, NewHuman(2, "Nary", 32, 6));
	list:= ListAdd(list, NewHuman(3, "Ivye", 36, 10));
	list:= ListAdd(list, NewHuman(4, "Lara", 40, 12));
	list:= ListAdd(list, NewHuman(5, "Math", 45, 16));
	
  list2:=EnumMap(list, ShowHuman);
	
  (* list.ShowHuman;
  list.next.ShowHuman; *)

END LambdaTest2.
