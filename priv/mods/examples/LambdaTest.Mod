MODULE LambdaTest; (*$MAIN*)
IMPORT C := Console, P := Platform, S := SYSTEM;

TYPE 
  (* однонаправленный список *)
  ItemList = POINTER TO Item;
  ItemPtr = POINTER TO Item;
  Item = RECORD
    next: ItemPtr;
  END;

  lambda = PROCEDURE (item:ItemPtr);

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
  list: ItemList;


(* работа с item типа человек *)
(* создание item *)
PROCEDURE NewHuman(id:INTEGER;name: ARRAY OF CHAR; age, work_y: INTEGER):ItemPtr;
VAR ptr:HumanPtr; item:ItemPtr;
BEGIN
  NEW(ptr);
  ptr.id:=id; ptr.age:=age; ptr.working_years:= work_y;
  ptr.next:=NIL;
  item:=S.VAL(ItemPtr, ptr);
  RETURN item; (* хак дл€ указател€, как на итем *)
END NewHuman;


PROCEDURE ListAdd(el:ItemPtr;li:ItemList):ItemList;
BEGIN
  IF (li=NIL) THEN
    RETURN el
  END;
  el.next:=li;
  RETURN el;
END ListAdd;

(* это л€мбды дл€ HumanItem*)
PROCEDURE (item:ItemPtr) ShowId;
VAR ptr:HumanPtr;
BEGIN
  ptr:=S.VAL(HumanPtr, item);
  C.WriteStr("id=");C.WriteInt(ptr.id);
  C.WriteStr(" name=");C.WriteStr(ptr.name);
  C.WriteLn;
END ShowId;

PROCEDURE ShowId(item:ItemPtr);
BEGIN
  item.ShowId();
END ShowId;
  
BEGIN
  C.Clear(C.Black);
  C.SetColors(C.LWhiteOnBlack);
  C.WriteStrLn("Linked lists and labda's tests:");C.WriteLn;
  
  C.WriteStrLn("New HumanItem, human.ShowId():");
	item:=NewHuman(1, "Lia", 25, 4);
	item.ShowId();
  C.WriteLn;


(*  list:= ListAdd(NewHuman(1, '123', 25, 4), NIL); *)

(*	list:= ListAdd(NewHuman(1, 25, 4),
	        ListAdd(NewHuman(2, 32, 6), 
	        ListAdd(NewHuman(3, 36, 10), 
	        ListAdd(NewHuman(4, 40, 12), 
	        ListAdd(NewHuman(5, 45, 16), 
	        NIL)))));
	
  list.ShowId;
  list.next.ShowId; *)

END LambdaTest.
