MODULE ASCII; (** portable CHR *)
IMPORT b := Basic, d := Debug;

TYPE
  List = POINTER TO ListItemRec;
  Name = ARRAY 12 OF CHAR;

VAR
  n: SHORTINT;
VAR
  i: SHORTINT; udg: INTEGER; text2ch: ARRAY 2 OF CHAR;
VAR
  ace: PT3x0A.Melody; udg: INTEGER;

lambda_it2it = PROCEDURE (item:Item):Item;
PROCEDURE ShowHuman(item:Item):Item;
PROCEDURE (item:Item) ShowHuman;
HumanRec = RECORD(ItemRec)
END
HumanRec = RECORD
END

BEGIN (*$MAIN*)
  c:=TRUE;
  e:="asdvsdvsd";
  e:='asdvsdvsd';
  b.Init; b.CLS;
  FOR n := 32-1 TO 127-1 DO Console.WriteCh(CHR(n+1)) END;
END ASCII.
