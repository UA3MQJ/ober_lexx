MODULE MemTest2; (*$MAIN*)
IMPORT C := Console, B := Basic, S := SYSTEM;

VAR I:INTEGER;

BEGIN
  C.Clear(C.Black);
  C.SetColors(C.LWhiteOnBlack);

  B.PORTOUT(32765, 16 + 0);
  C.Clear(C.Black);
  C.WriteStrLn("Video Page One");

  B.PORTOUT(32765, 16 + 8);
  C.WriteStrLn("                  Video Page Two");

  FOR I:=0 TO 32760 BY 1 DO
    B.PORTOUT(32765, 16 + 0);
    B.PORTOUT(32765, 16 + 8);
  END;


END MemTest2.
