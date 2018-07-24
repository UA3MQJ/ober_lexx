MODULE MemTest; (*$MAIN*)
IMPORT C := Console, B := Basic, S := SYSTEM;

BEGIN
  C.Clear(C.Black);
  C.SetColors(C.LWhiteOnBlack);

  C.WriteStrLn("Memory test:");
  B.PORTOUT(32765, 16 + 0);
  B.POKE(49152, 1);

  B.PORTOUT(32765, 16 + 1);
  B.POKE(49152, 2);
  
  B.PORTOUT(32765, 16 + 3);
  B.POKE(49152, 3);

  B.PORTOUT(32765, 16 + 4);
  B.POKE(49152, 4);

  B.PORTOUT(32765, 16 + 6);
  B.POKE(49152, 5);

  B.PORTOUT(32765, 16 + 0);
  C.WriteStr("Read Page 0: data:"); C.WriteInt(B.PEEK(49152));C.WriteLn();

  B.PORTOUT(32765, 16 + 1);
  C.WriteStr("Read Page 1: data:"); C.WriteInt(B.PEEK(49152));C.WriteLn();

  B.PORTOUT(32765, 16 + 3);
  C.WriteStr("Read Page 3: data:"); C.WriteInt(B.PEEK(49152));C.WriteLn();

  B.PORTOUT(32765, 16 + 4);
  C.WriteStr("Read Page 4: data:"); C.WriteInt(B.PEEK(49152));C.WriteLn();

  B.PORTOUT(32765, 16 + 6);
  C.WriteStr("Read Page 6: data:"); C.WriteInt(B.PEEK(49152));C.WriteLn();

END MemTest.
