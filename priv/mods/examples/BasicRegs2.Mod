MODULE BasicRegs2; (*$MAIN*)
IMPORT b := Basic, d := Debug;
BEGIN
  b.Init; b.CLS; b.AT(1, 0);

  b.COLOR(b.Yellow); b.PRSTR("AT_ROM_fastcall");
  d.SaveRegsDef; b.AT(1, 15); d.CheckRegs;

  b.COLOR(b.Yellow); b.PRSTR("ATTR_fastcall");
  d.SaveRegsDef; IF b.ATTR(1, 15) = 0 THEN END; d.CheckRegs;

  b.COLOR(b.Yellow); b.PRSTR("BEEP_EI");
  d.SaveRegsDef; b.BEEP(10, 1); d.CheckRegs;

  b.COLOR(b.Yellow); b.PRSTR("BORDER");
  d.SaveRegsDef; b.BORDER(3); d.CheckRegs;

  b.COLOR(b.Yellow); b.PRSTR("CIRCLEW_EI");
  d.SaveRegsDef; b.CIRCLEW(250, 170, 5); d.CheckRegs;

  (*b.COLOR(b.Yellow); b.PRSTR("CLS_FULLSCREEN");
  d.SaveRegsDef; b.CLS; d.CheckRegs;*)

  b.COLOR(b.Yellow); b.PRSTR("DRAW_fastcall"); b.PLOT(0, 0);
  d.SaveRegsDef; b.DRAW(3, 3); d.CheckRegs;

  b.COLOR(b.Yellow); b.PRSTR("PAUSE_EI");
  d.SaveRegsDef; b.PAUSE(1); d.CheckRegs;

  b.COLOR(b.Yellow); b.PRSTR("PLOT_fastcall");
  d.SaveRegsDef; b.PLOT(255, 175); d.CheckRegs;

  b.COLOR(b.Yellow); b.PRSTR("POINT_fastcall");
  d.SaveRegsDef; IF b.POINT(0, 0) THEN END; d.CheckRegs;

  b.COLOR(b.Yellow); b.PRSTR("PORTIN");
  d.SaveRegsDef; IF b.PORTIN(254) = 0 THEN END; d.CheckRegs;

  b.COLOR(b.Yellow); b.PRSTR("PORTOUT");
  d.SaveRegsDef; b.PORTOUT(254, 0); d.CheckRegs;

  b.COLOR(b.Yellow); b.PRSTR("PRCHAR_ROM");
  d.SaveRegsDef; b.PRCHAR(" "); d.CheckRegs;

  b.COLOR(b.Yellow); b.PRSTR("PRDATA_ROM");
  d.SaveRegsDef; b.PRDATA; b.DATACH2(" ", 0X); d.CheckRegs;

  b.COLOR(b.Yellow); b.PRSTR("PRESSED");
  d.SaveRegsDef; IF b.PRESSED() THEN END; d.CheckRegs;

  b.COLOR(b.Yellow); b.PRSTR("PRINT_ROM "); b.COLOR(b.Cyan);
  d.SaveRegsDef; b.PRINT(-12345); d.CheckRegs;

  b.COLOR(b.Yellow); b.PRSTR("PRLN_ROM");
  d.SaveRegsDef; b.PRLN; d.CheckRegs;

  b.COLOR(b.Yellow);
  d.SaveRegsDef; b.PRSTR("PRSTR_ROM"); d.CheckRegs;
(*
  b.COLOR(b.Yellow); b.PRSTR("PRUDG "); b.COLOR(b.Cyan);
  d.SaveRegsDef; b.PRUDG("A"); d.CheckRegs;
*)
  b.COLOR(b.Yellow); b.PRSTR("PRWORD_ROM "); b.COLOR(b.Cyan);
  d.SaveRegsDef; b.PRWORD(12345); d.CheckRegs;

  b.COLOR(b.Yellow); b.PRSTR("RANDOMIZE");
  d.SaveRegsDef; b.RANDOMIZE(0); d.CheckRegs;

  b.Quit
END BasicRegs2.
