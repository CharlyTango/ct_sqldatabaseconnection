program project;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils,
  Forms, memdslaz, Unit1;
  //, uguessfile, fcredentials, frame_example1
{$R *.res}

begin
    (*
   * Diese Demo kann prinzipiell auch ohne Leakview genutzt werden.
   *
   * Richtig toll wird es aber erst, wenn man das Package "Leakview" installiert, dann ist unter
   * Ansicht -> Leaks and Traces ein neuer Dialog verfügbar.
   *
   * Damit Heaptrace sauber arbeiten kann sollte
   * -gh = Einschalten Heaptrace
   * -gl = Zeilennummern
   * Aktiviert sein.
   *
   * Kommt in Leakview stets die Meldung "kann unit** nicht finden" dann kann es helfen im Reiter
   * Projekt -> Projekteinstellungen -> Compilereinstellungen -> Debuggen -> Debugging-Info-Typ auf "dwarf" um zu stellen.
   *)

  (*
   * Zeigt den Heaptrace Result dialog nur an, wenn auch Leaks da sind
   *)
{$IF declared(UseHeapTrace)}
  GlobalSkipIfNoLeaks := True;
  (*
   * Alten Trace Löschen, wenn es einen Gibt
   *)
  If fileexists('HeapTrace.trc') Then
    deletefile('HeapTrace.trc');
  (*
   * Dafür sorgen, dass Heaptrace auch in die .trc Datei loggt.
   *)
  SetHeapTraceOutput('HeapTrace.trc');
{$ENDIF}
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

