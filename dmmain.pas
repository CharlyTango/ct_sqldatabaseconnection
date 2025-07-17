unit dmmain;

{$mode ObjFPC}{$H+}

{$DEFINE USELOGINPROMPT}

interface

uses
  Classes, SysUtils, SQLDB, SQLite3Conn,Dialogs,Forms;

const
  LOGFILE = 'sql.log';
  cSQLiteDBName='data.db';

type

  { Tdmsqldb }

  Tdmsqldb = class(TDataModule)
    SQLite3Connection1: TSQLite3Connection;
    SQLTransaction1: TSQLTransaction;
    procedure SQLite3Connection1Log(Sender: TSQLConnection;
      EventType: TDBEventType; const Msg: String);
  private
    FsLogFileName:string;
    FbWriteSQLLogToFile:boolean;
    procedure Writedirecttofile(sFilename:string;s: string; bReset:boolean=false);
  public
    function InitDataModule:boolean;
    procedure OpenConnection(sCalledFrom:string ='');
  end;

var
  dmsqldb: Tdmsqldb;

implementation

{$R *.lfm}

{ Tdmsqldb }

procedure Tdmsqldb.SQLite3Connection1Log(Sender: TSQLConnection;
  EventType: TDBEventType; const Msg: String);
var
  f:TextFile;
  sFileName:string;
  LogString:string;
begin
  if not FbWriteSQLLogToFile then exit;

  case EventType of
    detCustom: LogString:='Custom';
    detPrepare: LogString:='Prepare';
    detExecute: LogString:='Execute';
    detFetch: LogString:='Fetch';
    detCommit: LogString:='Commit';
    detRollBack: LogString:='RollBack';
    detParamValue: LogString:='ParamValue';
    detActualSQL: LogString:='ActualSQL';
  else ;
    LogString:='Unknown';
  end;

  Logstring:=Logstring+' '+ Msg;
  WriteDirecttofile(FsLogFileName, Logstring, false);
end;

procedure Tdmsqldb.Writedirecttofile(sFilename: string; s: string;
  bReset: boolean);
var
   F: Textfile;
begin
    AssignFile(F,sFileName);
    //{$I-}
    try
      if not FileExists(sFileName) then
        Rewrite(F)
      else
        if (bReset ) then
          Rewrite(F)
        else
          Append(F);
      try
        Writeln(F, s);
        //Flush(F);
      finally
        CloseFile(F);
      end;
    except
      on E: EInOutError do
      MessageDlg(sFileName + ' --File access error in procedure "Writedirecttofile" -- '
                           //+ SysErrorMessage(GetLastError) //Getlasterror ist Windows only
                           +'   IOResult:'+inttostr(IOResult) , mtWarning, [mbOk], 0);

    end;
end;

function Tdmsqldb.InitDataModule: boolean;
var
   sFullDBName:string;
begin
  result:=true;
  {All connection requirements between the components are done in GUI therefore less to do}

  //tweak Transaction behaviour here

  //SQLTransaction1.Options:=[stoUseImplicit];
  //stoExplicitStart
  //SQLTransaction1.Params.Add('version1');
  //SQLTransaction1.Params.Add('read_committed');
  //SQLTransaction1.Params.Add('isc_tpb_lock_read=PARMTAB');

  {$ifdef USELOGINPROMPT}
  SQLite3Connection1.LoginPrompt:=true;
  {$endif}

  sFullDBName:=Application.Location + DirectorySeparator + cSQLiteDBName;
  if not FileExists(sFullDBName) then begin
    showmessage('Database file does not exist`:'+sFullDBName);
    result:=false;
    exit;
  end;
  SQLite3Connection1.Databasename:=sFullDBName;

  //Take care of logfile
  FsLogFileName:=Application.Location + DirectorySeparator +LOGFILE;
  WriteDirecttofile(FsLogFileName, 'SQL-Log', true);  //Clear Logfile
  FbWriteSQLLogToFile:=true; //Switch on SQL Log;
end;

procedure Tdmsqldb.OpenConnection(sCalledFrom: string);
begin
  if sCalledFrom='' then sCalledFrom:='unknown';

  if not SQLite3Connection1.Connected then
  begin
    try
      SQLite3Connection1.Open;
    except
      On E : Exception do
        begin
          showmessage( {$I calledfrom.inc}
                      + 'Called From: ' + sCalledFrom + LineEnding
                      + 'Connection to Database failed '+  LineEnding+ LineEnding
                      + E.Message);
        end;
    end;
  end;

end;//OpenConnection


end.

