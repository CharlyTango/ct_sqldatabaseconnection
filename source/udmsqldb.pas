unit udmsqldb;

{$mode ObjFPC}{$H+}

//Switches:
//You can disable switches by using a period after the curved
//bracket like this  {.$DEFINE SWITCHNAME}

{If you want to use a SQL log enable the switch}
{.$DEFINE UseSQLLOG}

{Note: an extra possibility to run queries that have no result like  INSERT, DELET or DDL language
 - SQLQuery1 is provided.
Create your own SQLQuery-Components as well as own Datasource-Components in your forms and
connect them with the SQLConnector1
}


interface

uses
  Classes, SysUtils, SQLDBLib, SQLDB, Forms ,inifiles
  {$ifdef UseSQLLOG}
  , LazLogger
  {$endif}

  , SQLite3Conn
  //,pqconnection     //PostgreSQ
  //,ibconnection     //Interbase ,Firebird
  //,mssqlconn        //MSSQL Server , Sybase
  //,oracleconnection //Oracle
  //,odbcconn         //ODBC Access
  ,mysql40conn      //different MySQL Versions as well as MariaDB
  ,mysql41conn
  ,mysql50conn
  ,mysql51conn
  ,mysql55conn
  ,mysql56conn
  ,mysql57conn
  ,mysql80conn

  ;

const
  cSQLiteType='SQLite3';
  cConnectorType='SQLite3';


type
  TDBCredentials = record
    Server: string;
    Port: integer;
    DatabaseName : string;
    Username: string;
    Password: string;
    ConnectorType: string;
    CustomLibraryName:string;
    WasConnectionSuccess:string; //had a valid connection before
    caption:string;
    section:string;
end;

type
  { Tdmsqldb }

  Tdmsqldb = class(TDataModule)
    SQLConnector1: TSQLConnector;
    SQLDBLibraryLoader1: TSQLDBLibraryLoader;
    SQLQuery1: TSQLQuery;
    SQLScript1: TSQLScript;
    SQLTransaction1: TSQLTransaction;
    {$ifdef UseSQLLOG}
    procedure DoSQLLog(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
    {$endif}
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FbAllowOpenDialogOnSQLImportFile: boolean;
    FDeleteDatabasefileBeforeOpen: boolean;
    FImportDemoDataOnCreate: boolean;
    FsConnectMessage: string;
    FsConnectorType: string;
    FsIniFileName: String;
    FIniFile:TMemInifile;
    FsPossibleConnectorTypes: string;
    FrCredentials:TDBCredentials;  //Credentials-Record
    FsPossibleDatabaseConnections: string;  //String of possible Database Connectiond from INIfile
    FsSQLCustomImportFileName: string;
    FbChooseDatabaseOnStartup:boolean;
//    SQLServerType : TSQLServerType;
    procedure SetbAllowOpenDialogOnSQLImportFile(AValue: boolean);
    procedure SetbDeleteDatabasefileBeforeOpen(AValue: boolean);
    procedure SetbImportDemoDataOnCreate(AValue: boolean);
    procedure SetConnectorType(AValue: string);
    procedure SetsIniFileName(AValue: String);

    {determines the standard library for a given connection type depending on the SQL units included in the uses section }
    function GetStandardLibraryName(sConnector:string):string;

    {determines and loads the SQL access library depending on the different operating systems and the selected connection type}
    procedure MyLoadLib(sConnectionType,sLibname : String);
    function MyGetLibraryName:string;
    procedure MyAssignInifile;

    procedure SetCredentialsToSQLConnector;

  public
    procedure Initialize;
    procedure ImportData(sImportFile: string='');
    procedure OpenConnection;
    procedure CloseConnection;
    function ChangeConnection(sDBSection:string):boolean;

    {lists the available connection types depending on the SQL units included in the uses section;
    if bCreateText is false you will get the the possible connector Types in case you will choose them and switch between;
    possible Connector Types are stored in the property psPossibleConnectorTypes}
    function ListConnectionTypes(bCreateText:boolean=true):string;

    function IsConnectorTypeValid(sConnectorType:string):boolean;
    procedure showcredentials;

    {returns the current Version of the connected database}
    function GetDatabaseVersion:string;

    {In case you need an additional TSQLQuery}
    function GetQuery : TSQLQuery;

    //Ini File Stuff
    procedure ReadStandardSectionFromIni;
    function ReadDefaultCredentialsFromIni: TDBCredentials;
    procedure RefreshCredentials(sSection: string);
    function GetCredentialsFromIniSection(sSection: string): TDBCredentials;
    function GetDBconnectionsFromIni: string;

    property psPossibleConnectorTypes:string read FsConnectorType;
    property psPossibleDatabaseConnections:string read FsPossibleDatabaseConnections;
    property psConnectorType:string read FsConnectorType write SetConnectorType;
    property pbDeleteDatabasefileBeforeOpen:boolean read FDeleteDatabasefileBeforeOpen write SetbDeleteDatabasefileBeforeOpen;
    property pbImportDemoDataOnCreate:boolean read FImportDemoDataOnCreate write SetbImportDemoDataOnCreate;
    property psIniFileName:String read FsIniFileName write SetsIniFileName;
    property pbAllowOpenDialogOnSQLImportFile:boolean read FbAllowOpenDialogOnSQLImportFile write SetbAllowOpenDialogOnSQLImportFile;
    property prCredentials:TDBCredentials read FrCredentials;
    property psConnectMessage:string read FsConnectMessage;
  end;

var
  dmsqldb: Tdmsqldb;
  function GetDefaultCredentials(FsConnectorType:string):TDBCredentials;
  function CheckDataModuleAssigned:boolean;
  procedure OpenCredentialManager(bIsDatabaseChooseMode:boolean=false);

implementation

uses
  dialogs ,uguessfile, fcredentials;

//This function tries to support you while finding standard access credentials
function GetDefaultCredentials(FsConnectorType: string): TDBCredentials;
begin

  result.server:='';
  result.username:='';
  result.password:='';
  result.DatabaseName:='';
  result.CustomLibraryName:='';

  case FsConnectorType of

  'Firebird': begin
      result.server:='localhost';
      result.username:='SYSDBA';
      result.password:='masterkey';
      result.DatabaseName:='data.fdb';
  end;

  'Interbase': begin
      result.server:='localhost';
      result.username:='SYSDBA';
      result.password:='masterkey';
      result.DatabaseName:='data.fdb';
  end;

  'MSSQLServer':
    begin
    result.username:='sa';
    end;

  'MySQL 4.0','MySQL 4.1','MySQL 5.0','MySQL 5.1','MySQL 5.5','MySQL 5.6','MySQL 5.7','MySQL 8.0':
    begin
      result.server:='localhost';
      result.username:='username';
      result.password:='password';
      result.DatabaseName:='test';
    end;

  'ODBC':
    begin
    end;

  'Oracle':
    begin
    end;

  'PostgreSQL':
    begin
    end;

  'SQLite3':
    begin
    result.DatabaseName:=cSQLDatabaseFile;
    end;

  'Sybase':
    begin
    result.server:='localhost';
    result.username:='SYSDBA';
    result.password:='masterkey';
    result.DatabaseName:='data.fdb';
    end;


  else ;
    result.server:='localhost';
    result.username:='username';
    result.password:='password';
    result.DatabaseName:='databasename';
  end;

end; //GetDefaultCredentials

function CheckDataModuleAssigned: boolean;
begin
  result:=false;
  //Check if the datamodule exists before using it in case
  //someone forgot to to create it in time or had
  //put it to the first place
  //in the project options Auto-Create forms list
  //or had created it by sourcecode
  if not assigned(dmsqldb) then
    raise Exception.Create( LineEnding
                          + 'File: ' + {$INCLUDE %FILE%} + LineEnding
                          + 'Methodname: ' +  {$I %CURRENTROUTINE%} + LineEnding
                          + 'Linenumber: ' + {$INCLUDE %LINE%} + LineEnding +LineEnding
                          + 'The data module has not yet been created. Please note that the data module must be created'+ LineEnding
                          + ' before any database related action. The recommended way is to create the datamodul by code in'+ LineEnding
                          + ' the OnActivate Event like this: '+ LineEnding
                          + LineEnding
                          + ' if not assigned(dmsqldb) then dmsqldb:=Tdmsqldb.Create(self)'+ LineEnding
                          + LineEnding
                          + ' Otherwise use the autocreate function of forms in the project options to create the'+ LineEnding
                          + ' udmsqldb unit as the first one. Check the order of creation in the project options (Ctrl+Shift+F11)'
                          + ' under position forms '
                          + LineEnding
                          )
  else
    result:=true;
end;

procedure OpenCredentialManager(bIsDatabaseChooseMode: boolean);
var
  myForm:TfrmCredentials;
begin
  myForm:=TfrmCredentials.Create(nil);
  try
    MyForm.pbIsDatabaseChooseMode:=bIsDatabaseChooseMode;
    myForm.ShowModal;

    if assigned(dmsqldb) then begin
      dmsqldb.MyAssignInifile;  //reread INI File in case something has changed
      dmsqldb.ReadStandardSectionFromIni; //Get Settings from possibly changed INIfile
    end;

  finally
    myForm.Free;
  end;
end;


{$R *.lfm}


{ Tdmsqldb }

procedure Tdmsqldb.DataModuleCreate(Sender: TObject);
begin
  FsPossibleConnectorTypes:=ListConnectionTypes(false);
  FsConnectorType:= cConnectorType;
  FDeleteDatabasefileBeforeOpen:=true;
  FImportDemoDataOnCreate:=true;
  FsConnectMessage:='Database offline';

  FsIniFileName:=GuessDBIniFile;

  MyAssignInifile;
  ReadStandardSectionFromIni; //Get Settings from INIfile

  if FbChooseDatabaseOnStartup then begin
    OpenCredentialManager;
    MyAssignInifile;  //reread INI File in case something has changed
    ReadStandardSectionFromIni; //Get Settings from possibly changed INIfile
  end;

  FrCredentials:=ReadDefaultCredentialsFromIni;

  //Emergency Exit for unsupported Connector Types, further messaging done in function
  if not IsConnectorTypeValid(FrCredentials.ConnectorType) then  exit;

  Initialize;
  OpenConnection;
  if FImportDemoDataOnCreate then ImportData;
end;

procedure Tdmsqldb.DataModuleDestroy(Sender: TObject);
begin
  freeandnil(FIniFile);
end;


procedure Tdmsqldb.Initialize;
var
  sLibrary:string;
begin
  CloseConnection;                                    //in case SQLDBLibraryLoader1 or SQLConnector1 were active in design mode close them

  //First
  sLibrary := MyGetLibraryName;
  if sLibrary = cUNDEF then exit;                     //for security reasons in case the exception fails

  //Second: load library
  MyLoadLib(FrCredentials.ConnectorType,sLibrary);

  //Third:
  // Try to find the Database file for file based systems or set credentials for others.
  // If database file for file based systems do not exist create the named database file automatically

  //distinguish file databases versus server based databases
  case FrCredentials.ConnectorType of

  'SQLite3': begin
    FrCredentials.DatabaseName := GuessSQLDatabaseFile(FDeleteDatabasefileBeforeOpen);
    end;

  'Firebird': begin
    FrCredentials.DatabaseName := GuessSQLDatabaseFile(FDeleteDatabasefileBeforeOpen);
    SQLConnector1.Params.Add('PAGE_SIZE=16384'); //enough space for indexes etc
    end;

  'MySQL 4.0','MySQL 4.1','MySQL 5.0','MySQL 5.1','MySQL 5.5','MySQL 5.6','MySQL 5.7','MySQL 8.0':
      begin
        //SQLConnector1.Params.Add('Port=3306'); //standard port if needed
      end;

   else
    //
  end;

  SetCredentialsToSQLConnector;

  SQLConnector1.CharSet:='UTF8';
  SQLConnector1.LoginPrompt := False;
  SQLConnector1.KeepConnection := False;
  SQLConnector1.Options:=[scoApplyUpdatesChecksRowsAffected] ;
  //SQLConnector1.SkipLibraryVersionCheck:=true; //Caution !!!!
  //SQLConnector1

  //+[sqImplicitTransaction];
  {
    TConnOption = (sqSupportParams, sqSupportEmptyDatabaseName, sqEscapeSlash, sqEscapeRepeat, sqImplicitTransaction, sqLastInsertID, sqSupportReturning,sqSequences);
  TConnOptions= set of TConnOption;


  Both sqoKeepOpenOnCommit and CommitRetaining will keep the result set open but they are not the same for transaction handling.

  Using sqoKeepOpenOnCommit with Commit will keep the result set and close the transaction.

  Using CommitRetaining will keep the result set and open a new transaction. I do not recommend using CommitRetaining unless you have a good reason for keeping an open transaction.

  }

  {$ifdef UseSQLLOG}
  SQLConnector1.OnLog:=@DoSQLLog;
  SQLConnector1.LogEvents:=LogAllEventsExtra;  //either LogAllEvents or LogAllEventsExtra
  {$endif}

end;

procedure Tdmsqldb.ImportData(sImportFile: string);
var
  TransactionWasStarted: boolean;

begin
  //store transaction state
  TransactionWasStarted:=SQLTransaction1.Active;
  if not TransactionWasStarted then SQLTransaction1.StartTransaction;

  if SQLQuery1.Active then SQLQuery1.Close;

  if sImportfile = '' then begin
     sImportFile := GuessSQLImportFile(FsSQLCustomImportFileName,FbAllowOpenDialogOnSQLImportFile);
     if sImportFile = cUNDEF then begin //just in case
        showmessage( 'SQL import file not found');
        exit;
     end
  end;
  if not FileExists(sImportFile) then begin
     showmessage('ImportData: File ' + sImportFile +'does not exist');
     exit;
  end;
  try
    SQLScript1.Script.LoadFromFile(sImportFile);
    SQLScript1.ExecuteScript;     //oder.Execute???
    SQLTransaction1.Commit;
  finally
    //
  end;
  // Make sure we leave the transaction state as we found it
  if TransactionWasStarted then SQLTransaction1.StartTransaction;
end;


procedure Tdmsqldb.OpenConnection;
begin
  if not SQLConnector1.Connected then
  begin
    try
      SQLConnector1.Open;
      FsConnectMessage:='Connection established to '+FrCredentials.section+', Database ready'
    except
      On E : Exception do
        begin
          showmessage( LineEnding
                      + 'File: ' + {$INCLUDE %FILE%} + LineEnding
                      + 'Methodname: ' +  {$I %CURRENTROUTINE%} + LineEnding
                      + 'Linenumber: ' + {$INCLUDE %LINE%} + LineEnding +LineEnding
                      + 'Connection to Database failed ('+FrCredentials.section+')'+  LineEnding+ LineEnding
                      + E.Message);
          Exit;
        end;
    end;
    if not SQLConnector1.Connected then begin
      FsConnectMessage:='Could not establish connection to '+FrCredentials.section+'-- closing connection, Database offline';
      showmessage(FsConnectMessage);
      SQLConnector1.Close;
    end;
  end;
end;

procedure Tdmsqldb.CloseConnection;
begin
    if SQLConnector1.Connected then SQLConnector1.Close;
    FsConnectMessage:='Connection closed, Database offline';
end;

function Tdmsqldb.ChangeConnection(sDBSection: string): boolean;
begin
  result:=false;
  RefreshCredentials(sDBSection);
  Initialize;
  OpenConnection;
  If dmsqldb.SQLConnector1.Connected then begin
    FsConnectMessage:='Connection established to '+sDBSection+', Database ready';
    result:=true;
  end
  else
    FsConnectMessage:='Could not establish connection to '+sDBSection+', Database offline';

  showmessage(FsConnectMessage);
end;

function Tdmsqldb.ListConnectionTypes(bCreateText: boolean): string;
Var
  S : TStringList;
  I : Integer;

begin
  S:=TStringList.Create;
  result:='';
  try
    getConnectionList(S);
    if s.Count=0 then showmessage('No connections available');

    if bCreateText then begin

      result:=LineEnding +'Available connection types:';
      For I:=0 to S.Count-1 do begin
        result:= result
                 + LineEnding
                 + S[i]
                 + ', Default library name: ' + GetConnectionDef(S[i]).DefaultLibraryName
                 + ', ConnectorType: ' + GetConnectionDef(S[i]).TypeName
                 ;
      end;
    end
    else
    begin
      For I:=0 to S.Count-1 do begin
        if i>0 then result:=result+';';
        result:=result+GetConnectionDef(S[i]).TypeName;
      end;
    end;

  finally
    S.free;
  end;
end;

function Tdmsqldb.IsConnectorTypeValid(sConnectorType: string): boolean;
begin
  result:=true;
  if (sConnectorType='') or (pos(sConnectorType, sConnectorType)=0) then
  begin
    result:=false;
    raise Exception.Create('The requested Connector Type >'+sConnectorType
                +'< is either empty or not available yet.'+ LineEnding+ LineEnding
                +'Please activate the appropriate connectors in the uses clause udmsqldb.pas at line 28ff');
  end;
end;

//fro debug Purposes;
procedure Tdmsqldb.showcredentials;
var
  s:string;
begin
  s:='Credentialrecord:';
  s:=s+LineEnding+'.HostName :'+ FrCredentials.server;
  s:=s+LineEnding+'.Port     :'+ inttostr(FrCredentials.port);
  s:=s+LineEnding+'.User     :'+ FrCredentials.username;
  s:=s+LineEnding+'.Password :'+ FrCredentials.password;
  s:=s+LineEnding+'.ConnectorType :'+ FrCredentials.ConnectorType;
  s:=s+LineEnding+'.Database :'+ FrCredentials.Databasename;
  s:=s+LineEnding+'.CustomLibraryName :'+ FrCredentials.CustomLibraryName;
  s:=s+LineEnding;
  s:=s+LineEnding+'Connection:';
  s:=s+LineEnding+'.HostName :'+ SQLConnector1.HostName;
  s:=s+LineEnding+'.User     :'+ SQLConnector1.UserName;
  s:=s+LineEnding+'.Password :'+ SQLConnector1.Password;
  s:=s+LineEnding+'.ConnectorType :'+ SQLConnector1.ConnectorType;
  s:=s+LineEnding+'.Database :'+ SQLConnector1.Databasename;
  s:=s+LineEnding+'.Connected  :'+ BoolToStr(SQLConnector1.Connected)+'  '+BoolToStr(SQLConnector1.Connected,true);
  s:=s+LineEnding;
  s:=s+LineEnding+'LibraryLoader:';
  s:=s+LineEnding+'.ConnectionType :'+ SQLDBLibraryLoader1.ConnectionType;
  s:=s+LineEnding+'.LibraryName :'+ SQLDBLibraryLoader1.LibraryName;

  MessageDlg(s, mtInformation, [mbOK], 0);
end;

function Tdmsqldb.GetStandardLibraryName(sConnector: string): string;
var
  S : TStringList;
  I : Integer;
begin
  result:=cUNDEF;
  S:=TStringList.Create;
  try
    getConnectionList(S);
    For I:=0 to S.Count-1 do
      if S[i] = sConnector then begin
        result := GetConnectionDef(S[i]).DefaultLibraryName;
        exit;
      end;
  finally
    S.free;
  end;

end;

procedure Tdmsqldb.MyLoadLib(sConnectionType, sLibname: String);
begin
  //Using Loadlibrary and unloadlibrary instead of enabling switch to catch exceptions
  try
    SQLDBLibraryLoader1.UnLoadLibrary;
  except
    On E : Exception do
      begin
        showmessage( LineEnding
                          + 'File: ' + {$INCLUDE %FILE%} + LineEnding
                          + 'Methodname: ' +  {$I %CURRENTROUTINE%} + LineEnding
                          + 'Linenumber: ' + {$INCLUDE %LINE%} + LineEnding +LineEnding
                          + 'Error unloading library : '+ SQLDBLibraryLoader1.LibraryName
                          + LineEnding + LineEnding
                          + E.Message);

        Exit;
      end;
  end;

  SQLDBLibraryLoader1.ConnectionType:=sConnectionType; //define connector Type
  SQLDBLibraryLoader1.LibraryName:=sLibname;
  try
    SQLDBLibraryLoader1.LoadLibrary;
  except
    On E : Exception do
      begin
        showmessage( LineEnding
                          + 'File: ' + {$INCLUDE %FILE%} + LineEnding
                          + 'Methodname: ' +  {$I %CURRENTROUTINE%} + LineEnding
                          + 'Linenumber: ' + {$INCLUDE %LINE%} + LineEnding +LineEnding
                          + 'Error loading library : '+ sLibName
                          + LineEnding + LineEnding
                          + E.Message);

        Exit;
      end;
  end;

end;

function Tdmsqldb.MyGetLibraryName: string;
var
  sStdLibName:string;
begin

  if FrCredentials.CustomLibraryName ='' then
    begin
      sStdLibName:='';
      sStdLibName:= GetStandardLibraryName(FsConnectorType);
      if sStdLibName=cUNDEF then
        showmessage('Could not find a standard library name for connectortype '+FsConnectorType+ LineEnding + LineEnding
                    +'it is suspected that in the data module udmsqldb.pas the necessary package was not activated/uncommented'+ LineEnding+ LineEnding
                    +'see -> udmsqldb.pas line 27 following'
                    );

    end;

  result:=cUNDEF;

  {$IFDEF UNIX}
    {$ifdef RasPi}
      result := '????';
    {$else}
      {$ifdef DARWIN}

        if FrCredentials.CustomLibraryName ='' then
          result:='/usr/lib/'+sStdLibName
        else
          result:='/usr/lib/'+FrCredentials.CustomLibraryName;

      {$else}

        if FrCredentials.CustomLibraryName ='' then
          result:=sStdLibName
        else
          result:=FrCredentials.CustomLibraryName;
      {$endif}
    {$endif}
  {$ELSE}       //windows OS

    if FrCredentials.CustomLibraryName ='' then begin
      result :='';
      result := GuessLibraryLocation(sStdLibName);
    end
    else
      result := GuessLibraryLocation(FrCredentials.CustomLibraryName);

  {$ENDIF}
end;

procedure Tdmsqldb.MyAssignInifile;
begin
  if assigned(FIniFile) then freeandnil(FIniFile);
  FIniFile:= TMemIniFile.Create(FsIniFileName,[ifoWriteStringBoolean]);
  FiniFile.SetBoolStringValues(true,['true','1']);
  FiniFile.SetBoolStringValues(false,['false','0']);
end;

procedure Tdmsqldb.SetCredentialsToSQLConnector;
begin
  SQLConnector1.ConnectorType:=FrCredentials.ConnectorType;
  SQLConnector1.DatabaseName:=FrCredentials.DatabaseName;
  SQLConnector1.UserName:=FrCredentials.username;
  SQLConnector1.Password:=FrCredentials.password;
  SQLConnector1.Hostname:=FrCredentials.server;
end;



function Tdmsqldb.GetDatabaseVersion: string;
var
  SQLQ:TSQLQuery;
  sSQL:string;
begin
  result:='No version info available';
  sSQL:='';

  case FsConnectorType of

  'Firebird':
    sSQL:='SELECT rdb$get_context(' + QuotedStr('SYSTEM') + ', ' + QuotedStr('ENGINE_VERSION') + ') as version from rdb$database)';

  'Interbase': begin
  end;

  'MSSQLServer','Sybase':
    sSQL:='SELECT @@VERSION as Version';

  'MySQL 4.0','MySQL 4.1','MySQL 5.0','MySQL 5.1','MySQL 5.5','MySQL 5.6','MySQL 5.7','MySQL 8.0':
    sSQL:='SELECT @@VERSION AS Version';

  //'ODBC':
  //  begin
  //  end;

  'Oracle':
    sSQL:='SELECT banner as version FROM v$version WHERE rownum=1';

  'PostgreSQL': sSQL:='SELECT version() AS version';

  'SQLite3':
    sSQL:='SELECT sqlite_version() AS version';

  end;

  if sSQL<>'' then begin
    SQLQ:=TSQLQuery.Create(nil);
    try
      SQLQ.DataBase := SQLConnector1;
      SQLQ.SQL.Text:=sSQL;
      SQLQ.Open;
      SQLQ.First;
      if SQLQ.RecordCount > 0 then
        result:= SQLQ.FieldByName('version').AsString;
      SQLQ.Close;
    finally
      freeandnil(SQLQ);
    end;
  end;

end;

function Tdmsqldb.GetQuery: TSQLQuery;
var
  AQuery : TSQLQuery;
begin
  AQuery := TSQLQuery.Create(nil);
  AQuery.Database := dmsqldb.SQLConnector1;
  AQuery.Transaction := dmsqldb.SQLTransaction1;
  Result := AQuery;
end;

procedure Tdmsqldb.ReadStandardSectionFromIni;
begin
  FImportDemoDataOnCreate:=      FIniFile.ReadBool('Standard','ImportDemoDataOnCreate',true);
  FDeleteDatabasefileBeforeOpen:=FIniFile.ReadBool('Standard','DeleteDatabasefileBeforeOpen',true);
  FbAllowOpenDialogOnSQLImportFile:=FIniFile.ReadBool('Standard','AllowOpenDialogOnSQLImportFile',false);
  FbChooseDatabaseOnStartup:=FIniFile.ReadBool('Standard','ChooseDatabaseOnStartup',false);
end;

function Tdmsqldb.ReadDefaultCredentialsFromIni: TDBCredentials;
var
   sDefaultSectionName:string;
begin
   sDefaultSectionName:=FIniFile.ReadString('Standard','startdbfromsection','SQLITE_demodb');
   result:= GetCredentialsFromIniSection(sDefaultSectionName);
end;

procedure Tdmsqldb.RefreshCredentials(sSection: string);
begin
  FrCredentials:=GetCredentialsFromIniSection(sSection);
end;

function Tdmsqldb.GetCredentialsFromIniSection(sSection: string): TDBCredentials;
var
   rStdCredentials:TDBCredentials;
   sMyConnectorType:string;
begin
  sMyConnectorType:=FIniFile.ReadString(sSection,'ConnectorType',cConnectorType);
  rStdCredentials:=GetDefaultCredentials(sMyConnectorType);
  IsConnectorTypeValid(sMyConnectorType); //Connector Types are valid if they are activated in uses clause

  result.Server:=FIniFile.ReadString(sSection,'Server',rStdCredentials.server);
  result.username:=FIniFile.ReadString(sSection,'Username',rStdCredentials.username);
  result.password:=FIniFile.ReadString(sSection,'Password',rStdCredentials.password);
  result.DatabaseName:=FIniFile.ReadString(sSection,'DatabaseName',rStdCredentials.DatabaseName);
  result.ConnectorType:=FIniFile.ReadString(sSection,'ConnectorType',rStdCredentials.ConnectorType);
  result.customlibraryname:=FIniFile.ReadString(sSection,'customlibraryname','');
  result.caption:=FIniFile.ReadString(sSection,'caption','no caption');
  result.section:=sSection;
  result.WasConnectionSuccess:=FIniFile.ReadString(sSection,'WasConnectionSuccess','1');

  FsConnectorType:=result.ConnectorType;
end;

function Tdmsqldb.GetDBconnectionsFromIni: string;
var
  sSection:string;
  slSections:TStringlist;
  i:integer;

  iCountDBSections:integer;
begin
  result:='';
  iCountDBSections:=0;

  slSections:=TStringlist.Create;
  try
    FIniFile.ReadSections(slSections);
    for i:=0 to slSections.Count -1 do
    begin
      sSection:=slSections.Strings[i];
      if sSection='Standard' then continue;

      if result ='' then
        result := sSection
      else
        result := result+';'+sSection;

      inc(iCountDBSections);
    end;

    if iCountDBSections<1 then
      raise Exception.Create('File: ' + {$INCLUDE %FILE%} + LineEnding +
                             'Methodname: ' +  {$I %CURRENTROUTINE%} + LineEnding +
                             'Linenumber: ' + {$INCLUDE %LINE%} + LineEnding + LineEnding+
                             'No valid Database connection section in configdb.ini');
  finally
    freeandnil(slSections);
  end;

end;

//*********************** Setter
procedure Tdmsqldb.SetsIniFileName(AValue: String);
begin
  if FsIniFileName=AValue then Exit;
  FsIniFileName:=AValue;
end;

procedure Tdmsqldb.SetbDeleteDatabasefileBeforeOpen(AValue: boolean);
begin
  if FDeleteDatabasefileBeforeOpen=AValue then Exit;
  FDeleteDatabasefileBeforeOpen:=AValue;
end;

procedure Tdmsqldb.SetbAllowOpenDialogOnSQLImportFile(AValue: boolean);
begin
  if FbAllowOpenDialogOnSQLImportFile=AValue then Exit;
  FbAllowOpenDialogOnSQLImportFile:=AValue;
end;

procedure Tdmsqldb.SetbImportDemoDataOnCreate(AValue: boolean);
begin
  if FImportDemoDataOnCreate=AValue then Exit;
  FImportDemoDataOnCreate:=AValue;
end;

procedure Tdmsqldb.SetConnectorType(AValue: string);
begin
  if FsConnectorType=AValue then Exit;
  FsConnectorType:=AValue;
end;

end.

