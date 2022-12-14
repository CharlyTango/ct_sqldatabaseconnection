{License:  GNU LESSER GENERAL PUBLIC LICENSE Version 2.1, February 1999
 copyright CharlyTango 2022
                       }

unit uguessfile;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, forms, dialogs{$ifdef WINDOWS},WinDirs{$endif};

const
  {$IFDEF UNIX}
    {$ifdef RasPi}
      cSQLiteLibraryName = '????';
    {$else}
      {$ifdef DARWIN}
         cSQLiteLibraryName='/usr/lib/libsqlite3.dylib';
      {$else}
         cSQLiteLibraryName = 'libsqlite3.so';
      {$endif}
    {$endif}
  {$ELSE}
    cSQLiteLibraryName = 'sqlite3.dll';        //name of standard SQLite access library in case other sources fail
  {$ENDIF}

  cDBIniFileName             ='configdb.ini';  //standard name for the INI file
  cSQLStandardImportFileName ='import.sql';    //standard name for the SQL import file
  cSQLDatabaseFile           ='data.db';       //standard name for SQLite database file
  cDelimiter                 ='@';             //used standard Delimiter for several strings
  cUNDEF                      ='undefined';    //standard value of undefined results

function GuessFile(const sFileName: string; const sWhereToSearch: string;
  var sSearched: string): string;      //tries to find a file in application directory and other directories
function GuessLibraryLocation(sLibraryName:string='';sConnectorType:string=''):string; //tries to find the SQL access library
function GuessSQLImportFile(sSQLCustomImportFileName:string;bAllowOpenDialog:boolean=false):string;    //tries to find the SQL import file
function GuessSQLDatabaseFile(bDeleteOldFile:boolean=false):string;  //tries to find the database file for SQLITE etc
function GuessDBIniFile(sIniFileName:string=''):string;  //tries to find the applications database INI file or creates a new one
function GetTargetCPUinfo:string; //determines whether the Target CPU is x86 or 64bit

implementation

function GuessFile(const sFileName: string; const sWhereToSearch: string;
  var sSearched: string): string;
var
  s:string;
  sl:TStringlist;
  i: integer;
begin
  result:=cUNDEF;

  s :=  Application.Location + sFileName;
  if FileExists(s) then
  begin
   result := s;
   exit;
  end;
  sSearched:=sSearched + s + LineEnding;

  sl := TStringList.Create;
  try
    sl.Delimiter:=cDelimiter;
    sl.DelimitedText := sWhereToSearch;
    for i := 0 to sl.Count - 1 do begin
      s :=  Application.Location + sl[i]+ DirectorySeparator + sFileName;
      if FileExists(s) then
      begin
       result := s;
       break;
      end;
      sSearched:=sSearched + s + LineEnding;
    end;
  finally
    sl.Free;
  end;

end;

function GuessLibraryLocation(sLibraryName: string; sConnectorType: string
  ): string;
var
  s: string;
  sSearched:string;
  sPathString:string;
  sLibName:string;
begin
  result:=cUNDEF;
  sSearched:='';

  if sLibraryName='' then
    sLibName:=cSQLiteLibraryName
  else
    sLibName:=sLibraryName;

  {$ifdef Unix}
  sPathString:='';//no directories necessary for Linux but must be installed
  {$else}
    //Search libraries with bitness
    if GetTargetCPUinfo = 'i386' then
       sPathString := 'dlltest@dllx86'
    else
       sPathString := 'dll@sql';
  {$endif}

  result:=GuessFile(sLibName,sPathString,sSearched);

  //Search in default directories
  {$if Defined(WINDOWS)}
  if result=cUNDEF  then begin
    if GetTargetCPUinfo = 'i386' then
        result :=  GetWindowsSpecialDir(CSIDL_WINDOWS,false) + 'sysWOW64'+ DirectorySeparator  + sLibName
    else
       result :=  GetWindowsSpecialDir(CSIDL_SYSTEM,false)  + sLibName;

  end;
  sSearched:=sSearched+ LineEnding+result;
  {$elseif Defined(Linux)}
    //regrettably i use Windows only
  {$elseif Defined(Darwin)}

  {$endif}

  if not FileExists(result) then  result := cUNDEF;

  if result=cUNDEF  then begin
    s:=  LineEnding
        + 'File: ' + {$INCLUDE %FILE%} + LineEnding
        + 'Methodname: ' +  {$I %CURRENTROUTINE%} + LineEnding
        + 'Linenumber: ' + {$INCLUDE %LINE%} + LineEnding +LineEnding
        + 'tried to find Library ' + sLibName + ' in '
        + LineEnding + LineEnding
        + sSearched
        + LineEnding + LineEnding+ 'but it was not found!' + LineEnding+ LineEnding
        + 'Please provide ' + sLibName +' in one of the searchpaths'+ LineEnding;



    case sConnectorType of

        'Firebird': begin

        end;

        'Interbase': begin

        end;

        'MSSQLServer':
          begin
            s:=s + 'The missing file may be downloaded at https://www.freetds.org/ or https://www.microsoft.com/en-us/sql-server/sql-server-downloads'
          end;

        'MySQL 4.0','MySQL 4.1','MySQL 5.0','MySQL 5.1','MySQL 5.5','MySQL 5.6','MySQL 5.7','MySQL 8.0':
          begin
            s:=s + 'The missing file may be downloaded at https://www.mysql.com/  or https://mariadb.org/'
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
            s:=s + 'The missing file may be downloaded at https://www.sqlite.org/download.html'
                {$ifdef Unix}
                + LineEnding + LineEnding
                + 'Possibly the following might help also:'
                + 'sudo apt-get install sqlite3 libsqlite3-dev'
                {$endif} ;
          end;

        'Sybase':
          begin

          end;


        else ;
           //do nothing more
        end;

    raise Exception.Create(s);
   end;
end;

{
The default import file or a file defined by the developer is searched and used.
If the user is allowed to select an import file set bAllowOpenDialog to true;
}
function GuessSQLImportFile(sSQLCustomImportFileName: string;
  bAllowOpenDialog: boolean): string;
var
  s:string;
  sSearchedin:string;
  sPathString:string;
  OpenDialog:TOpendialog;
begin
  result:=cUNDEF;
  sSearchedin:='';

  {$ifdef Unix}
  sPathString:='to define';
  {$else}
  sPathString := 'sql@data';
  {$endif}

  result:=GuessFile(sSQLCustomImportFileName,sPathString,sSearchedin);
  if result=cUNDEF  then
    result:=GuessFile(cSQLStandardImportFileName,sPathString,sSearchedin);

  if (result=cUNDEF) and (bAllowOpenDialog) then begin
    OpenDialog := TOpendialog.Create(nil);
    OpenDialog.DefaultExt:='*.sql';
    try
      if OpenDialog.Execute then
        result := OpenDialog.Filename
      else
        result:=cUNDEF;
    finally
      freeandnil(OpenDialog);
    end;
  end;

  if result=cUNDEF  then  begin

  //raise Exception.Create( LineEnding
  //                        + 'File: ' + {$INCLUDE %FILE%} + LineEnding
  //                        + 'Methodname: ' +  {$I %CURRENTROUTINE%} + LineEnding
  //                        + 'Linenumber: ' + {$INCLUDE %LINE%} + LineEnding +LineEnding
  //                        + 'tried to find SQL import File '
  //                        +s);
    if sSQLCustomImportFileName='' then begin
      s:= cSQLStandardImportFileName + ' in '
      + LineEnding + LineEnding
      + sSearchedin
      + 'but it was not found!' + LineEnding+ LineEnding
      + 'Please provide ' + cSQLStandardImportFileName +' in one of the searchpaths';

    end
    else begin
      s:= 'either' + sSQLCustomImportFileName + ' or ' + cSQLStandardImportFileName + ' in '
      + LineEnding + LineEnding
      + sSearchedin
      + 'but it was not found!' + LineEnding+ LineEnding
      + 'Please provide ' + 'either' + sSQLCustomImportFileName + ' or ' + cSQLStandardImportFileName +' in one of the searchpaths';
    end;


    showmessage( LineEnding
                          + 'File: ' + {$INCLUDE %FILE%} + LineEnding
                          + 'Methodname: ' +  {$I %CURRENTROUTINE%} + LineEnding
                          + 'Linenumber: ' + {$INCLUDE %LINE%} + LineEnding +LineEnding
                          + 'tried to find SQL import File '
                          +s);

  end;
end;

function GuessSQLDatabaseFile(bDeleteOldFile: boolean): string;
var
  sSearchedin:string;
  sPathString:string;
begin
  result:=cUNDEF;
  sSearchedin:='';

  {$ifdef Unix}
  sPathString:='to define';
  {$else}
  sPathString := 'data@sql';
  {$endif}

  result:=GuessFile(cSQLDatabaseFile,sPathString,sSearchedin);

  if result=cUNDEF  then begin
    showmessage( LineEnding
                + 'File: ' + {$INCLUDE %FILE%} + LineEnding
                + 'Methodname: ' +  {$I %CURRENTROUTINE%} + LineEnding
                + 'Linenumber: ' + {$INCLUDE %LINE%} + LineEnding +LineEnding
                + 'tried to find SQL data File in'
                + LineEnding + LineEnding
                + sSearchedin
                + 'but it was not found!' + LineEnding+ LineEnding
                + 'Please provide '  + cSQLDatabaseFile +' in one of the searchpaths'+ LineEnding+ LineEnding
                +' creating standard database file '+cSQLDatabaseFile+' in application directory instead');

    result:=cSQLDatabaseFile;
  end;

  if bDeleteOldFile then begin
    if FileExists(result) then begin
      DeleteFile(result);
      //if you want extra information uncomment next line.
      //showmessage('old database file was deleted and will be created as fresh new file');
    end;
  end;

end;

function GuessDBIniFile(sIniFileName: string): string;
var
  sSearched:string;
  sPathString:string;
  sFileName:string;
  sl:TStringlist;
begin
  result:=cUNDEF;
  sSearched:='';

  if sIniFileName='' then
    sFileName:=cDBIniFileName
  else
    sFileName:=sIniFileName;

  {$ifdef Unix}
  sPathString:='';//no directories unser Linux necessary
  {$else}
  sPathString := 'config@sql';
  {$endif}

  result:=GuessFile(sFileName,sPathString,sSearched);

  if result=cUNDEF  then begin
    showmessage( LineEnding
                + 'File: ' + {$INCLUDE %FILE%} + LineEnding
                + 'Methodname: ' +  {$I %CURRENTROUTINE%} + LineEnding
                + 'Linenumber: ' + {$INCLUDE %LINE%} + LineEnding +LineEnding
                + 'tried to find the database configuration file in'
                + LineEnding + LineEnding
                + sSearched
                + 'but it was not found!' + LineEnding+ LineEnding
                + 'Please provide '  + cDBIniFileName +' in one of the searchpaths'+ LineEnding+ LineEnding
                +' creating the database configuration file '+cDBIniFileName+' in application directory instead');



    sl:=TStringlist.Create;
    try
      sl.Append('[Standard]');
      sl.Append(';which Database should be started');
      sl.Append('startdbfromsection=SQLITE_demodb');
      sl.Append('');

      sl.Append('; shall the database be chosen at startup? true/false)');
      sl.Append('; -->standard is false  (True=1 False=0)');
      sl.Append('ChooseDatabaseOnStartup=false');
      sl.Append('');

      sl.Append('; the standard import file name for SQL data and commands ist standard.sql');
      sl.Append('; if you want to use another file, its name goes here');
      sl.Append('; -->standard is false  (True=1 False=0)');
      sl.Append('SQLCustomImportFileName=false');
      sl.Append('');

      sl.Append('; if neither the Standard import file (import.sql) nor the defined SQLCustomImportFileName');
      sl.Append('; is found an Open Dialog could be used to find the impoer file manually');
      sl.Append('; -->standard is false  (True=1 False=0)');
      sl.Append('AllowOpenDialogOnSQLImportFile=false');
      sl.Append('');

      sl.Append('; delete the databasefile before opening the database connection so every');
      sl.Append('; start a new and empty database file is created');
      sl.Append('; works for file based databases like SQLite/Firebird/Interbase only');
      sl.Append('; a similar result can be achieved by using delete commands in the SQL import file');
      sl.Append('; -->standard is true  (True=1 False=0)');
      sl.Append('DeleteDatabasefileBeforeOpen=true');
      sl.Append('');

      sl.Append('; Import Data automatically on datamodules create event');
      sl.Append('; You also can use the procedure ImportData to import data at any given time');
      sl.Append('; -->standard is true  (True=1 False=0)');
      sl.Append('ImportDemoDataOnCreate=true');
      sl.Append('');

      sl.Append('[SQLITE_demodb]');
      sl.Append(';Comment');
      sl.Append('Caption=SQLITE Demo database');
      sl.Append('Description=SQLite connection to local ' + cSQLDatabaseFile);
      sl.Append('Databasename=' + cSQLDatabaseFile);
      sl.Append('Server=');
      sl.Append('Username=');
      sl.Append('Password=');
      sl.Append('ConnectorType=SQLite3');
      sl.Append(';if you want to use a custom access library their name goes here');
      sl.Append(';otherwise standard access libraries are used');
      sl.Append('CustomLibraryName=');
      sl.Append('order=0');
      sl.Append(';identifies successful connections. 0=connection failed 1=connection untested  2=connection successful');
      sl.Append('WasConnectionSuccess=');

      sl.SaveToFile(cDBIniFileName);
      result:=cDBIniFileName;
    finally
      freeandnil(sl)
    end;

    result:=cDBIniFileName;
  end;

end;

function GetTargetCPUinfo: string;
begin
  result := {$I %FPCTARGETCPU%};
end;

end.

