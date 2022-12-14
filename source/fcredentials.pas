unit fcredentials;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, DB, memds, Forms, Controls, Graphics, Dialogs,
  DBGrids, StdCtrls, DBCtrls, ActnList, ComCtrls, Menus, inifiles, Grids;

const
  cEDIT=2;
  cBROWSE=1;
  cTOGGLE=0;

type

  { TfrmCredentials }

  TfrmCredentials = class(TForm)
    actn_ShowIniContent: TAction;
    actn_SaveAllToIni: TAction;
    actn_SaveCurrentToIni: TAction;
    actn_GetStandardCredentials: TAction;
    actn_PrepareAsExample: TAction;
    actn_WriteStandardSectionToINI: TAction;
    actn_ReadStandardSectionToControls: TAction;
    actn_MakeConnectionDefault: TAction;
    actn_TestConnection: TAction;
    actn_ReadAllFromIni: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    bttnSave: TButton;
    bttnMakeDefault: TButton;
    bttnGetStandardCredentials: TButton;
    bttnSaveAll: TButton;
    bttnTest: TButton;
    Button2: TButton;
    Button3: TButton;
    cbImportDemoDataOnCreate: TCheckBox;
    cbDeleteDatabasefileBeforeOpen: TCheckBox;
    cbChooseDatabaseOnStartup: TCheckBox;
    cbAllowOpenDialogOnSQLImportFile: TCheckBox;
    DataSource1: TDataSource;
    cbConnectionTypes: TDBComboBox;
    DBNavigator1: TDBNavigator;
    edDefaultConnection: TEdit;
    eSection: TDBEdit;
    eServer: TDBEdit;
    eDatabasename: TDBEdit;
    eUsername: TDBEdit;
    ePassword: TDBEdit;
    eCustomLibraryName: TDBEdit;
    eCaption: TDBEdit;
    DBGrid1: TDBGrid;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    mDescription: TDBMemo;
    eSQLCustomImportFileName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MenuItem1: TMenuItem;
    PageControl1: TPageControl;
    PopupMenu1: TPopupMenu;
    tsPrepareExample: TTabSheet;
    tsConnections: TTabSheet;
    tsConfiguration: TTabSheet;
    TConnections: TMemDataset;

    procedure actn_GetStandardCredentialsExecute(Sender: TObject);
    procedure actn_MakeConnectionDefaultExecute(Sender: TObject);
    procedure actn_ReadAllFromIniExecute(Sender: TObject);
    procedure actn_ReadStandardSectionToControlsExecute(Sender: TObject);
    procedure actn_SaveAllToIniExecute(Sender: TObject);
    procedure actn_SaveCurrentToIniExecute(Sender: TObject);
    procedure actn_ShowIniContentExecute(Sender: TObject);
    procedure actn_TestConnectionExecute(Sender: TObject);
    procedure actn_WriteStandardSectionToINIExecute(Sender: TObject);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid1PrepareCanvas(sender: TObject; DataCol: Integer;
      Column: TColumn; AState: TGridDrawState);
    procedure DBNavigator1Click(Sender: TObject; Button: TDBNavButtonType);
    procedure FormCreate(Sender: TObject);
    procedure TConnectionsBeforeDelete(DataSet: TDataSet);
  private
    FbIsDatabaseChooseMode: boolean;
    FsIniFileName:string;
    FIniFile:TMemIniFile;
    function ReadIniToTConnections: boolean;
    procedure ReadStandardSectionToControls;
    procedure SetIsDatabaseChooseMode(AValue: boolean);
    procedure WriteStandardSectionToINI;
    procedure WriteStandardSection;
    procedure WriteCredentials(sSection:string);
    procedure TestConnection;
    procedure ToggleControls(iHow:integer=0);
    procedure GetStandardCredentials;
    procedure MakeConnectionDefault;
    procedure SaveAllToIni;
    procedure MyAssignInifile;

    procedure WriteIni;

    procedure showinicontents;
  public
    procedure WriteCredentialsToIni(sSection:string);
    procedure DeleteCredentialFromIni(sSection:string);
    property pbIsDatabaseChooseMode:boolean read FbIsDatabaseChooseMode write SetIsDatabaseChooseMode;
  end;

var
  frmCredentials: TfrmCredentials;

implementation

{$R *.lfm}

uses
  uguessfile, udmsqldb;

{ TfrmCredentials }

procedure TfrmCredentials.FormCreate(Sender: TObject);
begin
  FbIsDatabaseChooseMode:=false;
  ToggleControls(cBROWSE);
  PageControl1.ActivePage:=tsConnections;

  if assigned(dmsqldb) then
    FsIniFileName:=dmsqldb.psIniFileName
  else
    FsIniFileName:=cDBIniFileName;  //in case the CredentialsManager is called before dmsqldb is created

  MyAssignInifile;

  TConnections.Open;

  cbConnectionTypes.Items.AddDelimitedtext(dmsqldb.ListConnectionTypes(false),';',true);

  ReadStandardSectionToControls;
  ReadIniToTConnections;
  TConnections.First;
end;

procedure TfrmCredentials.DBNavigator1Click(Sender: TObject;
  Button: TDBNavButtonType);
begin
  case Button of
  nbFirst :    begin
    end;
  nbPrior :    begin
    end;
  nbLast :    begin
    end;
  nbInsert :    begin
    ToggleControls(cEDIT);
    end;
  nbEdit :    begin
    ToggleControls(cEDIT);
    end;
  nbPost :    begin
    ToggleControls(cBROWSE);
  end;
  nbCancel :    begin
    ToggleControls(cBROWSE);
    end;
  nbDelete :    begin
    ToggleControls(cBROWSE);
    //showmessage(esection.text);
    //DeleteCredentialFromIni(eSection.text);
    //ReadIniToTConnections;
    end;
  nbRefresh:    begin
    ToggleControls(cBROWSE);
    ReadIniToTConnections;
    end;
  end; //case
end;

procedure TfrmCredentials.TestConnection;
begin
  dmsqldb.ChangeConnection(eSection.Text);

  if dmsqldb.SQLConnector1.Connected then begin
    TConnections.Edit;
    TConnections.FieldByName('WasConnectionSuccess').AsString:='2';
    TConnections.Post;
  end
  else begin
    TConnections.Edit;
    TConnections.FieldByName('WasConnectionSuccess').AsString:='0';
    TConnections.Post;
  end;
end;

procedure TfrmCredentials.ToggleControls(iHow: integer);
begin
  case iHow of
    0: begin   //Toggle mode
      cbConnectionTypes.Enabled:=not cbConnectionTypes.Enabled;
      eSection.Enabled:=not eSection.Enabled;
      eServer.Enabled:=not eServer.Enabled;
      eDatabasename.Enabled:=not eDatabasename.Enabled;
      eUsername.Enabled:= not eUsername.Enabled;
      ePassword.Enabled:=not ePassword.Enabled;
      eCustomLibraryName.Enabled:=not eCustomLibraryName.Enabled;
      eCaption.Enabled:=not eCaption.Enabled;
      mDescription.Enabled:=not mDescription.Enabled;

      actn_GetStandardCredentials.Enabled:=not actn_GetStandardCredentials.Enabled;
      actn_MakeConnectionDefault.Enabled:=not actn_MakeConnectionDefault.Enabled;
      actn_TestConnection.Enabled:=not actn_TestConnection.Enabled;
      actn_ReadAllFromIni.Enabled:=not actn_ReadAllFromIni.Enabled;

      actn_SaveAllToIni.Enabled:=not actn_SaveAllToIni.Enabled;
      actn_SaveCurrentToIni.Enabled:=not actn_SaveCurrentToIni.Enabled;
   end ;
   1 : begin   //Browse Mode
     cbConnectionTypes.Enabled:=false;
     eSection.Enabled:=false;
     eServer.Enabled:=false;
     eDatabasename.Enabled:=false;
     eUsername.Enabled:= false;
     ePassword.Enabled:=false;
     eCustomLibraryName.Enabled:=false;
     eCaption.Enabled:=false;
     mDescription.Enabled:=false;

     actn_GetStandardCredentials.Enabled:=false;
     actn_MakeConnectionDefault.Enabled:=true;
     actn_TestConnection.Enabled:=true;
     actn_ReadAllFromIni.Enabled:=true;
     actn_SaveAllToIni.Enabled:=true;
     actn_SaveCurrentToIni.Enabled:=true;
   end ;

   2 : begin //Edit Mode
     cbConnectionTypes.Enabled:=true;
     eSection.Enabled:=true;
     eServer.Enabled:=true;
     eDatabasename.Enabled:=true;
     eUsername.Enabled:= true;
     ePassword.Enabled:=true;
     eCustomLibraryName.Enabled:=true;
     eCaption.Enabled:=true;
     mDescription.Enabled:=true;

     actn_GetStandardCredentials.Enabled:=true;
     actn_MakeConnectionDefault.Enabled:=false;
     actn_TestConnection.Enabled:=false;
     actn_ReadAllFromIni.Enabled:=false;
     actn_SaveAllToIni.Enabled:=false;
     actn_SaveCurrentToIni.Enabled:=false;
   end ;

  else
  //
  end;

end;

procedure TfrmCredentials.GetStandardCredentials;
var
   stdcred:TDBCredentials;
begin
  stdcred:=GetDefaultCredentials(cbConnectionTypes.Text);
  eServer.Text:=stdcred.Server;
  eDatabasename.Text:=stdcred.DatabaseName;
  eUsername.Text:=stdcred.Username;
  ePassword.Text:=stdcred.Password;
  eCustomLibraryName.Text:=stdcred.CustomLibraryName;
  eCaption.Text:=stdcred.caption;
end;

procedure TfrmCredentials.MakeConnectionDefault;
begin
   FIniFile.WriteString('Standard','startdbfromsection',TConnections.FieldByName('section').AsString);
   edDefaultConnection.Text:=TConnections.FieldByName('section').AsString;
   WriteIni;
end;

procedure TfrmCredentials.SaveAllToIni;
begin
  TConnections.DisableControls;
  TConnections.First;

  while not TConnections.EOF do begin
    WriteCredentials(TConnections.FieldByName('section').AsString);
    TConnections.Next;
  end;
  WriteStandardSection;
  WriteIni;

  //showinicontents;

  TConnections.EnableControls;
  Application.ProcessMessages;
  WriteStandardSectionToINI;
end;

procedure TfrmCredentials.MyAssignInifile;
begin
  if assigned(FIniFile) then freeandnil(FIniFile);
  FIniFile:= TMemIniFile.Create(FsIniFileName,[ifoWriteStringBoolean]);
  FiniFile.SetBoolStringValues(true,['true','1']);
  FiniFile.SetBoolStringValues(false,['false','0']);

  {
ifoStripComments
    Strip comments from file
ifoStripInvalid
    Strip invalid lines from file
ifoEscapeLineFeeds
    Observe backslash as linefeed escape character
ifoCaseSensitive
    Key and section names are case sensitive
ifoStripQuotes
    Strip double quotes from values
ifoFormatSettingsActive
    Observe the values in FormatSettings
ifoWriteStringBoolean
    Read/Write booleans as strings instead of 0/1
    }

end;

procedure TfrmCredentials.WriteIni;
begin
  try
    FIniFile.UpdateFile;
  except
    on E:Exception do begin
      showmessage('Ini-file seems to be blocked by some reason. Please try it again'+LineEnding+LineEnding+E.Message)
    end;
  end;
end;

procedure TfrmCredentials.showinicontents;
var
  sl:TStringlist;
begin
  sl:=TStringlist.Create;
  try
    FIniFile.GetStrings(sl);
    showmessage(sl.Text);
  finally
    freeandnil(sl)
  end;

end;

function TfrmCredentials.ReadIniToTConnections: boolean;
var
  slSections:TStringlist;
  i,j:integer;
  s,sSection,sConnectortype:string;
  stdCredentials:TDBCredentials;
begin
  result:=false;
  slSections:=TStringlist.Create;

  TConnections.Clear(false);  //Clear data only, leave definitions as is
  TConnections.DisableControls;

  try
    FIniFile.ReadSections(slSections);
    for i:=0 to slSections.Count -1 do
    begin
      sSection:=slSections.Strings[i];

      //Section [Standard] contains no database related settings
      if sSection='Standard' then continue;

      sConnectortype:=FIniFile.ReadString(sSection,'connectortype','');
      //if sConnectortype='' then showmessage('You must provide a Connector Type');
      stdCredentials:=GetDefaultCredentials(sConnectortype);

      If not TConnections.Active then TConnections.Open;
      TConnections.Append;
      TConnections.FieldByName('section').AsString:=sSection;
      TConnections.FieldByName('server').AsString:=FIniFile.ReadString(sSection,'server',stdCredentials.server);
      TConnections.FieldByName('databasename').AsString:=FIniFile.ReadString(sSection,'databasename',stdCredentials.DatabaseName);
      TConnections.FieldByName('username').AsString:=FIniFile.ReadString(sSection,'username',stdCredentials.Username);
      TConnections.FieldByName('password').AsString:=FIniFile.ReadString(sSection,'password',stdCredentials.Password);
      TConnections.FieldByName('connectortype').AsString:=sConnectortype;

      s:=FIniFile.ReadString(sSection,'caption','');
      if s='' then s:= sSection;
      TConnections.FieldByName('caption').AsString:=s;
      TConnections.FieldByName('description').AsString:=FIniFile.ReadString(sSection,'description','');

      s:= FIniFile.ReadString(sSection,'order','');
      if s='' then j:=i+1;
      TConnections.FieldByName('order').AsInteger:= j;

      TConnections.FieldByName('CustomLibraryName').AsString:=FIniFile.ReadString(sSection,'CustomLibraryName','');
      TConnections.FieldByName('WasConnectionSuccess').AsString:=FIniFile.ReadString(sSection,'WasConnectionSuccess','1');

      TConnections.Post;
    end;  //for
    TConnections.First;
    result:=true;
  finally
    freeandnil(slSections);
    TConnections.EnableControls;
  end;

end;

procedure TfrmCredentials.ReadStandardSectionToControls;
begin
  cbImportDemoDataOnCreate.Checked:=FIniFile.ReadBool('Standard','ImportDemoDataOnCreate',true);
  cbDeleteDatabasefileBeforeOpen.Checked:=FIniFile.ReadBool('Standard','DeleteDatabasefileBeforeOpen',true);
  cbChooseDatabaseOnStartup.Checked:=FIniFile.ReadBool('Standard','ChooseDatabaseOnStartup',false);
  cbAllowOpenDialogOnSQLImportFile.Checked:=FIniFile.ReadBool('Standard','AllowOpenDialogOnSQLImportFile',false);
  eSQLCustomImportFileName.Text:=FIniFile.ReadString('Standard','SQLCustomImportFileName','');
  edDefaultConnection.Text:=FIniFile.ReadString('Standard','startdbfromsection','');
end;


procedure TfrmCredentials.WriteStandardSection;
begin
  FIniFile.WriteBool('Standard','ImportDemoDataOnCreate',cbImportDemoDataOnCreate.Checked);
  FIniFile.WriteBool('Standard','DeleteDatabasefileBeforeOpen',cbDeleteDatabasefileBeforeOpen.Checked);
  FIniFile.WriteBool('Standard','ChooseDatabaseOnStartup',cbChooseDatabaseOnStartup.Checked);
  FIniFile.WriteBool('Standard','AllowOpenDialogOnSQLImportFile',cbAllowOpenDialogOnSQLImportFile.Checked);
  FIniFile.WriteString('Standard','SQLCustomImportFileName',eSQLCustomImportFileName.Text);
end;

procedure TfrmCredentials.WriteCredentials(sSection: string);
begin
  FIniFile.Writestring(sSection,'Server',TConnections.FieldByName('server').AsString);
  FIniFile.Writestring(sSection,'Username',TConnections.FieldByName('username').AsString);
  FIniFile.Writestring(sSection,'Password',TConnections.FieldByName('password').AsString);
  FIniFile.Writestring(sSection,'DatabaseName',TConnections.FieldByName('databasename').AsString);
  FIniFile.Writestring(sSection,'ConnectorType',TConnections.FieldByName('connectortype').AsString);
  FIniFile.Writestring(sSection,'CustomLibraryName',TConnections.FieldByName('customlibraryname').AsString);
  FIniFile.Writestring(sSection,'Caption',TConnections.FieldByName('caption').AsString);
  FIniFile.Writestring(sSection,'Description',TConnections.FieldByName('description').AsString);
  FIniFile.Writestring(sSection,'WasConnectionSuccess',TConnections.FieldByName('WasConnectionSuccess').AsString);
end;

procedure TfrmCredentials.SetIsDatabaseChooseMode(AValue: boolean);
begin
  if FbIsDatabaseChooseMode=AValue then Exit;
  FbIsDatabaseChooseMode:=AValue;
  actn_MakeConnectionDefault.Caption:='Select Connection';
  tsPrepareExample.TabVisible:=false;
  Application.ProcessMessages;
end;

procedure TfrmCredentials.WriteStandardSectionToINI;
begin
   MyAssignIniFile;
   WriteStandardSection;
   WriteIni;
   Application.ProcessMessages;
end;

procedure TfrmCredentials.WriteCredentialsToIni(sSection: string);
begin
  WriteCredentials(sSection);
  WriteIni;
end;

procedure TfrmCredentials.DeleteCredentialFromIni(sSection: string);
begin
  if (TConnections.State in [dsEdit, dsInsert]) then TConnections.Post;

  FIniFile.EraseSection(sSection);
  WriteIni;
end;

//*********** Setter
procedure TfrmCredentials.TConnectionsBeforeDelete(DataSet: TDataSet);
begin
    showmessage(esection.text);  //TODO
    DeleteCredentialFromIni(eSection.text);
    ReadIniToTConnections;
end;

//*********** Actions
procedure TfrmCredentials.actn_TestConnectionExecute(Sender: TObject);
begin
  TestConnection;
end;

procedure TfrmCredentials.actn_WriteStandardSectionToINIExecute(Sender: TObject
  );
begin
  WriteStandardSectionToINI;
end;

procedure TfrmCredentials.DBGrid1DrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin

end;

procedure TfrmCredentials.DBGrid1PrepareCanvas(sender: TObject;
  DataCol: Integer; Column: TColumn; AState: TGridDrawState);
var
  s:string;
begin
  with Sender as TDBGrid do
  begin
    if DataCol in [0, 1] then begin
      s:=TConnections.FieldByName('WasConnectionSuccess').AsString;
      case s of
        '0': begin
          Canvas.Brush.Color := $005469FC;
          Canvas.Font.Color := clDefault;
        end;
        '1': begin
        end;
        '2': begin
          Canvas.Brush.Color := $0089E882;
          Canvas.Font.Color := clDefault;
        end
      else
      end;

    if (gdSelected in AState) and Focused then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clDefault;
    end;
    end;//DataCol
  end;  //with
end;


procedure TfrmCredentials.actn_ReadAllFromIniExecute(Sender: TObject);
begin
  ReadIniToTConnections;
end;

procedure TfrmCredentials.actn_ReadStandardSectionToControlsExecute(
  Sender: TObject);
begin
  ReadStandardSectionToControls;
end;

procedure TfrmCredentials.actn_SaveAllToIniExecute(Sender: TObject);
begin
  SaveAllToIni;
end;

procedure TfrmCredentials.actn_SaveCurrentToIniExecute(Sender: TObject);
begin
  WriteCredentialsToIni(eSection.text);
end;

procedure TfrmCredentials.actn_ShowIniContentExecute(Sender: TObject);
begin
  showinicontents;
end;

procedure TfrmCredentials.actn_MakeConnectionDefaultExecute(Sender: TObject);
begin
  MakeConnectionDefault;
  if FbIsDatabaseChooseMode then close;
end;

procedure TfrmCredentials.actn_GetStandardCredentialsExecute(Sender: TObject);
begin
  GetStandardCredentials;
end;

end.

