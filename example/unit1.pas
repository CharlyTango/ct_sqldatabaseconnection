unit Unit1;

{$mode objfpc}{$H+}


{if you want some Example Code enable 'UseFrameExampleCode'
 by removing the period bevor the declaration.
 Then recompile.
 You also may therefore delete all code enclosed in this define brackets
 including the define statements without any damage to the core framework if
 you need a clean suface for your own application
 }

{$DEFINE UseFrameExampleCode}//<-- preferred example

{this switch influences whether a data module (udmsqldb.pas) is used for database
access or an access object (usqldbruntime.pas) which creates all components at runtime.

Default setting is udmsqldb.pas recommended is usqldbruntime.pas}

{$DEFINE USESQLDBGUI}

// {$ifdef USESQLDBGUI} {$else}  {$endif}


interface

uses
  Classes, SysUtils, DB, SQLDB, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBGrids, ExtCtrls, DBCtrls, ComCtrls, Buttons, Menus, ActnList
  {$ifdef USESQLDBGUI} , udmsqldb {$else} ,usqldbruntime {$endif}
  {$ifdef UseFrameExampleCode}, frame_example1{$endif}
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    actnCloseApplication: TAction;
    actnOpenCredentialManager: TAction;
    ActionList1: TActionList;
    bttnManageCredentials: TBitBtn;
    comboDBSelector: TComboBox;
    DataSource1: TDataSource;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    mnuChangeConnection: TMenuItem;
    SQLQuery1: TSQLQuery;
    StatusBar1: TStatusBar;

    procedure actnCloseApplicationExecute(Sender: TObject);
    procedure actnOpenCredentialManagerExecute(Sender: TObject);
    procedure comboDBSelectorChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);

  private
    {$ifdef UseFrameExampleCode}
    FrameExample: TFrameExample1;
    {$endif}
    procedure UpdateGUI;  //update GUI because something might have changed
    procedure ReactOnDatamoduleInfo(Sender: TObject; Info: string);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  {Use this as an emergency break if you autocreate the datamodule
  -->see the function description
  as we create the datamodule by code the function is commented }

  //CheckDataModuleAssigned;    //check if the datamodule already exists
end;

procedure TForm1.UpdateGUI;
begin
  {$ifdef UseFrameExampleCode}
  if assigned(FrameExample) then
  begin
    FrameExample.ToggleControls(1);
    FrameExample.ToggleControls(3);
  end;
  {$endif}

  with comboDBSelector do //fill the combo box with possible database connections
  begin
    Items.Clear;
    Items.AddDelimitedtext(dmsqldb.GetDBconnectionsFromIni, ';', True);
    Text := dmsqldb.prCredentials.section;
  end;

  dmsqldb.RenewMenuItems(mnuChangeConnection);       // Renew Change Menu
  StatusBar1.SimpleText := dmsqldb.psConnectMessage;   // Renew Status Bar Message

  bttnManageCredentials.SetFocus;
  Application.ProcessMessages;
end;

procedure TForm1.ReactOnDatamoduleInfo(Sender: TObject; Info: string);
begin
  {you can react to individual all or some information of the data module }
  //case Info of
  //  infoDatabaseChanged:;
  //  infoDatabaseConnected:;
  //  infoDatabaseCouldNotConnect:;
  //  infoDatabaseClosed:;
  //  infoDatabaseCannotClose:;
  //else
  //  //showmessage(Info);
  //end;

  UpdateGUI; //In case a database change occurs you have to react
end;

procedure TForm1.FormActivate(Sender: TObject);
var
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  StatusBar1.SimpleText := 'Please wait, database is created and prepared....';
  Screen.Cursor := crSQLWait;
  Application.ProcessMessages;

  {create the database module here by code at a certain time
  to avoid autocreate which may bring some troubles.
  Despite the use of a global variable, the main window takes care of the
  destruction of the object by assigning the owner to it}

  {$ifdef USESQLDBGUI}
  if not assigned(dmsqldb) then dmsqldb := Tdmsqldb.Create(self);
  {$else}
  if not assigned(dmsqldb) then dmsqldb := Tsqldbruntime.Create(self);
  {$endif}

  dmsqldb.OnInfo:=@ReactOnDatamoduleInfo;  //assign event to react on information from the datamodule

  try
    UpdateGUI;
    if dmsqldb.SQLConnector1.Connected then
    begin
       {The most easiest way to migrate existing applications to this framework
       is similar to the use of frames.
       Create your former main window by code and connect your queries to the new connection
       as shown in the following few lines. Connection related code should be removed from your form
       It does not matter if you do this with frames or full blown forms.
      }
      {$ifdef UseFrameExampleCode}
      dmsqldb.OpenConnection;
      if not assigned(FrameExample) then FrameExample := TFrameExample1.Create(self);
      FrameExample.Parent := self;
      FrameExample.Align := alClient;
      FrameExample.SQLQuery1.DataBase := dmsqldb.SQLConnector1;
      FrameExample.Initialize;
      {$endif}
    end;
  finally
    StatusBar1.SimpleText := dmsqldb.psConnectMessage;
    Screen.Cursor := OldCursor;
    Application.ProcessMessages;
  end;

  //Maybe your code goes here...

end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    {$ifdef UseFrameExampleCode}
  FreeAndNil(FrameExample);
  //just for security reason -- the main form should care about the frames destruction
    {$endif}
end;

//Example how to use a combo box as a database changer
procedure TForm1.comboDBSelectorChange(Sender: TObject);
begin
  dmsqldb.ChangeConnection(comboDBSelector.Text);
  UpdateGUI;  //In case a database change occurs you have to react
end;

procedure TForm1.actnOpenCredentialManagerExecute(Sender: TObject);
begin
  OpenCredentialManager;  //Example how to use the Credentials Manager
  UpdateGUI;              //In case a database change occurs you have to react
end;

procedure TForm1.actnCloseApplicationExecute(Sender: TObject);
begin
  Close;
end;


end.
