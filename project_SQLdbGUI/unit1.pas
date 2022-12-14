unit Unit1;

{$mode objfpc}{$H+}

{.$DEFINE UseFrameExampleCode}  //<-- preferred example

{if you want some Example Code enable either 'UseFrameExampleCode'
 by removing thee period bevor the declaration.
 Then recompile.
 You also may therefore delete all code enclosed in this define brackets
 including the define statements without any damage to the core framework if
 you need a clean suface for your own application
 }

interface

uses
  Classes, SysUtils, DB, SQLDB, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBGrids, ExtCtrls, DBCtrls, ComCtrls, Buttons
  ,udmsqldb {$ifdef UseFrameExampleCode},frame_example1{$endif};

type

  { TForm1 }

  TForm1 = class(TForm)
    bttnManageCredentials: TBitBtn;
    comboDBSelector: TComboBox;
    DataSource1: TDataSource;
    Label1: TLabel;
    SQLQuery1: TSQLQuery;
    StatusBar1: TStatusBar;

    procedure comboDBSelectorChange(Sender: TObject);
    procedure bttnManageCredentialsClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);

  private
    {$ifdef UseFrameExampleCode}
    FrameExample:TFrameExample1;
    {$endif}

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

procedure TForm1.FormActivate(Sender: TObject);
var
  OldCursor: TCursor;
begin
  OldCursor:=Screen.Cursor;
  StatusBar1.SimpleText:='Please wait, database is created and prepared....';
  Screen.Cursor := crSQLWait;
  Application.ProcessMessages;

  if not assigned(dmsqldb) then dmsqldb:=Tdmsqldb.Create(self);   //create the database module
  try
    with comboDBSelector do //fill the combo box with possible database connections
    begin
      Items.Clear;
      Items.AddDelimitedtext(dmsqldb.GetDBconnectionsFromIni,';',true);
      text:=dmsqldb.prCredentials.section;
    end;
    bttnManageCredentials.SetFocus;

    If dmsqldb.SQLConnector1.Connected then begin
       {The most easiest way to migrate existing applications to this framework
       is similar to the use of frames.
       Create your man window by code and connect your queries to the new connection
       as shown in the following few lines. Connection related code should be removed from your form
       It does not matter if you do this with frames or full blown forms.
      }
      {$ifdef UseFrameExampleCode}
        dmsqldb.OpenConnection;
        if not assigned(FrameExample) then FrameExample := TFrameExample1.Create(self);
        FrameExample.Parent := self;
        FrameExample.Align:=alClient;
        FrameExample.SQLQuery1.DataBase:=dmsqldb.SQLConnector1;
        FrameExample.Initialize;
      {$endif}
    end;
  finally
    StatusBar1.SimpleText:=dmsqldb.psConnectMessage;
    Screen.Cursor:=OldCursor;
    Application.ProcessMessages;
  end;

  //Maybe your code goes here...

end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    {$ifdef UseFrameExampleCode}
    freeandnil(FrameExample); //just for security reason -- the main form should care about the frames destruction
    {$endif}
end;

//Example how to use a database changer
procedure TForm1.comboDBSelectorChange(Sender: TObject);
begin
  dmsqldb.ChangeConnection(comboDBSelector.Text);
  //In case an database change occurs you have to react

  {$ifdef UseFrameExampleCode}
   if assigned(FrameExample) then begin
     FrameExample.ToggleControls(1);
     FrameExample.ToggleControls(3);
   end;
  {$endif}

end;

//Example how to use the Credentials Manager
procedure TForm1.bttnManageCredentialsClick(Sender: TObject);
begin
  OpenCredentialManager;
  StatusBar1.SimpleText:=dmsqldb.psConnectMessage;
end;

end.

