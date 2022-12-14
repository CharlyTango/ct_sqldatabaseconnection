unit frame_example1;

{ this frame is meant as an example of how to use the framework.
similar to this frame you can also use your complete main form to embed it
into the framework with very little effort.
the framework (basically the data module) takes care of the database connection.
On the individual frames or forms exist the necessary SQLQuery components including
the associated datasource components. The database connection is assigned to the
SQLQuery components either at design time in the Object Inspector or at runtime in the code.
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls,
  DBCtrls, DBGrids, ActnList, EditBtn;

type

  { TFrameExample1 }

  TFrameExample1 = class(TFrame)
    actn_ConnectDB: TAction;
    actn_ImportData: TAction;
    actn_QueryTable: TAction;
    actn_DisconnectFromDB: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    DataSource1: TDataSource;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBEdit4: TDBEdit;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MemoLog: TMemo;
    PageControl1: TPageControl;
    pnlLeft: TPanel;
    pnlBottom: TPanel;
    pnlNav: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SQLQuery1: TSQLQuery;
    tabsheetConnection: TTabSheet;
    TabSheet2: TTabSheet;
    procedure actn_ConnectDBExecute(Sender: TObject);
    procedure actn_DisconnectFromDBExecute(Sender: TObject);
    procedure actn_ImportDataExecute(Sender: TObject);
    procedure actn_QueryTableExecute(Sender: TObject);
  private
    procedure MyOpenQuery;
    procedure MyImportData;
    procedure MyConnectDB;
    procedure MyDisconnectDB;
  public
    procedure Initialize;
    procedure ToggleControls(iWhatToDo: integer);
  end;

implementation

{$R *.lfm}

uses
  udmsqldb;

{ TFrameExample1 }

procedure TFrameExample1.MyOpenQuery;
begin
  if SQLQuery1.Active then SQLQuery1.Close;
  SQLQuery1.DataBase:=dmsqldb.SQLConnector1;  // either assign the database connection by code or Lazarus-GUI

  SQLQuery1.SQL.Text := 'SELECT CustomerId,FirstName,LastName,Company,Address,City,State,Country,PostalCode,Phone,Fax,Email FROM Customer';
  SQLQuery1.Open;
end;

procedure TFrameExample1.MyImportData;
begin
  MemoLog.Append('Importing data, please wait...');
  if FileNameEdit1.text='' then
    dmsqldb.ImportData
  else
    dmsqldb.ImportData(FileNameEdit1.text);

  MemoLog.Append(LineEnding+'dmsqldb: Import was done');
end;

procedure TFrameExample1.MyConnectDB;
begin
  //Close connection as precaution because one connection may already be open and
  //single user databases like SQLite do not want concurrent connections
  dmsqldb.CloseConnection;

  MemoLog.Clear;
  MemoLog.Append('Connecting...');
  memoLog.Append(dmsqldb.ListConnectionTypes);
  MemoLog.Append(LineEnding+'For more connection possibilities see uses clause in udmsqldb unit!');

  if not assigned(dmsqldb) then exit;
  MemoLog.Append(LineEnding+'using Library:' + LineEnding + dmsqldb.SQLDBLibraryLoader1.LibraryName+LineEnding);

  dmsqldb.OpenConnection;

  if dmsqldb.SQLConnector1.Connected then begin
    memolog.Append(LineEnding+'Database Name:      '+dmsqldb.SQLConnector1.DatabaseName) ;
    memolog.Append('Database Connector: '+dmsqldb.psConnectorType);
    memolog.Append('Database Version:   '+dmsqldb.GetDatabaseVersion);
  end;

  memolog.Append(LineEnding+dmsqldb.psConnectMessage);

  ToggleControls(2);
end;

procedure TFrameExample1.MyDisconnectDB;
begin
  if not assigned(dmsqldb) then exit;
  dmsqldb.SQLConnector1.Connected:=false;
  if not dmsqldb.SQLConnector1.Connected then memolog.Append('Disconnected ') ;

  ToggleControls(1);
end;

procedure TFrameExample1.Initialize;
begin
  MemoLog.Clear;
  PageControl1.ActivePage:=tabsheetConnection;
  if dmsqldb.SQLConnector1.Connected then begin
    MemoLog.Append('The Connection to the database was established and Data was retrieved automatically.');
    MemoLog.Append('');
    MemoLog.Append('If You want to test the connection process yourselve use the numbered buttons in their sequence');
    MemoLog.Append('Scrolling the memofield may help in some cases');
    ToggleControls(1);
    MyOpenQuery;
  end;
end;

procedure TFrameExample1.ToggleControls(iWhatToDo: integer);
begin
  case iWhatToDo of
    0: begin
      //just toggle Controls
      actn_ConnectDB.Visible:=not actn_ConnectDB.Visible;
      actn_DisconnectFromDB.Visible:= not actn_DisconnectFromDB.Visible;
      actn_QueryTable.Visible:= not actn_QueryTable.Visible;
      actn_ImportData.Visible:=not actn_ImportData.Visible;
      DBGrid1.Visible:= not DBGrid1.Visible;
      DBNavigator1.Visible:= not DBNavigator1.Visible;
      MemoLog.Visible:= not MemoLog.Visible;
      pnlLeft.Visible:= not pnlLeft.Visible;
      Splitter1.Visible:= not Splitter1.Visible;
    end;
    1: begin
      actn_ConnectDB.Enabled:=true;
      actn_DisconnectFromDB.Enabled:=false;
      actn_QueryTable.Enabled:=false;
      actn_ImportData.Enabled:=false;
    end;
    2: begin
      actn_ConnectDB.Enabled:=false;
      actn_DisconnectFromDB.Enabled:=true;
      actn_QueryTable.Enabled:=true;
      actn_ImportData.Enabled:=true;
    end;
    3: begin
      MemoLog.Clear;
    end;

  end;
end;

procedure TFrameExample1.actn_DisconnectFromDBExecute(Sender: TObject);
begin
  MyDisconnectDB;
end;

procedure TFrameExample1.actn_ConnectDBExecute(Sender: TObject);
begin
  MyConnectDB;
end;

procedure TFrameExample1.actn_ImportDataExecute(Sender: TObject);
begin
  MyImportData;
end;

procedure TFrameExample1.actn_QueryTableExecute(Sender: TObject);
begin
  MyOpenQuery;
end;

end.

