unit fmaintainartists;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, Forms, Controls, Graphics, Dialogs, DBGrids,
  DBCtrls;

type

  { TFormMaintainArtists }

  TFormMaintainArtists = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    SQLQuery1: TSQLQuery;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormMaintainArtists: TFormMaintainArtists;

implementation

uses
  dmmain;

{$R *.lfm}

{ TFormMaintainArtists }

procedure TFormMaintainArtists.FormCreate(Sender: TObject);
begin
  SQLQuery1.database:=dmsqldb.SQLite3Connection1;
  SQLQuery1.SQL.Text:='SELECT * FROM Album';
  SQLQuery1.Open;
end;

end.

