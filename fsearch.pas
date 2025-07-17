unit fsearch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, Forms, Controls, Graphics, Dialogs, DBGrids,
  ExtCtrls;

type

  { TFormSearch }

  TFormSearch = class(TForm)
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DataSource3: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DBGrid3: TDBGrid;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SQLQuery1: TSQLQuery;
    SQLQuery2: TSQLQuery;
    SQLQuery3: TSQLQuery;
  private

  public
    procedure GetData;
  end;

var
  FormSearch: TFormSearch;

implementation

uses
  dmmain;

{$R *.lfm}

{ TFormSearch }



procedure TFormSearch.GetData;
begin
  SQLQuery1.database:=dmsqldb.SQLite3Connection1;
  SQLQuery2.database:=dmsqldb.SQLite3Connection1;
  SQLQuery3.database:=dmsqldb.SQLite3Connection1;
  SQLQuery1.SQL.Text:='SELECT * FROM Album';
  SQLQuery2.SQL.Text:='SELECT * FROM Artist';
  SQLQuery3.SQL.Text:='SELECT album.title AS title,artist.name as artist FROM Album JOIN artist ON album.artistid=artist.artistid';
  SQLQuery1.Open;
  SQLQuery2.Open;
  SQLQuery3.Open;
end;

end.

