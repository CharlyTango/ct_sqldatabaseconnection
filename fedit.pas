unit fedit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, Forms, Controls, Graphics, Dialogs, DBCtrls;

type

  { TFormEdit }

  TFormEdit = class(TForm)
    DataSource1: TDataSource;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    SQLQuery1: TSQLQuery;
  private

  public

  end;

var
  FormEdit: TFormEdit;

implementation

{$R *.lfm}

end.

