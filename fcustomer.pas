unit fcustomer;
{ First example of manipulating data of the table CUSTOMERS
Just a TDBGrid, a TDBNavigator und some TDBEdit controls-
Nothing else.
No beautifying the grid or doing anything with the logic of the TDBNavigator.
Just rock bottom to see how it works basically.

Using the TDBNavigator you can add, edit and delete records.
There is no check of referential integrity, so deleting records will damage your SQLite database

Everything is done by Lazarus - no additional code except the tree lines in the FormCreate event

The field "email" is mandatory which is defined in the database. If you don't write an email in the field you will get an error message when saving the record.
But this message is triggered by the SQLite database whil validating the input.

}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, Forms, Controls, Graphics, Dialogs, DBGrids,
  ExtCtrls, DBCtrls, StdCtrls;

type

  { TfrmCustomer }

  TfrmCustomer = class(TForm)
    DataSource1: TDataSource;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    SQLQuery1: TSQLQuery;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmCustomer: TfrmCustomer;

implementation

uses dmmain;
{$R *.lfm}

{ TfrmCustomer }

procedure TfrmCustomer.FormCreate(Sender: TObject);
begin
  SQLQuery1.database:=dmsqldb.SQLite3Connection1;
  SQLQuery1.SQL.Text:=
    ' SELECT '
  + ' CustomerId,FirstName,LastName,Company,Address,City,'
  + ' State,Country,PostalCode,Phone,Fax,Email '
  + ' FROM Customer';

  SQLQuery1.Open;
end;

end.

