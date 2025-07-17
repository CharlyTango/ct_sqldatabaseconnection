unit fmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, Forms, Controls, Graphics, Dialogs, DBGrids,
  ExtCtrls, ComCtrls, Menus, ActnList;

type

  { Tform_main }

  Tform_main = class(TForm)
    actn_maintain_customers: TAction;
    actn_maintain_artists: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PageControl1: TPageControl;
    tsSearch: TTabSheet;
    procedure actn_maintain_artistsExecute(Sender: TObject);
    procedure actn_maintain_customersExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private

  public

  end;

var
  form_main: Tform_main;

implementation

uses
  dmmain,fsearch,fmaintainartists, udmicons, fcustomer;

{$R *.lfm}

{ Tform_main }

procedure Tform_main.FormActivate(Sender: TObject);
var
  fsearch:TFormSearch;
begin
  {create the database module here by code at a certain time
  to avoid autocreate which may bring some troubles.
  Despite the use of a global variable, the main window takes care of the
  destruction of the object by assigning the owner to it}

  if not assigned(dmsqldb) then dmsqldb := Tdmsqldb.Create(self);

  if not dmsqldb.InitDataModule then exit; //Initialize Datamodule

  {open Database Connection while telling from where it was called from}
  dmsqldb.OpenConnection({$I calledfrom.inc});

  {Create the datamodule containing the application icons for later use the same way}
  if not assigned(dmIcons) then dmIcons := TdmIcons.Create(self);

  {Here, the second form is glued into tsSearch in order to create possibilities
   for the use of several windows in the main without messing the mainform too much.
   No need to destroy the window  at terminanting the application because the main
   window takes care about that}
  fsearch:=TFormSearch.Create(self);
  fsearch.BorderStyle:=bsNone;
  fsearch.Parent:=tsSearch;
  fsearch.Align:=alclient;
  fSearch.GetData;
  fsearch.Show;
end;

procedure Tform_main.actn_maintain_artistsExecute(Sender: TObject);
var
  fmaint:TFormMaintainArtists;
begin
  fmaint:=TFormMaintainArtists.Create(self);
  fmaint.ShowModal;
  { If some Artist data has changed you possibly have to refresh Artist related Queries on currently open windows
    but thats quite another story}

end;

procedure Tform_main.actn_maintain_customersExecute(Sender: TObject);
var
  myform:TfrmCustomer;
begin
  myform:=TfrmCustomer.Create(self);
  myform.ShowModal;
end;

end.

