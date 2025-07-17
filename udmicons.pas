unit udmIcons;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TdmIcons }

  TdmIcons = class(TDataModule)
    ImageList1: TImageList;
  private

  public

  end;

var
  dmIcons: TdmIcons;

implementation

{$R *.lfm}

end.

