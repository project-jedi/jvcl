unit BmpAnimMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, JvBmpAnim,
  ExtCtrls, Menus, ComCtrls, StdCtrls, ImgList, JvComponent;

type
  TBmpAnimMainForm = class(TForm)
    Edit1: TEdit;
    UpDown1: TUpDown;
    OnOff: TButton;
    BmpAnimator1: TJvBmpAnimator;
    Edit2: TEdit;
    UpDown2: TUpDown;
    Label1: TLabel;
    Label2: TLabel;
    ImageList1: TImageList;
    procedure OnOffClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
  end;

var
  BmpAnimMainForm: TBmpAnimMainForm;

implementation

{$R *.DFM}

procedure TBmpAnimMainForm.OnOffClick(Sender: TObject);
begin
  with BmpAnimator1 do
  begin
    Active := not Active;
    if not Active then Position := 0;
  end;
end;

procedure TBmpAnimMainForm.Edit1Change(Sender: TObject);
begin
  if not BmpAnimator1.Active then
     BmpAnimator1.Position := UpDown1.Position;
end;

procedure TBmpAnimMainForm.Edit2Change(Sender: TObject);
begin
  try
    BmpAnimator1.Speed := StrToInt(Edit2.Text);
  except
    BmpAnimator1.Speed := 15;
end;

end;

end.
