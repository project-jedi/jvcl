unit JvBmpAnimatorU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, StdCtrls, ComCtrls, JvComponent, JvBmpAnim, ExtCtrls,
  JvCaptionPanel;

type
  TJvBmpAnimatorFrm = class(TFrame)
    JvCaptionPanel1: TJvCaptionPanel;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    UpDown1: TUpDown;
    OnOff: TButton;
    Edit2: TEdit;
    UpDown2: TUpDown;
    BmpAnimator1: TJvBmpAnimator;
    ImageList1: TImageList;
    procedure OnOffClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TJvBmpAnimatorFrm.OnOffClick(Sender: TObject);
begin
  with BmpAnimator1 do
  begin
    Active := not Active;
    if not Active then Position := 0;
  end;
end;

procedure TJvBmpAnimatorFrm.Edit1Change(Sender: TObject);
begin
  if not BmpAnimator1.Active then
     BmpAnimator1.Position := UpDown1.Position;
end;

procedure TJvBmpAnimatorFrm.Edit2Change(Sender: TObject);
begin
  try
    BmpAnimator1.Speed := StrToInt(Edit2.Text);
  except
    BmpAnimator1.Speed := 15;
  end;
end;

end.
