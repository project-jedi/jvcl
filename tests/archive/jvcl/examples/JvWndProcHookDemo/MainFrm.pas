{ This demo shows that the new RegisterWndProcHook method in JvWndProcHook can
  handle several components subclassing the same control witout losing track of the
  original WndProc regardless of the order of registration/unregistration
  It also shows that the WndProc link(s) are unaffected by RecreateWnd
  (which destroys the old window handle)
}
unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvCaptionButton, StdCtrls;

type
  TForm1 = class(TForm)
    btnAdd: TButton;
    btnDelete: TButton;
    btnRecreateWnd: TButton;
    lbButtons: TListBox;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnRecreateWndClick(Sender: TObject);
  private
    { Private declarations }
    FButtonCount: integer;
    procedure DoButtonClick(Sender: TObject);
    function UniqueName(const BaseName: string): string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.DoButtonClick(Sender: TObject);
begin
  Caption := Format('Button "%s" clicked', [(Sender as TJvCaptionButton).Caption]);
end;

function TForm1.UniqueName(const BaseName: string): string;
var i: integer;
begin
  i := 1;
  Result := BaseName + IntToStr(i);
  while FindComponent(Result) <> nil do
  begin
    Inc(i);
    Result := BaseName + IntToStr(i);
    if i > 2000 then
      raise Exception.Create('Unable to create unique name!');
  end;
end;

procedure TForm1.btnAddClick(Sender: TObject);
var b: TJvCaptionButton;
begin
  B := TJvCaptionButton.Create(self);
  B.OnClick := DoButtonClick;
  B.Caption := Char(Ord(FButtonCount) + Ord('A'));

  B.Name := UniqueName('JvCaptionButton');
  B.ButtonLeft := B.ButtonLeft + FButtonCount * B.ButtonWidth + 2;
  lbButtons.Items.AddObject(B.Caption,B);
  Inc(FButtonCount);
end;

procedure TForm1.btnDeleteClick(Sender: TObject);
var i: integer;
begin
  i := lbButtons.ItemIndex;
  if (i > -1) then
  begin
    TJvCaptionButton(lbButtons.Items.Objects[i]).Free;
    lbButtons.Items.Delete(i);
  end;
  if i >= lbButtons.Items.Count then
    Dec(i);
  lbButtons.ItemIndex := i;
  if lbButtons.Items.Count = 0 then
    FButtonCount := 0;

end;

procedure TForm1.btnRecreateWndClick(Sender: TObject);
begin
  RecreateWnd;
end;

end.

