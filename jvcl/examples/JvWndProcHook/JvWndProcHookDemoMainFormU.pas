unit JvWndProcHookDemoMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  JvCaptionButton ;

type
  TJvWndProcHookDemoMainForm = class(TForm)
    btnAdd: TButton;
    btnDelete: TButton;
    btnRecreateWnd: TButton;
    lbButtons: TListBox;
    Label1: TLabel;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnRecreateWndClick(Sender: TObject);
  private
    { Private declarations }
    FButtonCount: integer;
    procedure DoButtonClick(Sender: TObject);
    function UniqueName(const BaseName: string): string;
   end;

var
  JvWndProcHookDemoMainForm: TJvWndProcHookDemoMainForm;

implementation

{$R *.dfm}

procedure TJvWndProcHookDemoMainForm.DoButtonClick(Sender: TObject);
begin
  Caption := Format('Button "%s" clicked', [(Sender as TJvCaptionButton).Caption]);
end;

function TJvWndProcHookDemoMainForm.UniqueName(const BaseName: string): string;
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

procedure TJvWndProcHookDemoMainForm.btnAddClick(Sender: TObject);
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

procedure TJvWndProcHookDemoMainForm.btnDeleteClick(Sender: TObject);
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

procedure TJvWndProcHookDemoMainForm.btnRecreateWndClick(Sender: TObject);
begin
  RecreateWnd;
end;

end.

