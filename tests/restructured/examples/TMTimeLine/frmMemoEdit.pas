unit frmMemoEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Menus;

type
  TMemoEditFrm = class(TForm)
    Panel1: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    reLines: TRichEdit;
    popMemo: TPopupMenu;
    Load1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Selectall1: TMenuItem;
    N2: TMenuItem;
    procedure Load1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Selectall1Click(Sender: TObject);
    procedure reLinesKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    class function Edit(Lines: TStrings;ADate:TDateTime;Icon:TIcon = nil): boolean;
  end;


implementation

{$R *.DFM}

{ TMemoEditFrm }

class function TMemoEditFrm.Edit(Lines: TStrings;ADate:TDateTime;Icon:TIcon = nil): boolean;
var f:TMemoEditFrm;
begin
  f := self.Create(nil);
  try
    f.Icon := Icon;
    f.Caption := Format(f.Caption,[DateToStr(ADate)]);
    f.reLines.Lines := Lines;
    Result := f.ShowModal = MROK;
    if Result then
      Lines.Assign(f.ReLines.Lines);
  finally
    f.Free;
  end;
end;

procedure TMemoEditFrm.Load1Click(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    if Execute then
      reLines.Lines.LoadFromFile(Filename);
  finally
    Free;
  end;
end;

procedure TMemoEditFrm.Save1Click(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  try
    if Execute then
      reLines.Lines.SaveToFile(Filename);
  finally
    Free;
  end;
end;

procedure TMemoEditFrm.Cut1Click(Sender: TObject);
begin
  reLines.CutToClipboard;
end;

procedure TMemoEditFrm.Copy1Click(Sender: TObject);
begin
  reLines.CopyToClipboard;
end;

procedure TMemoEditFrm.Paste1Click(Sender: TObject);
begin
  reLines.PasteFromClipboard;
end;

procedure TMemoEditFrm.Selectall1Click(Sender: TObject);
begin
  reLines.SelectAll;
end;

procedure TMemoEditFrm.reLinesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

end.

