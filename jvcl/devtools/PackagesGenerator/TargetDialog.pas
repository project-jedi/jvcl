unit TargetDialog;

{$I JVCL.INC}

interface

uses
  Windows, Messages, SysUtils, {$IFDEF COMPILER6_UP}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, CheckLst;

type
  TfrmTargets = class(TForm)
    bbtOk: TBitBtn;
    bbtCancel: TBitBtn;
    clbBuilds: TCheckListBox;
    lblPleaseIndicate: TLabel;
    lblOnlyChanged: TLabel;
    chkGenDof: TCheckBox;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Path : string;
  end;

var
  frmTargets: TfrmTargets;

implementation

{$R *.dfm}

uses JclStrings, GenerateUtils;

{ TfrmTargets }

procedure TfrmTargets.FormShow(Sender: TObject);
var
  i : Integer;
begin
  if clbBuilds.Items.Count = 0 then
  begin
    EnumerateTargets(clbBuilds.Items);
    for i := 0 to clbBuilds.Items.Count - 1 do
      clbBuilds.Checked[i] := True;
  end;
end;

end.
