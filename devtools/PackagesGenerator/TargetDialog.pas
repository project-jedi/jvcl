unit TargetDialog;

{$I jvcl.inc}

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
  targets : TStringList;
begin
  targets := TStringList.Create;
  try
    EnumerateTargets(targets);

    if targets.Text <> clbBuilds.Items.Text then
    begin
      clbBuilds.Items.Assign(targets);
      for i := 0 to clbBuilds.Items.Count - 1 do
        clbBuilds.Checked[i] := True;
    end;
  finally
    targets.Free;
  end;
end;

end.
