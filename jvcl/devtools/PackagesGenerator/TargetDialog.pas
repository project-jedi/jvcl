unit TargetDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
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
  wasEmpty : Boolean;
  i : Integer;
begin
  wasEmpty := clbBuilds.Count = 0;
  EnumerateTargets(Path, clbBuilds.Items);
  if wasEmpty then
  begin
    for i := 0 to clbBuilds.Count - 1 do
      clbBuilds.Checked[i] := True;
  end;
end;

end.
