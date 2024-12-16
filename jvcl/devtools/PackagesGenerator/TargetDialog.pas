unit TargetDialog;

{$I jvcl.inc}

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
    btnToggle: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnToggleClick(Sender: TObject);
  private
    { Private declarations }
    FTargets: TStrings;
    procedure SetTargets(const Value: TStrings);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Targets: TStrings read FTargets write SetTargets;
  end;

var
  frmTargets: TfrmTargets;

implementation

{$R *.dfm}

uses
  JclStrings,
  GenerateUtils;

{ TfrmTargets }

procedure TfrmTargets.btnToggleClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to clbBuilds.Items.Count - 1 do
    clbBuilds.Checked[i] := not clbBuilds.Checked[i];
end;

constructor TfrmTargets.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTargets := TStringList.Create;
end;

destructor TfrmTargets.Destroy;
begin
  FTargets.Free;
  inherited Destroy;
end;

procedure TfrmTargets.FormShow(Sender: TObject);
var
  i: Integer;
begin
  if Targets.Text <> clbBuilds.Items.Text then
  begin
    clbBuilds.Items.Assign(Targets);
    for i := 0 to clbBuilds.Items.Count - 1 do
      clbBuilds.Checked[i] := True;
  end;
end;

procedure TfrmTargets.SetTargets(const Value: TStrings);
begin
  FTargets.Assign(Value);
end;

end.
