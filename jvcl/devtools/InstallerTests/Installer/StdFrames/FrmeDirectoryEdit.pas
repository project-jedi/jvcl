unit FrmeDirectoryEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  {$WARNINGS OFF}
  FileCtrl,
  {$WARNINGS ON}
  Dialogs, StdCtrls, Buttons, ConfigOptions;

type
  TFrameDirectoryEdit = class(TFrame)
    edtDirectory: TEdit;
    btnBrowse: TBitBtn;
    lblCaption: TLabel;
    procedure edtDirectoryExit(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
  private
    FOption: TOption;
    procedure SetOption(const Value: TOption);
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override; 
    property Option: TOption read FOption write SetOption;
  end;

implementation

{$R *.dfm}

procedure TFrameDirectoryEdit.btnBrowseClick(Sender: TObject);
var
  Directory: string;
begin
  Directory := Trim(edtDirectory.Text);
  if SelectDirectory('Browse', '', Directory) then
  begin
    Option.AsString := Trim(Directory);
    edtDirectory.Text := Option.AsString;
  end;
end;

procedure TFrameDirectoryEdit.SetOption(const Value: TOption);
begin
  FOption := Value;
  if Assigned(FOption) then
  begin
    edtDirectory.Text := Option.AsString;
    lblCaption.Caption := Option.Caption;
    edtDirectory.Hint := Option.Hint;
    edtDirectory.ShowHint := edtDirectory.Hint <> '';
  end;
end;

procedure TFrameDirectoryEdit.edtDirectoryExit(Sender: TObject);
begin
  if Assigned(FOption) then
    Option.AsString := Trim(edtDirectory.Text);
end;

constructor TFrameDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  edtDirectory.Anchors := [akLeft, akTop, akRight];
  btnBrowse.Anchors := [akTop, akRight];
end;

end.
