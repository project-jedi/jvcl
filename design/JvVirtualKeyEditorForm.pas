unit JvVirtualKeyEditorForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, JvVirtualKeySelectionFrame;

type
  TfrmJvVirtualKeyEditor = class(TForm)
    bbtOk: TBitBtn;
    bbtCancel: TBitBtn;
  private
    { Private declarations }
    FEditingFrame : TJvVirtualKeySelectionFrame;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property EditingFrame : TJvVirtualKeySelectionFrame read FEditingFrame;
  end;

var
  frmJvVirtualKeyEditor: TfrmJvVirtualKeyEditor;

implementation

{$R *.dfm}

{ TJvVirtualKeyEditorForm }

constructor TfrmJvVirtualKeyEditor.Create(AOwner: TComponent);
begin
  inherited;
  FEditingFrame := TJvVirtualKeySelectionFrame.Create(Self);
  with FEditingFrame do begin
    Left := 4;
    Top := 8;
    Parent := self;
    visible := true;
  end;
end;

destructor TfrmJvVirtualKeyEditor.Destroy;
begin
  FEditingFrame.Free;
  inherited;
end;

end.
