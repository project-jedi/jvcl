unit SourceEditUnit;

interface

uses
  Windows, Messages, SysUtils{, Variants}, Classes, Graphics, Controls, Forms,
  Dialogs, JvHLEditor;

type
  TSourceEditForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure mwCustomEdit1StatusChange(Sender:TObject);
  public
    { Public declarations }
    SourceEditor: TJvHLEditor;
//    SyntaxHighlighter:
    procedure LoadFromFile(AFileName: string);

  end;
var
  SourceEditForm: TSourceEditForm;

implementation

uses Main;

{$R *.dfm}

{ TSourceEditForm }

procedure TSourceEditForm.LoadFromFile(AFileName: string);
begin
  SourceEditor.Lines.LoadFromFile(AFileName);
  Caption := AFileName;
end;

procedure TSourceEditForm.FormCreate(Sender: TObject);
begin
  SourceEditor := TJvHLEditor.Create(Self);
  SourceEditor.Parent := Self;
  SourceEditor.Align := alClient;
  SourceEditor.ReadOnly := True;
  SourceEditor.Font.Name := 'Courier New';
  SourceEditor.Font.Size := 10;
  SourceEditor.RightMargin := 80;
  SourceEditor.Highlighter := hlCBuilder;
//  SourceEditor.MaxUndo := $FFFF;
  SourceEditor.OnChangeStatus := mwCustomEdit1StatusChange;
end;

procedure TSourceEditForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  MainForm.ActiveEdit := nil;
end;

procedure TSourceEditForm.WMSize(var Message: TWMSize);
begin
  if (Message.SizeType = SIZE_MAXIMIZED) or
    (Message.SizeType = SIZE_MINIMIZED) or
    (Message.SizeType = SIZE_RESTORED) then
    MainForm.MainMenu_ToolBar.Invalidate;
  inherited;
end;

procedure TSourceEditForm.FormActivate(Sender: TObject);
begin
  MainForm.ActiveEdit := SourceEditor;
end;

procedure TSourceEditForm.mwCustomEdit1StatusChange(Sender: TObject);
begin
  MainForm.StatusBar1.Panels[0].Text := Format('Ln %d, Col %d', [SourceEditor.CaretY, SourceEditor.CaretX]);
end;

end.
