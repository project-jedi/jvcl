{$I JVCL.INC}

unit JvTranslatorMainFormU;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6_UP} Variants,{$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, JvTranslator, StdCtrls, ComCtrls, JvComponent;

type
  TJvTranslatorMainForm = class(TForm)
    TreeView1: TTreeView;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    JvTranslator1: TJvTranslator;
    Button3: TButton;
    Button4: TButton;
    RichEdit1: TRichEdit;
    ListView1: TListView;
    Variables: TJvTranslatorStrings;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  end;

var
  JvTranslatorMainForm: TJvTranslatorMainForm;

implementation

uses
  JclStrings;

{$R *.dfm}

var
 CONST_SomeText: string = 'Wooow, this was good :p';
 CONST_SomeMoreText: string = 'Wooow, this was better :p';
 CONST_EvenMoreText: string = 'Wooow, this was the best :p';


procedure TJvTranslatorMainForm.FormCreate(Sender: TObject);
begin
  Variables.Add('SomeText',CONST_SomeText);
  Variables.Add('SomeMoreText',CONST_SomeMoreText);
  Variables.Add('EvenMoreText',CONST_EvenMoreText);
end;

procedure TJvTranslatorMainForm.Button3Click(Sender: TObject);
begin
  ShowMessage(CONST_SomeText + #13#10 + CONST_SomeMoreText + #13#10 + CONST_EvenMoreText);
end;

procedure TJvTranslatorMainForm.Button1Click(Sender: TObject);
var
  transFileName : string;
begin

  transFileName := ExtractFilePath(Application.ExeName);
  transFileName := transFileName + 'French.xml';
  if not FileExists(transFileName) then
    MessageDlg('File not found: ' + transFileName, mtError, [mbOK], 0)
  else
    JvTranslator1.Translate(transFileName);
end;

procedure TJvTranslatorMainForm.Button2Click(Sender: TObject);
var
  transFileName : string;
begin

  transFileName := ExtractFilePath(Application.ExeName);
  transFileName := transFileName + 'English.xml';
  if not FileExists(transFileName) then
    MessageDlg('File not found: ' + transFileName, mtError, [mbOK], 0)
  else
    JvTranslator1.Translate(transFileName);
end;

procedure TJvTranslatorMainForm.Button4Click(Sender: TObject);
begin
  JvTranslator1.SkipClass(TRichEdit);
  RichEdit1.Lines.Text := JvTranslator1.ComponentToXML(self,true);
end;

end.
