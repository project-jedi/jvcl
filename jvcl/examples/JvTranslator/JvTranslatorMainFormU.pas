{$I jvcl.inc}

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
    Variables: TJvTranslatorStrings;
    Button3: TButton;
    JvTranslator2: TJvTranslator;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  end;

var
  JvTranslatorMainForm: TJvTranslatorMainForm;

implementation

uses
  JclStrings;

{$R *.dfm}

var
 CONST_SomeText: string = 'Wooow, this was good :p';


procedure TJvTranslatorMainForm.FormCreate(Sender: TObject);
begin
  Variables.Add('SomeText',CONST_SomeText);
end;

procedure TJvTranslatorMainForm.Button3Click(Sender: TObject);
begin
  ShowMessage(CONST_SomeText);
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

end.
