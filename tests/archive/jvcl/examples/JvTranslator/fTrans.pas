unit fTrans;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6_UP} Variants,{$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, JvTranslator, StdCtrls, ComCtrls, JvComponent;

type
  TForm1 = class(TForm)
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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var
 CONST_SomeText: string = 'Wooow, this was good :p';



procedure TForm1.FormCreate(Sender: TObject);
begin
  Variables.Add('SomeText',CONST_SomeText);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ShowMessage(CONST_SomeText);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  JvTranslator1.Translate(ExtractFilePath(Application.ExeName)+'Translations\French.xml');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  JvTranslator1.Translate(ExtractFilePath(Application.ExeName)+'Translations\English.xml');
end;

end.
