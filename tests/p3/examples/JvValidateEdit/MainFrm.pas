{
 A demo for the JvValidateEdit component. You don't need to install the component
 to run this demo, but the JvValidateEdit and the JvCharStrEditor units
 must be in the path ..\..\source.
}
unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvValidateEdit, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button2: TButton;
    RichEdit1: TRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1KeyPress(Sender: TObject; var Key: Char);
    procedure ComboBox1Change(Sender: TObject);
  private
    procedure DoCustomValidate(Sender: TObject; Key: char;
      const AText: string; var IsValid: boolean);
    { Private declarations }
  public
    { Public declarations }
    FValidateEdit:TJvValidateEdit;
  end;

var
  Form1: TForm1;

implementation
uses
  TypInfo, JvCharStrEditor;

{$R *.DFM}

procedure TForm1.DoCustomValidate(Sender:TObject;Key:char;const AText:string; var IsValid:boolean);
function KeyOrAscii(Key:Char):string;
begin
  if Key < #32 then
    Result := Format('#%s',[Key])
  else
    Result := Key;
end;

begin
  IsValid := MessageBox(Handle,PChar(Format('Accept this key: %s?',[KeyOrAscii(Key)])),PChar('Validate'),MB_YESNO) = IDYES;
end;

procedure TForm1.FormCreate(Sender: TObject);
var i:TJvCharType;
begin
  FValidateEdit := TJvValidateEdit.Create(self);
  FValidateEdit.Parent := self;
  FValidateEdit.SetBounds(Label2.Left,Label2.Top + Label2.Height + 4,self.ClientWidth - Label2.Left * 2,FValidateEdit.Height);
  FValidateEdit.Anchors := [akLeft,akTop,akRight];
  FValidateEdit.OnCustomValidate := DoCustomValidate;
  for i := Low(TJvCharType) to High(TJvCharType) do
    ComboBox1.Items.Add(GetEnumName(typeinfo(TJvCharType),Ord(i)));
  ComboBox1.ItemIndex := 0;
  ComboBox1Change(self);
end;


procedure TForm1.Button2Click(Sender: TObject);
var S:String;
begin
  S := FValidateEdit.Characters;
  if TfrmJvCharEditDlg.Edit(S) then
  begin
    FValidateEdit.Characters := S;
    RichEdit1.Lines.Text := SysCharSetToString(StringToSysCharSet(FValidateEdit.Characters),true);
    FValidateEdit.MakeValid;
  end;
end;

procedure TForm1.ComboBox1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    ComboBox1Change(Sender);
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  FValidateEdit.CharType := TJvCharType(ComboBox1.ItemIndex);
end;

end.

