unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvLookOut, JvValidateEdit, StdCtrls, ComCtrls;

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
    procedure ComboBox1CloseUp(Sender: TObject);
    procedure ComboBox1KeyPress(Sender: TObject; var Key: Char);
  private
    procedure DoCustomValidate(Sender: TObject; Key: char;
      const AText: string; var IsValid: boolean);
    { Private declarations }
  public
    { Public declarations }
    FE:TJvValidateEdit;
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
  FE := TJvValidateEdit.Create(self);
  FE.Parent := self;
  FE.SetBounds(Label2.Left,Label2.Top + Label2.Height + 4,self.ClientWidth - Label2.Left * 2,FE.Height);
  FE.Anchors := [akLeft,akTop,akRight];
  FE.OnCustomValidate := DoCustomValidate;
  for i := Low(TJvCharType) to High(TJvCharType) do
    ComboBox1.Items.Add(GetEnumName(typeinfo(TJvCharType),Ord(i)));
  ComboBox1.ItemIndex := 0;
  ComboBox1CloseUp(self);
end;

function StringToSet(const S:string):TSysCharSet;
var i:integer;
begin
  Result := [];
  for i := 1 to Length(S) do
    Include(result,S[i]);
end;

function GetCharName(const Ch:char):string;
var FTmpString:string;FTmpType:word;
begin
  Result := cAsciiNames[Ch];
  FTmpString := Ch;
  GetStringTypeEx(LOCALE_USER_DEFAULT,CT_CTYPE1,Pchar(FtmpString),1,FTmpType);
  if (FTmpType and C1_CNTRL = 0) and (Ch <> #32) then
    Result := #39 + Result + #39;
end;

// far from perfect, but kind of works...
function SysCharSetToString(ASet:TSysCharSet;Brackets:boolean):string;
var i,LastChar,PrevChar:char;
begin
  PrevChar := #255;
  LastChar := #0;
  for i := #0 to #255 do
  begin
    if i in ASet then
    begin
//      if PrevChar = #0 then
      if Ord(i)-Ord(PrevChar) <> 1 then
      begin
        if Result <> '' then
          Result := Result + ',' + getCharName(i)
        else
          Result := getCharName(i);
        LastChar := i;
      end
      else if i = #255 then
      begin
        if Result = '' then
          Result := GetCharName(i)
        else if Ord(i) - Ord(LastChar) > 1 then
          Result := Result + '...' + GetCharName(i)
        else
          Result := Result + ',' + GetCharName(i);
        Break;
      end;
      PrevChar := i;
    end
    else
    begin
      if Ord(i) - Ord(LastChar) > 1 then
        Result := Result + '...' + GetCharName(Pred(i))
      else if (LastChar = #0) and (Pred(i) <> LastChar) and (i <> #0) then
        Result := Result + ',' + GetCharName(Pred(i));
      PrevChar := #255;
      LastChar := i;
    end;
  end;
  if (Length(Result) > 0) and (AnsiLastChar(Result) = ',') then
    SetLength(Result,Length(Result)-1);
  if Brackets then
    Result := '[' + Result + ']';
end;

procedure TForm1.Button2Click(Sender: TObject);
var S:String;
begin
  S := FE.Characters;
  if TfrmJvCharEditDlg.Edit(S) then
  begin
    FE.Characters := S;
    RichEdit1.Lines.Text := SysCharSetToString(StringToSet(FE.Characters),true);
    FE.MakeValid;
  end;
end;

procedure TForm1.ComboBox1CloseUp(Sender: TObject);
begin
  FE.CharType := TJvCharType(ComboBox1.ItemIndex);
end;

procedure TForm1.ComboBox1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    ComboBox1CloseUp(Sender);
end;

end.

