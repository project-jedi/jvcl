unit MainFrm;

interface
// Enable this define (remove the dot) if you have Troy Wolbrink's Tnt Controls installed
// (http://home.ccci.org/wolbrink/tnt/delphi_unicode_controls.htm)
{$DEFINE USETNT}
// Enable this define if you want to include JclUnicode and the TJvCharMap.CharRange.Filter functionality
// You must enable this define in JvCharMap as well
{$DEFINE USEUNICODE}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus,
  JvColorCombo, JvCharMap, JvCombobox
  {$IFDEF USEUNICODE}, JclUnicode{$ENDIF}
  {$IFDEF USETNT}, TntStdCtrls{$ENDIF};

type
  TForm1 = class(TForm)
    FontDialog1: TFontDialog;
    Panel1: TPanel;
    btnFont: TButton;
    chkZoomPanel: TCheckBox;
    Label1: TLabel;
    Edit1: TEdit;
    udStart: TUpDown;
    Label2: TLabel;
    Edit2: TEdit;
    udEnd: TUpDown;
    Label3: TLabel;
    Edit3: TEdit;
    udColumns: TUpDown;
    cbColor: TJvColorComboBox;
    cbFont: TJvFontComboBox;
    chkUnicode: TCheckBox;
    reInfo: TRichEdit;
    PopupMenu1: TPopupMenu;
    Copy1: TMenuItem;
    btnSelect: TButton;
    lblFilter: TLabel;
    cbFilter: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure chkZoomPanelClick(Sender: TObject);
    procedure udStartClick(Sender: TObject; Button: TUDBtnType);
    procedure udEndClick(Sender: TObject; Button: TUDBtnType);
    procedure udColumnsClick(Sender: TObject; Button: TUDBtnType);
    procedure chkUnicodeClick(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure cbColorChange(Sender: TObject);
    procedure cbFontChange(Sender: TObject);
    procedure cbFilterClick(Sender: TObject);
  private
    { Private declarations }
{$IFDEF USETNT}
    edCharacter: TTntEdit;
{$ELSE}
    edCharacter: TEdit;
{$ENDIF}
{$IFDEF USEUNICODE}
    procedure FillFilter;
{$ENDIF}    
    procedure DoJMKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure DoJMMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure DoJMMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure DisplayInfo;
  public
    { Public declarations }
    JM: TJvCharMap;
  end;

var
  Form1: TForm1;

implementation
uses
  TypInfo,
{$IFDEF USETNT}
  TntClipbrd;
{$ELSE}
  ClipBrd;
{$ENDIF}

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  JM := TJvCharMap.Create(self);
//  JM.Align := alClient;
  JM.Parent := self;
  JM.CharRange.EndChar := 512;
  JM.OnKeyUp := DoJMKeyUp;
  JM.OnMouseUp := DoJMMouseUp;
  JM.OnMouseWheel := DoJMMouseWheel;

  JM.ClientWidth := JM.CellSize.cx * JM.Columns + JM.Columns;
  JM.Left := (ClientWidth - JM.Width) div 2;
  JM.Top := 8;
  JM.Height := Panel1.Top - JM.Top - 20;
  JM.Anchors := [akTop, akBottom];
  JM.PopupMenu := PopupMenu1;
  JM.AutoSizeWidth := true;
  chkZoomPanel.Checked := JM.ShowZoomPanel;
  udStart.Position := JM.CharRange.StartChar;
  udEnd.Position := JM.CharRange.EndChar;
  udColumns.Position := JM.Columns;
  cbColor.ColorValue := JM.Color;
  cbFont.Fontname := JM.Font.Name;
  cbColor.OnChange := cbColorChange;
  cbFont.OnChange := cbFontChange;

{$IFDEF USETNT}
  edCharacter := TTntEdit.Create(self);
{$ELSE}
  edCharacter := TEdit.Create(self);
{$ENDIF}
  edCharacter.Parent := Panel1;
  edCharacter.Left := 312;
  edCharacter.Top := 16;
  edCharacter.Width := 121;
  edCharacter.Height := 22;
  edCharacter.Anchors := [akLeft, akTop, akRight];
  edCharacter.TabOrder := 11;
  {$IFDEF USEUNICODE}
  FillFilter;
  {$ELSE}
  lblFilter.Visible := false;
  cbFilter.Visible := false;
  {$ENDIF}
  ActiveControl := JM;
end;

procedure TForm1.btnFontClick(Sender: TObject);
begin
  FontDialog1.Font := JM.Font;
  if FontDialog1.Execute then
  begin
    JM.Font := FontDialog1.Font;
    cbFont.Fontname := JM.Font.Name;
  end;
end;

procedure TForm1.chkZoomPanelClick(Sender: TObject);
begin
  JM.ShowZoomPanel := chkZoomPanel.Checked;
end;

procedure TForm1.DoJMMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  DisplayInfo;
end;

procedure TForm1.DoJMKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  DisplayInfo;
end;

procedure TForm1.udStartClick(Sender: TObject; Button: TUDBtnType);
begin
  JM.CharRange.StartChar := udStart.Position;
end;

procedure TForm1.udEndClick(Sender: TObject; Button: TUDBtnType);
begin
  if not chkUnicode.Checked then
    JM.CharRange.EndChar := udEnd.Position;
end;

procedure TForm1.udColumnsClick(Sender: TObject; Button: TUDBtnType);
begin
  JM.Columns := udColumns.Position;
end;

function GetTypeString1(AChar: WideChar): WideString;
var
  ACharInfo: word;
begin
  Result := '';
  if GetStringTypeExW(LOCALE_USER_DEFAULT, CT_CTYPE1, @AChar, 1, ACharInfo) then
  begin
    if ACharInfo and C1_UPPER = C1_UPPER then
      Result := Result + ',C1_UPPER';
    if ACharInfo and C1_LOWER = C1_LOWER then
      Result := Result + ',C1_LOWER';
    if ACharInfo and C1_DIGIT = C1_DIGIT then
      Result := Result + ',C1_DIGIT';
    if ACharInfo and C1_SPACE = C1_SPACE then
      Result := Result + ',C1_SPACE';
    if ACharInfo and C1_PUNCT = C1_PUNCT then
      Result := Result + ',C1_PUNCT';
    if ACharInfo and C1_CNTRL = C1_CNTRL then
      Result := Result + ',C1_CNTRL';
    if ACharInfo and C1_BLANK = C1_BLANK then
      Result := Result + ',C1_BLANK';
    if ACharInfo and C1_XDIGIT = C1_XDIGIT then
      Result := Result + ',C1_XDIGIT';
    if ACharInfo and C1_ALPHA = C1_ALPHA then
      Result := Result + ',C1_ALPHA';
  end;
  if Result <> '' then
    Result := Copy(Result, 2, MaxInt);
end;

function GetTypeString2(AChar: WideChar): WideString;
var
  ACharInfo: word;
begin
  Result := '';
  if GetStringTypeExW(LOCALE_USER_DEFAULT, CT_CTYPE2, @AChar, 1, ACharInfo) then
  begin
    if ACharInfo and C2_LEFTTORIGHT = C2_LEFTTORIGHT then
      Result := Result + ',C2_LEFTTORIGHT';
    if ACharInfo and C2_RIGHTTOLEFT = C2_RIGHTTOLEFT then
      Result := Result + ',C2_RIGHTTOLEFT';
    if ACharInfo and C2_EUROPENUMBER = C2_EUROPENUMBER then
      Result := Result + ',C2_EUROPENUMBER';
    if ACharInfo and C2_EUROPESEPARATOR = C2_EUROPESEPARATOR then
      Result := Result + ',C2_EUROPESEPARATOR';
    if ACharInfo and C2_EUROPETERMINATOR = C2_EUROPETERMINATOR then
      Result := Result + ',C2_EUROPETERMINATOR';
    if ACharInfo and C2_ARABICNUMBER = C2_ARABICNUMBER then
      Result := Result + ',C2_ARABICNUMBER';
    if ACharInfo and C2_COMMONSEPARATOR = C2_COMMONSEPARATOR then
      Result := Result + ',C2_COMMONSEPARATOR';
    if ACharInfo and C2_BLOCKSEPARATOR = C2_BLOCKSEPARATOR then
      Result := Result + ',C2_BLOCKSEPARATOR';
    if ACharInfo and C2_SEGMENTSEPARATOR = C2_SEGMENTSEPARATOR then
      Result := Result + ',C2_SEGMENTSEPARATOR';
    if ACharInfo and C2_WHITESPACE = C2_WHITESPACE then
      Result := Result + ',C2_WHITESPACE';
    if ACharInfo and C2_OTHERNEUTRAL = C2_OTHERNEUTRAL then
      Result := Result + ',C2_OTHERNEUTRAL';
  end;
  if Result <> '' then
    Result := Copy(Result, 2, MaxInt);
end;

function GetTypeString3(AChar: WideChar): WideString;
var
  ACharInfo: word;
begin
  Result := '';
  if GetStringTypeExW(LOCALE_USER_DEFAULT, CT_CTYPE3, @AChar, 1, ACharInfo) then
  begin
    if ACharInfo and C3_NONSPACING = C3_NONSPACING then
      Result := Result + ',C3_NONSPACING';
    if ACharInfo and C3_DIACRITIC = C3_DIACRITIC then
      Result := Result + ',C3_DIACRITIC ';
    if ACharInfo and C3_VOWELMARK = C3_VOWELMARK then
      Result := Result + ',C3_VOWELMARK';
    if ACharInfo and C3_SYMBOL = C3_SYMBOL then
      Result := Result + ',C3_SYMBOL';
    if ACharInfo and C3_KATAKANA = C3_KATAKANA then
      Result := Result + ',C1_PUNCT';
    if ACharInfo and C3_HIRAGANA = C3_HIRAGANA then
      Result := Result + ',C3_HIRAGANA';
    if ACharInfo and C3_HALFWIDTH = C3_HALFWIDTH then
      Result := Result + ',C3_HALFWIDTH';
    if ACharInfo and C3_FULLWIDTH = C3_FULLWIDTH then
      Result := Result + ',C3_FULLWIDTH';
    if ACharInfo and C3_IDEOGRAPH = C3_IDEOGRAPH then
      Result := Result + ',C3_IDEOGRAPH';
    if ACharInfo and C3_KASHIDA = C3_KASHIDA then
      Result := Result + ',C3_KASHIDA';
    if ACharInfo and C3_ALPHA = C3_ALPHA then
      Result := Result + ',C3_ALPHA';
  end;
  if Result <> '' then
    Result := Copy(Result, 2, MaxInt);
end;

procedure TForm1.DisplayInfo;
begin
  reInfo.Clear;
  reInfo.Lines.Add('Character Type: ' + GetTypeString1(JM.Character));
  reInfo.Lines.Add('Bidirectional Layout: ' + GetTypeString2(JM.Character));
  reInfo.Lines.Add('Text Processing:' + GetTypeString3(JM.Character));
  reInfo.Lines.Add(Format('Keyboard Code: U+%.4x', [Ord(JM.Character)]));
  reInfo.Hint := trim(reInfo.Lines.Text);
end;

procedure TForm1.chkUnicodeClick(Sender: TObject);
begin
  lblFilter.Enabled := chkUnicode.Checked;
  cbFilter.Enabled := chkUnicode.Checked;
  if chkUnicode.Checked then
  {$IFDEF USEUNICODE}
    JM.CharRange.Filter := TUnicodeBlock(cbFilter.ItemIndex)
  {$ELSE}
    JM.CharRange.EndChar := $FEFF
  {$ENDIF}
  else
    JM.CharRange.EndChar := udEnd.Position;

end;

procedure TForm1.Copy1Click(Sender: TObject);
begin
{$IFDEF USETNT}
  TntClipboard.AsWideText := JM.Character;
{$ELSE}
  Clipboard.AsText := WideString(JM.Character);
{$ENDIF}
end;

procedure TForm1.DoJMMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  DisplayInfo;
end;

procedure TForm1.btnSelectClick(Sender: TObject);
begin
  edCharacter.Text := edCharacter.Text + JM.Character;
end;

procedure TForm1.cbColorChange(Sender: TObject);
begin
  if JM <> nil then
    JM.Color := cbColor.ColorValue;
end;

procedure TForm1.cbFontChange(Sender: TObject);
begin
  if JM <> nil then
    JM.Font.Name := cbFont.FontName;
end;

{$IFDEF USEUNICODE}
procedure TForm1.FillFilter;
var
  i: TUnicodeBlock;
begin
  cbFilter.Items.BeginUpdate;
  try
    cbFilter.Items.Clear;
    for i := Low(TUnicodeBlock) to High(TUnicodeBlock) do
      cbFilter.Items.Add(GetEnumName(typeinfo(TUnicodeBlock), Ord(i)));
  finally
    cbFilter.Items.EndUpdate;
  end;
  cbFilter.ItemIndex := Ord(JM.CharRange.Filter);
end;
{$ENDIF}

procedure TForm1.cbFilterClick(Sender: TObject);
begin
  {$IFDEF USEUNICODE}
  if chkUnicode.Checked then
    JM.CharRange.Filter := TUnicodeBlock(cbFilter.ItemIndex);
  {$ENDIF}
end;

end.

