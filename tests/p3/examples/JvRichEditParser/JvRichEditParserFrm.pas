unit JvRichEditParserFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TfrmMain = class(TForm)
    reOriginal: TRichEdit;
    OpenDialog1: TOpenDialog;
    reParsed: TRichEdit;
    Panel1: TPanel;
    btnParse: TButton;
    btnLoad: TButton;
    Splitter1: TSplitter;
    btnRecreate: TButton;
    chkWordwrap: TCheckBox;
    chkNoWrite: TCheckBox;
    StatusBar1: TStatusBar;
    procedure btnLoadClick(Sender: TObject);
    procedure btnParseClick(Sender: TObject);
    procedure btnRecreateClick(Sender: TObject);
    procedure chkWordwrapClick(Sender: TObject);
  private
    procedure DoAttributeChange(Sender: TObject;
      Attributes: TTextAttributes; ParaAttributes:TParaAttributes;const AText: string);
    procedure DoRecreateDoc(Sender: TObject;
      Attributes: TTextAttributes; ParaAttributes:TParaAttributes;const AText: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  JvRTFParser, TypInfo;

{$R *.dfm}

procedure TfrmMain.btnLoadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    reOriginal.Lines.LoadFromFile(OpenDialog1.Filename);
end;

function StyleToStr(Style:TFontStyles):String;
begin
  Result := '';
  if fsBold in Style then
    Result := Result + 'fsBold,';
  if fsItalic in Style then
    Result := Result + 'fsItalic,';
  if fsUnderline in Style then
    Result := Result + 'fsUnderline,';
  if fsStrikeOut in Style then
    Result := Result + 'fsStrikeOut,';
  if (Length(Result) > 0) and (AnsiLastChar(Result) = ',') then
    SetLength(Result,Length(result) - 1);
  Result := '[' + Result + ']';
end;

function AttrToText(Attr:TTextAttributes;PAttr:TParaAttributes):string;
var S:String;
begin
  if not CharsetToIdent(Attr.CharSet,S) then S := IntToStr(Attr.CharSet);
  Result := Format('%s-%.2d-%s-%s-%s (%d,%d,%d,%s,%s):',
    [Attr.Name,Attr.Size,ColorToString(Attr.Color),S,StyleToStr(Attr.Style),
     PAttr.FirstIndent, PAttr.LeftIndent,PAttr.RightIndent,GetEnumName(typeinfo(TAlignment), Ord(PAttr.Alignment)),
       GetEnumName(typeinfo(TNumberingStyle),Ord(PAttr.Numbering))]);
end;

procedure TfrmMain.DoAttributeChange(Sender:TObject;Attributes:TTextAttributes;
  ParaAttributes:TParaAttributes; const AText:string);
begin
  reParsed.SelLength := 0;
  reParsed.SelAttributes.Style := [fsBold];
  reParsed.SelText := AttrToText(Attributes,ParaAttributes);
  reParsed.SelLength := 0;
  reParsed.SelAttributes.Style := [fsItalic];
  reParsed.SelText := trim(AText);
  reParsed.Lines.Add('');
end;

procedure TfrmMain.btnParseClick(Sender: TObject);
var RP:TJvRichEditParser;tt:Cardinal;l:integer;
begin
  reParsed.Clear;
  reParsed.DefAttributes.Assign(Font);
  RP := TJvRichEditParser.Create(reOriginal);
  Screen.Cursor := crHourGlass;
  try
    if not chkNoWrite.Checked then
      RP.OnAttributeChange := DoAttributeChange;
    tt := GetTickCount;
    RP.ExtractContent;
  finally
    RP.Free;
    Screen.Cursor := crDefault;
  end;
  tt := GetTickCount - tt;
  if tt = 0 then tt := 1;
  l := Length(reOriginal.Lines.Text);
  StatusBar1.Panels[0].Text := Format('  Parsed %d bytes in %d ms (%f bytes per ms)',[l,tt,l/tt]);
end;

procedure TfrmMain.DoRecreateDoc(Sender: TObject;
  Attributes: TTextAttributes; ParaAttributes: TParaAttributes;
  const AText: string);
begin
  reParsed.SelLength := 0;
  reParsed.Paragraph.Assign(ParaAttributes);
  reParsed.SelAttributes := Attributes;
  reParsed.SelText := AText;
end;

procedure TfrmMain.btnRecreateClick(Sender: TObject);
var RP:TJvRichEditParser;tt:Cardinal;l:integer;
begin
  reParsed.Clear;
  reParsed.DefAttributes.Assign(Font);
  RP := TJvRichEditParser.Create(reOriginal);
  Screen.Cursor := crHourGlass;
  try
    if not chkNoWrite.Checked then
      RP.OnAttributeChange := DoRecreateDoc;
    tt := GetTickCount;
    RP.ExtractContent;
  finally
    RP.Free;
    Screen.Cursor := crDefault;
  end;
  tt := GetTickCount - tt;
  if tt = 0 then tt := 1;
  l := Length(reOriginal.Lines.Text);
  StatusBar1.Panels[0].Text := Format('  Parsed %d bytes in %d ms (%f bytes per ms)',[l,tt,l/tt]);
end;

procedure TfrmMain.chkWordwrapClick(Sender: TObject);
begin
  reOriginal.WordWrap := chkWordwrap.Checked;
  reParsed.WordWrap := chkWordwrap.Checked;
end;

end.
