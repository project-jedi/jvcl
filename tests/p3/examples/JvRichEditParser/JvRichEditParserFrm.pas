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
    procedure btnLoadClick(Sender: TObject);
    procedure btnParseClick(Sender: TObject);
  private
    procedure DoAttributeChange(Sender: TObject;
      Attributes: TTextAttributes; const AText: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses JvRTFParser;

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
    Result := Result + 'Bold,';
  if fsItalic in Style then
    Result := Result + 'Italic,';
  if fsUnderline in Style then
    Result := Result + 'Underline,';
  if fsStrikeOut in Style then
    Result := Result + 'StrikeOut,';
  if (Length(Result) > 0) and (AnsiLastChar(Result) = ',') then
    SetLength(Result,Length(result) - 1);
  Result := '[' + Result + ']';
end;

function AttrToText(Attr:TTextAttributes):string;
begin
  Result := Format('%s-%d-$%.8x - %s: ',[Attr.Name,Attr.Size,Attr.Color,StyleToStr(Attr.Style)]);
end;

procedure TfrmMain.DoAttributeChange(Sender:TObject;Attributes:TTextAttributes;const AText:string);
var S:String;
begin
  reParsed.SelLength := 0;
  reParsed.SelAttributes.Style := [fsBold];
  reParsed.SelText := AttrToText(Attributes);
  reParsed.SelLength := 0;
  reParsed.SelAttributes.Style := [fsItalic];
  reParsed.SelText := trim(AText);
  reParsed.Lines.Add('');
end;

procedure TfrmMain.btnParseClick(Sender: TObject);
var RP:TJvRichEditParser;
begin
  reParsed.Clear;
  RP := TJvRichEditParser.Create(reOriginal);
  try
    RP.OnAttributeChange := DoAttributeChange;
    RP.ExtractContent;
  finally
    RP.Free;
  end;
end;

end.
