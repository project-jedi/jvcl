unit JvRTFParser;

interface
uses
  Windows, SysUtils, Graphics, ComCtrls;

type
  TJvTextAttributeChange = procedure (Sender:TObject;Attributes:TTextAttributes;const AText:string) of object;
  TJvRichEditParser = class(TObject)
  private
    FAttributes:TFont;
    FRichEdit:TCustomRichEdit;
    FOnAttributeChange: TJvTextAttributeChange;
  protected
    function NextChange(StartPos:integer):integer;
    function SameAttributes(Attr1:TFont;Attr2:TTextAttributes):boolean;
  public
    constructor Create(ARichEdit:TCustomRichEdit);

    procedure ExtractContent;
    procedure AttributeChange(Attributes:TTextAttributes;const AText:string);
    property OnAttributeChange:TJvTextAttributeChange read FOnAttributeChange write FOnAttributeChange;
    destructor Destroy; override;
  end;

implementation
uses
  JclStrings;

{ TJvRichEditParser }

procedure TJvRichEditParser.AttributeChange(Attributes: TTextAttributes;
  const AText: string);
begin
  if Assigned(FOnAttributeChange) then
    FOnAttributeChange(self,Attributes,AText);
end;

constructor TJvRichEditParser.Create(ARichEdit: TCustomRichEdit);
begin
  inherited Create;
  FRichEdit := ARichEdit;
  FAttributes := TFont.Create;
end;

destructor TJvRichEditParser.Destroy;
begin
  FAttributes.Free;
  inherited;
end;

procedure TJvRichEditParser.ExtractContent;
var
  FStartPos,FEndPos:integer;
begin
  Assert(FRichEdit <> nil,'');
  FStartPos := 1;
  FEndPos := NextChange(FStartPos);
  while FEndPos <> FStartPos do
  begin
    FRichEdit.SelStart := FStartPos;
    FRichEdit.SelLength := FEndPos - FStartPos;
    AttributeChange(FRichEdit.SelAttributes, FRichEdit.SelText);
    FStartPos := FEndPos;
    FEndPos := NextChange(FStartPos);
  end;
end;

function TJvRichEditParser.NextChange(StartPos:integer):integer;
var tmp,l:integer;
begin
  FRichEdit.SelStart := StartPos;
  FRichEdit.SelLength := 1;
  FAttributes.Assign(FRichEdit.SelAttributes);
  l := Length(FRichEdit.Lines.text);
  while SameAttributes(FAttributes,FRichEdit.SelAttributes) do
  begin
    tmp := FRichEdit.SelStart;
    FRichEdit.SelStart := tmp + 1;
    while FRichEdit.SelStart <= tmp do
    begin
      Inc(tmp);
      FRichEdit.SelStart := tmp;
      if (FRichEdit.SelStart = tmp) or (tmp >= l) then Break;
    end;
    if tmp >= FRichEdit.SelStart then Break;
  end;
  Result := FRichEdit.SelStart;
end;

function TJvRichEditParser.SameAttributes(Attr1:TFont;
  Attr2: TTextAttributes): boolean;
begin
  // ordered by "most likely to happen"
  Result :=
  (Attr1.Size = Attr2.Size) and
  (Attr1.Color = Attr2.Color) and
  (Attr1.Style = Attr2.Style) and
  (Attr1.Charset = Attr2.CharSet) and
  (Attr1.Pitch = Attr2.Pitch) and
  AnsiSameText(Attr1.Name,Attr2.Name);
end;

end.
