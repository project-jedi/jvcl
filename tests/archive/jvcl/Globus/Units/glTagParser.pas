unit glTagParser;

interface
uses Classes, SysUtils, httpapp;

type

  TglTagParser = class
  private
    TagParams: TStrings;
  public
    AttributeFilter: TStrings;
    constructor Create;
    destructor Destroy; override;
    function Attributes(const sTag: string): TStrings;
    procedure OnHTMLTag(Sender: TObject; Tag: TTag; const TagString: String; TagParams: TStrings; var ReplaceText: String);
  end;

implementation
//uses ;

function TglTagParser.Attributes(const sTag: string): TStrings;
var
  i: integer;
  PageProducer: TPageProducer;
  sTemp, sIncludeParamName, sIncludeParamValue: string;
begin
  Result := TStringList.Create;
  try
  PageProducer := TPageProducer.Create(nil);
  PageProducer.HTMLDoc.Text := StringReplace(sTag, '<?', '<#', []);
  PageProducer.OnHTMLTag := OnHTMLTag;
  sTemp := PageProducer.Content;

  try
    for i := 1 to TagParams.Count-1 do
    begin
      sIncludeParamValue := TagParams.Values[TagParams.Names[i]];
      sIncludeParamName := TagParams.Names[i];
      sIncludeParamValue := StringReplace(sIncludeParamValue, '[', '<', [rfReplaceAll]);
      sIncludeParamValue := StringReplace(sIncludeParamValue, ']', '>', [rfReplaceAll]);
      Result.Add(sIncludeParamName + '=' + sIncludeParamValue);
    end;
  finally
    PageProducer.Free;
  end;
  except
    FreeAndNil(Result);
  end;
end;


constructor TglTagParser.Create;
begin
  TagParams := TStringList.Create;
  AttributeFilter := TStringList.Create;
end;

destructor TglTagParser.Destroy;
begin
  TagParams.Free;
  AttributeFilter.Free;
  inherited;
end;

procedure TglTagParser.OnHTMLTag(Sender: TObject; Tag: TTag; const TagString: String; TagParams: TStrings; var ReplaceText: String);
var
  i: integer;
begin
  TagParams.Text := LowerCase(TagParams.Text);

  with AttributeFilter do
  for i:=0 to pred(TagParams.Count) do
    if IndexOfName(TagParams.Names[i]) <> -1 then
    if Values[TagParams.Names[i]] = Values[TagParams.Names[i]] then
    begin
      self.TagParams.Assign(TagParams);
    end;
end;

end.
