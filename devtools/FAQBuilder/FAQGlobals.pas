unit FAQGlobals;

interface
uses
  Windows, Classes, SysUtils;

type

  TFAQOptions = class(TPersistent)
  private
    FStylePath: string;
    FImagePath: string;
    FStylesheet: TFilename;
    FAImage: TFilename;
    FQImage: TFilename;
    FFooter: TStrings;
    FItem: TStrings;
    FHeader: TStrings;
    FTitle: string;
    FFilename:string;
    procedure SetFooter(const Value: TStrings);
    procedure SetHeader(const Value: TStrings);
    procedure SetItem(const Value: TStrings);
    procedure SetDefaults;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const Filename:string);
    procedure SaveToFile(const Filename:string);

  published
    property Header: TStrings read FHeader write SetHeader;
    property Item: TStrings read FItem write SetItem;
    property Footer: TStrings read FFooter write SetFooter;
    property Title: string read FTitle write FTitle;
    property QImage: TFilename read FQImage write FQImage;
    property AImage: TFilename read FAImage write FAImage;
    property ImagePath: string read FImagePath write FImagePath;
    property Stylesheet: TFilename read FStylesheet write FStylesheet;
    property StylePath: string read FStylePath write FStylePath;
  end;

function FAQOptions: TFAQOptions;
function DoSaveReplace(const S:String):string;
function DoLoadReplace(const S:String):string;
function FlattenText(Strings:TStrings):string;

implementation
uses
  Forms, IniFiles;

var
  GlobalFAQOptions: TFAQOptions = nil;

function FAQOptions: TFAQOptions;
begin
  if GlobalFAQOptions = nil then
  begin
    GlobalFAQOptions := TFAQOptions.Create;
    GlobalFAQOptions.LoadFromFile(ChangeFileExt(Application.Exename, '.ini'));
  end;
  Result := GlobalFAQOptions;
end;

function DoSaveReplace(const S:String):string;
begin
  Result := StringReplace(S,#13#10,'{CRLF}',[rfReplaceAll, rfIgnoreCase]);
end;

function DoLoadReplace(const S:String):string;
begin
  Result := StringReplace(S,'{CRLF}',#13#10,[rfReplaceAll, rfIgnoreCase]);
end;

function FlattenText(Strings:TStrings):string;
var i:integer;
begin
  Result := '';
  for i := 0 to Strings.Count - 1 do
    Result := Result + Strings[i] + ' ';
  while (Length(Result) > 0) and (AnsiLastChar(Result) <= #32) do  // remove spaces at end
    SetLength(Result,Length(Result)-1);
end;

{ TFAQOptions }

constructor TFAQOptions.Create;
begin
  inherited Create;
  FHeader := TStringlist.Create;
  FItem := TStringlist.Create;
  FFooter := TStringlist.Create;
  SetDefaults;
end;

destructor TFAQOptions.Destroy;
begin
  if FFilename <> '' then
    SaveToFile(FFilename);
  FHeader.Free;
  FItem.Free;
  FFooter.Free;
  inherited;
end;

procedure TFAQOptions.LoadFromFile(const Filename: string);
begin
  with TIniFile.Create(Filename) do
  try
    if ValueExists('Settings', 'Header') then
      Header.Text := DoLoadReplace(ReadString('Settings','Header',''));
    if ValueExists('Settings', 'Item') then
      Item.Text := DoLoadReplace(ReadString('Settings','Item',''));
    if ValueExists('Settings', 'Footer') then
      Footer.Text := DoLoadReplace(ReadString('Settings','Footer',''));
    Title := ReadString('Settings','Title',Title);
    QImage := ReadString('Settings','QImage',QImage);
    AImage := ReadString('Settings','AImage',AImage);
    ImagePath := ReadString('Settings','ImagePath',ImagePath);
    Stylesheet := ReadString('Settings','Stylesheet',Stylesheet);
    StylePath := ReadString('Settings','StylePath',StylePath);
  finally
    Free;
  end;
  FFilename := Filename;
end;

procedure TFAQOptions.SaveToFile(const Filename: string);
begin
  with TIniFile.Create(Filename) do
  try
    WriteString('Settings','Header',DoSaveReplace(Header.Text));
    WriteString('Settings','Item',DoSaveReplace(Item.Text));
    WriteString('Settings','Footer',DoSaveReplace(Footer.Text));
    WriteString('Settings','Title',Title);
    WriteString('Settings','QImage',QImage);
    WriteString('Settings','AImage',AImage);
    WriteString('Settings','ImagePath',ImagePath);
    WriteString('Settings','Stylesheet',Stylesheet);
    WriteString('Settings','StylePath',StylePath);
  finally
    Free;
  end;
  FFilename := Filename;
end;

procedure TFAQOptions.SetDefaults;
begin
  Header.Add('<html>');
  Header.Add('<head>');
  Header.Add('<title><#TITLE></title>');
  Header.Add('<link rel="stylesheet" type="text/css" href="<#STYLESHEET>">');
  Header.Add('<style>');
  Header.Add('div a { text-decoration:underline; font-weight: normal;}');
  Header.Add('a { text-decoration:none; font-weight: bold;}');
  Header.Add('a:link {color: #336699;}');
  Header.Add('a:hover {color: #336699;}');
  Header.Add('a:visited { color: #336699;}');
  Header.Add('</style>');
  Header.Add('');
  Header.Add('<script language="JavaScript" type="text/javascript">');
  Header.Add('<!--');
  Header.Add('var lastobj = null;');
  Header.Add('var allvisible = false;');
  Header.Add('function ToggleAll() {');
  Header.Add('var i = 0');
  Header.Add('var obj;');
  Header.Add('  do {');
  Header.Add('     obj = document.getElementById(''FAQ'' + i);');
  Header.Add('     if (obj != null) {');
  Header.Add('       obj.style.display = allvisible ? ''none'':''block'';');
  Header.Add('       i += 1;');
  Header.Add('     }');
  Header.Add('   } while(obj != null);');
  Header.Add('');
  Header.Add('  allvisible = !allvisible;');
  Header.Add('}');
  Header.Add('');
  Header.Add('function ToggleVisible(objID) {');
  Header.Add('');
  Header.Add('  var obj = document.getElementById(objId);');
  Header.Add('  if (obj == null) return;');
  Header.Add('  if ((lastobj) && (lastobj != obj)) {');
  Header.Add('    lastobj.style.display = "none";');
  Header.Add('  }');
  Header.Add('  obj.style.display = (obj.style.display == ''none'') ? ''block'': ''none'';');
  Header.Add('  lastobj = obj;');
  Header.Add('}');
  Header.Add('//-->');
  Header.Add('</script>');
  Header.Add('</head>');
  Header.Add('<body>');
  Header.Add('<h1><#TITLE></h1>');
  Header.Add('<a style="text-decoration:none;" href="javascript:void(1)" onClick="ToggleAll()">');
  Header.Add('<div style="font-family: Verdana; font-size:90%;text-decoration:none;font-weight:bold;" align=right>Expand/Collapse</div></a>');

  Item.Add('<img src="<#Q_IMAGE>" border=0>&nbsp;<a href="javascript:void(1)" onClick="ToggleVisible(''FAQ<#FAQINDEX>'');"><#Q_ITEM></a><br>');
  Item.Add('<div id="FAQ<#FAQINDEX>" style="color: #334499;display:none;margin-left:25px; margin-top: 5px; margin-bottom:10px;">');
  Item.Add('<img src="<#A_IMAGE>" border=0>&nbsp;<#A_ITEM></div>');

  Footer.Add('</body>');
  Footer.Add('</html>');

  QImage := 'q.gif';
  AImage := 'a.gif';
  ImagePath := 'images/';
  StyleSheet := 'default.css';
  StylePath := 'styles/';
  Title := 'Frequently Asked Questions';

end;

procedure TFAQOptions.SetFooter(const Value: TStrings);
begin
  FFooter.Assign(Value);
end;

procedure TFAQOptions.SetHeader(const Value: TStrings);
begin
  FHeader.Assign(Value);
end;

procedure TFAQOptions.SetItem(const Value: TStrings);
begin
  FItem.Assign(Value);
end;

initialization

finalization
  FreeAndNil(GlobalFAQOptions);
end.

