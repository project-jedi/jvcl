unit WebDocumentIterator;

interface
uses windows, classes, sysutils, graphics, controls, menus, extctrls, glCommcl, glTypes, shdocvw;

type
{  TIterator = class
    procedure First; virtual; abstract;
    procedure Next; virtual; abstract;
    function IsDone: boolean; virtual; abstract;
  end;}

  TWebDocumentIterator = class{(TIterator)}
  private
    WebBrowser: TWebBrowser;
    Doc, Item: variant;
    ItemIndex: integer;
    DocLocationHref: string;
    FCurrentItem: TFileName;
  public
    property CurrentItem: TFileName read FCurrentItem;

    constructor Create(WebBrowser: TWebBrowser);
    procedure First;
    procedure Next;
    function IsDone: boolean;
  end;



implementation

{ TWebDocumentIterator }

constructor TWebDocumentIterator.Create(WebBrowser: TWebBrowser);
begin
  self.WebBrowser := WebBrowser;
  Doc := WebBrowser.Document;
  DocLocationHref := Doc.Location.Href;
  DocLocationHref := StringReplace(DocLocationHref, '#', ' ', [rfReplaceAll]);
  DocLocationHref := StringReplace(DocLocationHref, 'file:///', '', [rfReplaceAll, rfIgnoreCase]);
  DocLocationHref := StringReplace(DocLocationHref, '/', '\', [rfReplaceAll]);
end;

procedure TWebDocumentIterator.First;
begin
  ItemIndex := -1;
  Next;
end;

function TWebDocumentIterator.IsDone: boolean;
begin
  Result := ItemIndex > Doc.images.length + {Doc.all.length +} Doc.links.length;
end;

procedure TWebDocumentIterator.Next;
begin
  inc(ItemIndex);
  FCurrentItem := '';
  if IsDone then exit;

  try
  if ItemIndex <= Doc.images.length - 1 then
  begin
    Item := Doc.images.Item(ItemIndex);
    FCurrentItem := Item.src;
  end else
  if ItemIndex - Doc.images.length <= Doc.links.length - 1 then
  begin
    Item := Doc.links.Item(ItemIndex - Doc.images.length);
    FCurrentItem := Item.href;
  end else
  if ItemIndex - Doc.images.length - Doc.links.length <= Doc.all.length - 1 then
  begin
    Item := Doc.all.item(ItemIndex - Doc.images.length - Doc.links.length).style;
    FCurrentItem := Item.backgroundImage;
  end;
  except
   Next; exit;
  end;

  FCurrentItem := LowerCase(trim(FCurrentItem));
  if (FCurrentItem = '') and not IsDone then Next;

  if pos('#', FCurrentItem) > 0 then
    FCurrentItem := copy(FCurrentItem, 1, pos('#', FCurrentItem)-1);

  FCurrentItem := StringReplace(FCurrentItem, 'file:///', '', [rfReplaceAll, rfIgnoreCase]);
  FCurrentItem := StringReplace(FCurrentItem, '/', '\', [rfReplaceAll]);

  if DocLocationHref = FCurrentItem then
  begin
    Next;
    exit;
  end;

  if pos('http:\\', FCurrentItem) = 1 then
  begin
    Next;
    exit;
  end;

  if pos('mailto:', FCurrentItem) = 1 then
  begin
    Next;
    exit;
  end;

  FCurrentItem := StringReplace(FCurrentItem, 'url(', '', [rfReplaceAll]);
  FCurrentItem := StringReplace(FCurrentItem, ')', '', [rfReplaceAll]);
  FCurrentItem := StringReplace(FCurrentItem, '%20', ' ', [rfReplaceAll]);
end;


end.

