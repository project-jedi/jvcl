unit testClasses;

interface
uses Classes;

type
  TDocuments = class;
  Document = class;

  TCatalogue = class(TPersistent)
  private
    FDocuments: TDocuments;
    FFooter: string;
    FHeader: string;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property Header: string read FHeader write FHeader;
    property Documents: TDocuments read FDocuments write FDocuments;
    property Footer: string read FFooter write FFooter;
  end;

  Document = class(TCollectionItem)
  private
    FDocIndex: integer;
    FTitle: string;
    FAuthor: string;
    FISBN: string;
    FPublisher: string;
    FPublicDate: string;
    FAdditionalInformation: string;
    FVersion: string;
  published
    property DocIndex: integer read FDocIndex write FDocIndex;
    property ISBN: string read FISBN write FISBN;
    property Author: string read FAuthor write FAuthor;
    property Title: string read FTitle write FTitle;
    property Publisher: string read FPublisher write FPublisher;
    property Version: string read FVersion write FVersion;
    property PublicDate: string read FPublicDate write FPublicDate;
    property AdditionalInformation: string read FAdditionalInformation write FAdditionalInformation;
  end;

  TDocuments = class(TCollection)
  private
    function GetItem(Index: Integer): Document;
    procedure SetItem(Index: Integer; const Value: Document);
  public
    function  Add: Document;
    function Insert(Index: Integer): Document;
    property Items[Index: Integer]: Document read GetItem  write SetItem; default;
  end;

implementation

{ Document }

{ TDocuments }

function TDocuments.Add: Document;
begin
  Result := Document(inherited Add);
end;

function TDocuments.GetItem(Index: Integer): Document;
begin
  Result := Document(inherited Items[Index]);
end;

function TDocuments.Insert(Index: Integer): Document;
begin
  Result := Document(inherited Insert(Index));
end;

procedure TDocuments.SetItem(Index: Integer; const Value: Document);
begin
  Items[Index].Assign(Value);
end;

{ TCatalogue }

constructor TCatalogue.Create(AOwner: TComponent);
begin
  Documents := TDocuments.Create(Document);
end;

destructor TCatalogue.Destroy;
begin
  inherited;
  Documents.Free;
end;


end.
