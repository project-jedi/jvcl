unit FileWrapper;

interface

uses FileItem;

type
  TFileWrapper = class
  private
    FStatus: string;
    FPackage: string;
    FItems: TFileItemList;
  protected
  public
    constructor Create(FileName : string);
    destructor Destroy; override;

    property Package : string read FPackage write FPackage;
    property Status : string read FStatus write FStatus;
    property Items : TFileItemList read FItems;
  end;

implementation

uses Classes, SysUtils, JclStrings;

{ TFileWrapper }

constructor TFileWrapper.Create(FileName: string);
var
  dtx : TFileStream;
  sstr : TStringStream;
  slist : TStringList;
  curLine : string;
  curLineIndex : Integer;
begin
  inherited Create;

  FItems := TFileItemList.Create; 

  // read the given file
  sstr := TStringStream.Create('');
  slist := TStringList.Create;

  try
    dtx := TFileStream.Create(FileName, fmOpenRead);
    sstr.CopyFrom(dtx, dtx.Size);
    dtx.Free;
    slist.Text := sstr.DataString;
  finally
    sstr.Free;
  end;

  // now that it's in a string list, it's easier to read
  // line by line
  curLineIndex := 0;
  while curLineIndex < slist.Count do
  begin
    curLine := trim(slist[curLineIndex]);

    if StrHasPrefix(curLine, ['##Package:']) then
      FPackage := Copy(curLine, Pos(':', curLine)+2, Length(curLine));

    if StrHasPrefix(curLine, ['##Status:']) then
      FStatus := Copy(curLine, Pos(':', curLine)+2, Length(curLine));

    if StrHasPrefix(curLine, ['@@']) then // found start of an item
      FItems.Add(TFileItem.Create(slist, curLineIndex));

    Inc(curLineIndex);
  end;

  slist.Free;
end;

destructor TFileWrapper.Destroy;
begin
  FItems.Free;
  
  inherited;
end;

end.
