{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit JvSurveyImpl;

interface

uses
  SysUtils, Classes,
  JvSurveyIntf, JvSimpleXML;

type
  EJvSurveyError = class(Exception);

  TJvSurveyItem = class(TInterfacedObject, IUnknown, IJvSurveyItem)
  private
    FChoices: WideString;
    FResponses: WideString;
    FDescription: WideString;
    FComments:WideString;
    FID: integer;
    FRequired: WordBool;
    FSurveyType: TJvSurveyType;
    FTitle: WideString;
    function GetChoices: WideString;
    function GetDescription: WideString;
    function GetID: Integer;
    function GetRequired: WordBool;
    function GetResponses: WideString;
    function GetSurveyType: TJvSurveyType;
    function GetTitle: WideString;
    procedure SetDescription(const Value: WideString);
    procedure SetID(const Value: Integer);
    procedure SetRequired(const Value: WordBool);
    procedure SetSurveyType(const Value: TJvSurveyType);
    procedure SetTitle(const Value: WideString);
    procedure SetChoices(const Value: WideString);
    procedure SetResponses(const Value: WideString);
    function GetComments: WideString;
    procedure SetComments(const Value: WideString);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SortResponses;

    property ID: integer read GetID write SetID;
    property Title: WideString read GetTitle write SetTitle;
    property Description: WideString read GetDescription write SetDescription;
    property SurveyType: TJvSurveyType read GetSurveyType write SetSurveyType;
    property Choices: WideString read GetChoices write SetChoices;
    property Responses: WideString read GetResponses write SetResponses;
    property Required: WordBool read GetRequired write SetRequired;
    property Comments:WideString read GetComments write SetComments;

  end;

  TJvSurveyItems = class(TInterfacedObject, IUnknown, IJvSurveyItems)
  private
    FItems: TInterfaceList;
    function Add: IJvSurveyItem;
    procedure Delete(Index: Integer);
    procedure Clear;
    procedure Sort;
    function GetCount: Integer;
    function GetItem(Index: Integer): IJvSurveyItem;
  public
    constructor Create;
    destructor Destroy; override;

    property Items[Index: integer]: IJvSurveyItem read GetItem;
    property Count: integer read GetCount;
  end;

  TJvSurveyTaker = class(TInterfacedObject, IUnknown, IJvSurveyTaker)
  private
    FUserName: WideString;
    FMailAddress: WideString;
    FNotes: WideString;
    FID: WideString;
    function GetUserName: WideString;
    procedure SetUserName(const Value: WideString);
    function GetMailAddress: WideString;
    function GetNotes: WideString;
    procedure SetMailAddress(const Value: WideString);
    procedure SetNotes(const Value: WideString);
    function GetID: WideString;
    procedure SetID(const Value: WideString);
  public
    property ID: WideString read GetID write SetID;
    property UserName: WideString read GetUserName write SetUserName;
    property MailAddress: WideString read GetMailAddress write SetMailAddress;
    property Notes: WideString read GetNotes write SetNotes;
  end;

  TJvSurvey = class(TInterfacedObject, IUnknown, IJvSurvey)
  private
    FDescription: WideString;
    FID: integer;
    FItems: IJvSurveyItems;
    FRecipient: WideString;
    FTitle: WideString;
    FRecipientMail: WideString;
    FReleaseDate: TDateTime;
    FExpiryDate: TDateTime;
    FResultHREF: WideString;
    FSurveyTaker: IJvSurveyTaker;
    FFilename: string;
    FLastItem: IJvSurveyItem;
    function GetDescription: WideString;
    function GetID: Integer;
    function GetItems: IJvSurveyItems;
    function GetRecipient: WideString;
    function GetTitle: WideString;
    procedure SetDescription(const Value: WideString);
    procedure SetID(const Value: Integer);
    procedure SetRecipient(const Value: WideString);
    procedure SetTitle(const Value: WideString);
    function GetRecipientMail: WideString;
    procedure SetRecipientMail(const Value: WideString);
    function GetReleaseDate: TDateTime;
    procedure SetReleaseDate(const Value: TDateTime);
    function GetExpiryDate: TDateTime;
    procedure SetExpiryDate(const Value: TDateTime);
    function GetResultHREF: WideString;
    procedure SetResultHREF(const Value: WideString);
    function GetSurveyTaker: IJvSurveyTaker;
    procedure ParseXML(Node: TJvSimpleXmlElem);
    function IsCompressedStream(Stream: TStream): boolean;
    procedure DecompressStream(Source, Dest: TStream);
    procedure CompressStream(Source, Dest: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream; Format: TJvSurveyFileFormat);
    procedure LoadFromFile(const Filename: WideString);
    procedure SaveToFile(const Filename: WideString; Format: TJvSurveyFileFormat);
    property ID: integer read GetID write SetID;
    property Title: WideString read GetTitle write SetTitle;
    property Description: WideString read GetDescription write SetDescription;
    property Items: IJvSurveyItems read GetItems;
    property Recipient: WideString read GetRecipient write SetRecipient;
    property RecipientMail: WideString read GetRecipientMail write SetRecipientMail;
    property ResultHRef: WideString read GetResultHREF write SetResultHREF;
    property ReleaseDate: TDateTime read GetReleaseDate write SetReleaseDate;
    property ExpiryDate: TDateTime read GetExpiryDate write SetExpiryDate;
    property SurveyTaker: IJvSurveyTaker read GetSurveyTaker;

    property Filename: string read FFilename;
  end;

implementation

uses
  ZLib,
  JclSysInfo, JvJVCLUtils, JvSurveyUtils;

resourcestring
  SErrUnknownFormatFmt = 'Unknown survey format in "%s"!';
  SErrUnsupportedVersionFmt = 'Unsupported version (%s)';
  SErrInvalidFileFormatFmt = 'Invalid survey file "%s"';

function InternalCreateSurvey: IJvSurvey;
begin
  Result := TJvSurvey.Create;
end;

{ TJvSurveyItem }

constructor TJvSurveyItem.Create;
begin
  inherited Create;
end;

destructor TJvSurveyItem.Destroy;
begin
  inherited;
end;

function TJvSurveyItem.GetChoices: WideString;
begin
  Result := FChoices;
end;

function TJvSurveyItem.GetDescription: WideString;
begin
  Result := FDescription;
end;

function TJvSurveyItem.GetID: Integer;
begin
  Result := FID;
end;

function TJvSurveyItem.GetRequired: WordBool;
begin
  Result := FRequired;
end;

function TJvSurveyItem.GetResponses: WideString;
begin
  Result := FResponses;
end;

function TJvSurveyItem.GetSurveyType: TJvSurveyType;
begin
  Result := FSurveyType;
end;

function TJvSurveyItem.GetTitle: WideString;
begin
  Result := FTitle;
end;

procedure TJvSurveyItem.SetChoices(const Value: WideString);
begin
  FChoices := Value;
end;

procedure TJvSurveyItem.SetDescription(const Value: WideString);
begin
  FDescription := Value;
end;

procedure TJvSurveyItem.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TJvSurveyItem.SetResponses(const Value: WideString);
begin
  Fresponses := Value;
end;

procedure TJvSurveyItem.SetRequired(const Value: WordBool);
begin
  FRequired := Value;
end;

procedure TJvSurveyItem.SetSurveyType(const Value: TJvSurveyType);
begin
  FSurveyType := Value;
end;

procedure TJvSurveyItem.SetTitle(const Value: WideString);
begin
  FTitle := Value;
end;

function InvertResponseSort(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := StrToIntDef(List[Index2], 0) - StrToIntDef(List[Index1], 0);
end;

procedure TJvSurveyItem.SortResponses;
var
  C, C2, R: TStringlist;
  i, j: integer;
begin
  if SurveyType = stFreeForm then Exit;
  // sort on responses, i.e change '0,0,1,2,0,4' into '4,2,1,0,0,0', choices are sorted accordingly
  // (p3) there must be a simpler way of doing this...
  C := TStringlist.Create;
  C2 := TStringlist.Create;
  R := TStringlist.Create;
  try
    C.Text := DecodeChoice(Choices, SurveyType);
    C2.Text := C.Text;
    R.Text := DecodeResponse(Responses, SurveyType);
    while R.Count < C.Count do
      R.Add('0');
    while C.Count < R.Count do
      R.Delete(R.Count - 1);
    for i := 0 to R.Count - 1 do
      R.Objects[i] := TObject(i); // save old index
    R.CustomSort(InvertResponseSort);
    for i := 0 to R.Count - 1 do
    begin
      j := integer(R.Objects[i]);
      C2[i] := C[j]; // move items according to index
    end;
    Choices := EncodeChoice(C2.Text, Surveytype);
    Responses := EncodeResponse(R.Text, Surveytype);
  finally
    C.Free;
    C2.Free;
    R.Free;
  end;
end;

function TJvSurveyItem.GetComments: WideString;
begin
  Result := FComments;
end;

procedure TJvSurveyItem.SetComments(const Value: WideString);
begin
  FComments := Value;
end;

{ TJvSurveyItems }

function TJvSurveyItems.Add: IJvSurveyItem;
begin
  Result := TJvSurveyItem.Create;
  Result.ID := -1;
  FItems.Add(Result);
end;

procedure TJvSurveyItems.Clear;
begin
  FItems.Count := 0;
end;

constructor TJvSurveyItems.Create;
begin
  inherited Create;
  FItems := TInterfacelist.Create;
end;

procedure TJvSurveyItems.Delete(Index: Integer);
begin
  FItems.Delete(Index);
end;

destructor TJvSurveyItems.Destroy;
begin
  FItems := nil;
  inherited;
end;

function TJvSurveyItems.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJvSurveyItems.GetItem(Index: Integer): IJvSurveyItem;
begin
  Result := FItems[Index] as IJvSurveyItem;
end;

type
  TInterfaceListSortCompare = function(const Item1, Item2: IUnknown): integer;

procedure QuickSort(AList: TInterfaceList; L, R: Integer;
  SCompare: TInterfaceListSortCompare);
var
  I, J: Integer;
  P, T: IUnknown;
begin
  repeat
    I := L;
    J := R;
    P := AList[(L + R) shr 1];
    repeat
      while SCompare(AList[I], P) < 0 do
        Inc(I);
      while SCompare(AList[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := AList[I];
        AList[I] := AList[J];
        AList[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, SCompare);
    L := I;
  until I >= R;
end;

function IDCompare(const Item1, Item2: IUnknown): integer;
begin
  Result := (Item1 as IJvSurveyItem).ID - (Item2 as IJvSurveyItem).ID;
end;

procedure TJvSurveyItems.Sort;
begin
  if Count > 1 then
    QuickSort(FItems, 0, Count - 1, IDCompare);
end;

{ TJvSurveyTaker }

function TJvSurveyTaker.GetID: WideString;
begin
  Result := FID;
end;

function TJvSurveyTaker.GetMailAddress: WideString;
begin
  Result := FMailAddress;
end;

function TJvSurveyTaker.GetNotes: WideString;
begin
  Result := FNotes;
end;

function TJvSurveyTaker.GetUserName: WideString;
begin
  Result := FUserName;
end;

procedure TJvSurveyTaker.SetID(const Value: WideString);
begin
  FID := Value;
end;

procedure TJvSurveyTaker.SetMailAddress(const Value: WideString);
begin
  FMailAddress := Value;
end;

procedure TJvSurveyTaker.SetNotes(const Value: WideString);
begin
  FNotes := Value;
end;

procedure TJvSurveyTaker.SetUserName(const Value: WideString);
begin
  FUserName := Value;
end;

{ TJvSurvey }

constructor TJvSurvey.Create;
begin
  inherited;
  FItems := TJvSurveyItems.Create;
  FSurveyTaker := TJvSurveyTaker.Create;
end;

destructor TJvSurvey.Destroy;
begin
  FItems := nil;
  FSurveyTaker := nil;
  inherited;
end;

function TJvSurvey.GetDescription: WideString;
begin
  Result := FDescription;
end;

function TJvSurvey.GetExpiryDate: TDateTime;
begin
  Result := FExpiryDate;
end;

function TJvSurvey.GetID: Integer;
begin
  Result := FID;
end;

function TJvSurvey.GetItems: IJvSurveyItems;
begin
  Result := FItems;
end;

function TJvSurvey.GetRecipient: WideString;
begin
  Result := FRecipient;
end;

function TJvSurvey.GetRecipientMail: WideString;
begin
  Result := FRecipientMail;
end;

function TJvSurvey.GetReleaseDate: TDateTime;
begin
  Result := FReleaseDate;
end;

function TJvSurvey.GetResultHREF: WideString;
begin
  Result := FResultHREF;
end;

function TJvSurvey.GetTitle: WideString;
begin
  Result := FTitle;
end;

procedure TJvSurvey.LoadFromFile(const Filename: WideString);
var
  F: TFileStream;
begin
  FFilename := Filename;
  F := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TJvSurvey.ParseXML(Node: TJvSimpleXmlElem);
var
  i: integer;
  item: IJvSurveyItem;

begin
  if AnsiSameText(Node.Name, 'JEDISURVEY') then
  begin
    if Node.Properties.Value('Version', '') <> '1.0' then
      raise EJvSurveyError.CreateFmt(SErrUnsupportedVersionFmt, [Node.Properties.Value('Version', '')]);
  end
  else if AnsiSameText(Node.Name, 'SURVEY') then
  begin
    ID := Node.Properties.IntValue('ID', 0);
    Title := Node.Properties.Value('Title', '');
    ResultHREF := Node.Properties.Value('HREF', 'http://delphi-jedi.org');
    ReleaseDate := StrToDate(Node.Properties.Value('ReleaseDate', DateToStr(Date)));
    ExpiryDate := StrToDate(Node.Properties.Value('ExpiryDate', DateToStr(Date + 100)));
    Description := Node.Properties.Value('Description', '');
  end
  else if AnsiSameText(Node.Name, 'SURVEYTAKER') then
  begin
    SurveyTaker.ID := Node.Properties.Value('id', '');
    SurveyTaker.UserName := Node.Properties.Value('username', SurveyTaker.UserName);
    SurveyTaker.MailAddress := Node.Properties.Value('mailto', SurveyTaker.MailAddress);
    SurveyTaker.Notes := Node.Value;
  end
  else if AnsiSameText(Node.Name, 'RECIPIENT') then
  begin
    Recipient := Node.Properties.Value('username', '');
    RecipientMail := Node.Properties.Value('mailto', '');
    // TODO: recipient notes not used
  end
  else if AnsiSameText(Node.Name, 'ITEM') then
  begin
    item := Items.Add;
    item.ID := Node.Properties.IntValue('ID', Items.Count);
    item.Title := Node.Properties.Value('Title', '');
    item.Description := Node.Properties.Value('Description', '');
    item.SurveyType := DecodeType(Node.Properties.Value('Type', 'freeform'));
    item.Required := Node.Properties.BoolValue('Required', true);
    FLastItem := item;
  end
  else if AnsiSameText(Node.Name, 'CHOICES') then
  begin
    if FLastItem = nil then
      raise EJvSurveyError.CreateFmt(SErrInvalidFileFormatFmt, [Filename]);
    FLastItem.Choices := Node.Value;
  end
  else if AnsiSameText(Node.Name, 'RESPONSES') then
  begin
    if FLastItem = nil then
      raise EJvSurveyError.CreateFmt(SErrInvalidFileFormatFmt, [Filename]);
    FLastItem.Responses := Node.Value;
  end
  else if AnsiSameText(Node.Name, 'COMMENTS') then
  begin
    if FLastItem = nil then
      raise EJvSurveyError.CreateFmt(SErrInvalidFileFormatFmt, [Filename]);
    FLastItem.Comments := Node.Value;
  end;
  for i := 0 to Node.Items.Count - 1 do
    ParseXML(Node.Items[i]);
end;

function TJvSurvey.IsCompressedStream(Stream: TStream): boolean;
var
  buf: array[0..4] of char;
  Pos: Cardinal;
begin
  Pos := Stream.Read(buf[0], sizeof(buf));
  if Pos <> sizeof(buf) then
    raise Exception.Create('Invalid stream');
  Result := not AnsiSameText('<?xml', buf);
  Stream.Seek(-Pos, soFromCurrent);
end;

procedure CopyStream(Source,Dest:TStream);
var
  BufSize, N: Integer;
  Buffer: PChar;
begin
  BufSize := $F000;
  GetMem(Buffer, BufSize);
  try
    N := Source.Read(Buffer^,BufSize);
    while N = BufSize do
    begin
      Dest.Write(Buffer^,BufSize);
      N := Source.Read(Buffer^,BufSize);
    end;
    if N > 0 then
      Dest.Write(Buffer^,N);
  finally
    FreeMem(Buffer, BufSize);
  end;
end;

procedure TJvSurvey.DecompressStream(Source, Dest: TStream);
var
  ZStream: TDecompressionStream;
begin
  ZStream := TDecompressionStream.Create(Source);
  try
    CopyStream(ZStream,Dest); // decompress - doesn't work with Count = 0
  finally
    ZStream.Free;
  end;
end;

procedure TJvSurvey.CompressStream(Source, Dest: TStream);
var
  ZStream: TCompressionStream;
begin
  ZStream := TCompressionStream.Create(clMax, Dest);
  try
    ZStream.CopyFrom(Source, 0); // compress
  finally
    ZStream.Free;
  end;
  Dest.Seek(0, soFromBeginning);
end;

procedure TJvSurvey.LoadFromStream(Stream: TStream);
var
  X: TJvSimpleXML;
  AStream: TmemoryStream;
begin
  DecimalSeparator := '.';
  ShortDateFormat := 'YYYY-MM-DD';
  DateSeparator := '-';
  Items.Clear;
  AStream := TMemoryStream.Create;
  try
    if IsCompressedStream(Stream) then
    begin
      DecompressStream(Stream, AStream);
      AStream.Seek(0, soFromBeginning);
      Stream := AStream;
    end;
    X := TJvSimpleXML.Create(nil);
    try
      X.LoadFromStream(Stream);
      if not AnsiSameText(X.Root.Name, 'JEDISURVEY') then
        raise EJvSurveyError.CreateFmt(SErrUnknownFormatFmt, [Filename]);
      // set up defaults
      SurveyTaker.UserName := GetLocalUserName;
      SurveyTaker.MailAddress := Format('%s@%s.com', [GetLocalUserName, GetLocalComputerName]);
      ParseXML(X.Root);
    finally
      X.Free;
      GetFormatSettings;
    end;
    Items.Sort;
  finally
    AStream.Free;
  end;
end;

procedure TJvSurvey.SaveToFile(const Filename: WideString; Format: TJvSurveyFileFormat);
var
  F: TFileStream;
begin
  F := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(F, Format);
    FFilename := Filename;
  finally
    F.Free;
  end;
end;

procedure TJvSurvey.SaveToStream(Stream: TStream; Format: TJvSurveyFileFormat);
var
  X: TJvSimpleXML;
  item, item2: TJvSimpleXmlElem;
  i: integer;
  PrologStream: TStringStream;
  AStream: TMemoryStream;
begin
  DecimalSeparator := '.';
  ShortDateFormat := 'YYYY-MM-DD';
  DateSeparator := '-';
  // DONE: build XML doc
  X := TJvSimpleXML.Create(nil);
  try
    Items.Sort;
    // this is weird: does it really have to be this complicated?
    PrologStream := TStringStream.Create('<?xml version="1.0" stand-alone="yes" encoding="UTF-8" ?>');
    try
      PrologStream.Seek(0, soFromBeginning);
      X.Prolog.LoadFromStream(PrologStream);
    finally
      PrologStream.Free;
    end;
    X.Root.Name := 'JEDISURVEY';
    X.Root.Properties.Add('Version', '1.0');
    item := X.Root.Items.Add('SURVEY');
    item.Properties.Add('ID', ID);
    item.Properties.Add('Title', Title);
    item.Properties.Add('ReleaseDate', DateToStr(ReleaseDate));
    item.Properties.Add('ExpiryDate', DateToStr(ExpiryDate));
    item.Properties.Add('HREF', ResultHREF);
    item.Properties.Add('Description', Description);
    item := X.Root.Items.Add('RECIPIENT');
    item.Properties.Add('username', Recipient);
    item.Properties.Add('mailto', RecipientMail);
    item := X.Root.Items.Add('SURVEYTAKER');
    item.Properties.Add('username', SurveyTaker.UserName);
    item.Properties.Add('mailto', SurveyTaker.MailAddress);
    item.Properties.Add('id', SurveyTaker.ID);

    item := X.Root.Items.Add('ITEMS');
    for i := 0 to self.Items.Count - 1 do
    begin
      item2 := item.Items.Add('ITEM');
      item2.Properties.Add('ID', self.Items[i].ID);
      item2.Properties.Add('Title', self.Items[i].Title);
      item2.Properties.Add('Type', EncodeType(self.Items[i].SurveyType));
      item2.Properties.Add('Required', self.Items[i].Required);
      item2.Properties.Add('Description', self.Items[i].Description);
      with item2.Items.Add('CHOICES') do
        Value := EncodeChoice(self.Items[i].Choices, self.Items[i].SurveyType);
      with item2.Items.Add('RESPONSES') do
        Value := EncodeResponse(self.Items[i].Responses, self.Items[i].SurveyType);
      with item2.Items.Add('COMMENTS') do
        Value := EncodeResponse(self.Items[i].Comments,stFreeForm);
    end;
    X.SaveToStream(Stream);
    if Format = ffBinary then
    begin
      AStream := TMemoryStream.Create;
      try
        CompressStream(Stream, AStream);
        Stream.Size := 0;
        Stream.CopyFrom(AStream, 0);
      finally
        AStream.Free;
      end;
    end;
  finally
    X.Free;
    GetFormatSettings;
  end;
end;

procedure TJvSurvey.SetDescription(const Value: WideString);
begin
  FDescription := Value;
end;

procedure TJvSurvey.SetExpiryDate(const Value: TDateTime);
begin
  FExpiryDate := Value;
end;

procedure TJvSurvey.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TJvSurvey.SetRecipient(const Value: WideString);
begin
  FRecipient := Value;
end;

procedure TJvSurvey.SetRecipientMail(const Value: WideString);
begin
  FRecipientMail := Value;
end;

procedure TJvSurvey.SetReleaseDate(const Value: TDateTime);
begin
  FReleaseDate := Value;
end;

procedure TJvSurvey.SetResultHREF(const Value: WideString);
begin
  FResultHREF := Value;
end;

procedure TJvSurvey.SetTitle(const Value: WideString);
begin
  FTitle := Value;
end;

function TJvSurvey.GetSurveyTaker: IJvSurveyTaker;
begin
  Result := FSurveyTaker;
end;

initialization
  CreateSurvey := @InternalCreateSurvey;

end.

