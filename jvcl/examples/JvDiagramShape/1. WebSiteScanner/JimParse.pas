{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

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

unit JimParse;

interface

uses
  SysUtils, Classes;


type
  TjimToken = class(TObject)
  private
    FTokenType : Integer;
    FAsString  : string;
  public
    property TokenType : Integer read FTokenType write FTokenType;
    property AsString  : string read FAsString write FAsString;
  end;


  TjimLexicalAnalyser = class(TObject)
  private
    FText     : string;
    FPosition : Integer;

    procedure SetText(const Value : string);
  public
    constructor Create;

    procedure GetNextToken(NextToken : TjimToken);
    property  Text : string read FText write SetText;
  end;


  TjimSymbolType = (stTitle,stBase,stLink,stImage);


  TjimSymbol = class(TCollectionItem)
  private
    FSymbolType  : TjimSymbolType;
    FSymbolValue : string;
  public
    procedure Assign(Source : TPersistent); override;

    property SymbolType  : TjimSymbolType read FSymbolType write FSymbolType;
    property SymbolValue : string read FSymbolValue write FSymbolValue;
  end;


  TjimSymbolTable = class(TCollection)
  private
    function  GetItem(Index : Integer) : TjimSymbol;
    procedure SetItem(Index : Integer;Value : TjimSymbol);
  public
    function Add : TjimSymbol;
    function AddSymbol(SymType : TjimSymbolType;SymValue : string) : TjimSymbol;

    property Items[Index : Integer] : TjimSymbol read GetItem write SetItem; default;
  end;


  TjimHtmlParser = class(TObject)
  private
    FLookahead   : TjimToken;
    FLexAnalyser : TjimLexicalAnalyser;
    FSymbolTable : TjimSymbolTable;
    FLastTag     : string;

    procedure Match(T : Integer);
    procedure ConsumeWhiteSpace;
    procedure Document;
    procedure Tag;
    procedure Data;
    procedure TagName;
    procedure AttributeList;
    function  AttributeName : string;
    function  Value : string;
    function  Identifier : string;
    function  QuotedValue : string;
    function  PlainValue : string;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Parse(const DocString : string);

    property SymbolTable : TjimSymbolTable read FSymbolTable;
  end;


  EjimHtmlParserError = class(Exception);


implementation

const
  // Token types are the characters 0 to 255, along with the following
  ttEndOfDoc = -1;



// --------------------------- TjimLexicalAnalyser ---------------------------

constructor TjimLexicalAnalyser.Create;
begin {Create}
  FText     := '';
  FPosition := 0;
end;  {Create}


procedure TjimLexicalAnalyser.SetText(const Value : string);
begin {SetText}
  if FText = Value then begin
    // Only proceed if setting a new string
    Exit;
  end;

  FPosition := 1;
  FText     := Value;
end;  {SetText}


procedure TjimLexicalAnalyser.GetNextToken(NextToken : TjimToken);
begin {GetNextToken}

  // Read the next character
  if FPosition > Length(FText) then begin
    // At the end of the document
    NextToken.AsString  := #0;
    NextToken.TokenType := ttEndOfDoc;
  end else begin
    // Return the character
    NextToken.AsString  := FText[FPosition];
    NextToken.TokenType := Integer(FText[FPosition]);
    Inc(FPosition);
  end;
end;  {GetNextToken}


// ------------------------------- TjimSymbol -------------------------------

procedure TjimSymbol.Assign(Source : TPersistent);
begin {Assign}
  if Source is TjimSymbol then begin
    SymbolType  := TjimSymbol(Source).SymbolType;
    SymbolValue := TjimSymbol(Source).SymbolValue;
    Exit;
  end;

  inherited Assign(Source);
end;  {Assign}


// ------------------------------ TjimSymbolTable ----------------------------

function TjimSymbolTable.GetItem(Index : Integer) : TjimSymbol;
begin {GetItem}
  Result := TjimSymbol(inherited GetItem(Index));
end;  {GetItem}


procedure TjimSymbolTable.SetItem(Index : Integer;Value : TjimSymbol);
begin {SetItem}
  inherited SetItem(Index,Value);
end;  {SetItem}


function TjimSymbolTable.Add : TjimSymbol;
begin {Add}
  Result := TjimSymbol(inherited Add);
end;  {Add}


function TjimSymbolTable.AddSymbol(SymType : TjimSymbolType;SymValue : string) : TjimSymbol;
  var
    i : Integer;
begin {AddSymbol}
  Result := nil;
  
  // Check whether symbol is already in the list
  for i := 0 to Count - 1 do begin
    if (Items[i].SymbolType = SymType) and (Items[i].SymbolValue = SymValue) then begin
      Exit;
    end;
  end;

  Result := Add;
  Result.SymbolType  := SymType;
  result.SymbolValue := SymValue;
end;  {AddSymbol}


// ------------------------------ TjimHtmlParser -----------------------------

procedure TjimHtmlParser.ConsumeWhiteSpace;
  // Eats 'whitespace' ie chars 0 to 32 inclusive. Here instead of lexical
  // analyser because white space is allowed sometimes.
begin {ConsumeWhiteSpace}
  while (FLookahead.TokenType <> ttEndOfDoc) and
        (FLookAhead.AsString <= ' ') do begin
    FLexAnalyser.GetNextToken(FLookAhead);
  end;
end;  {ConsumeWhiteSpace}


procedure TjimHtmlParser.Match(T : Integer);
  // If the token type T matches the FLookahead token type then FLookAhead is
  // set to the next token, otherwise an exception is raised
begin {Match}
  if FLookahead.TokenType = T then begin
    FLexAnalyser.GetNextToken(FLookahead);
  end else begin
    raise EjimHtmlParserError.Create('HTML syntax error. Expected ' +
                                     IntToStr(FLookahead.TokenType));
  end;
end;  {Match}


procedure TjimHtmlParser.Document;
begin {Document}
  while FLookahead.TokenType <> ttEndOfDoc do begin
    ConsumeWhiteSpace;

    if FLookahead.AsString = '<' then begin
      Tag;
    end else begin
      Data;
    end;
  end;

  Match(ttEndOfDoc);
end;  {Document}


procedure TjimHtmlParser.Tag;
begin {Tag}
  Match(Ord('<'));
  ConsumeWhiteSpace;

  if FLookahead.AsString = '/' then begin
    // Finding an end tag
    Match(Ord('/'));
    FLastTag := '/';
    ConsumeWhiteSpace;
    TagName;
  end else begin
    // Finding a start tag, or a tag that doesn't enclose anything
    FLastTag := '';
    ConsumeWhiteSpace;
    TagName;
    ConsumeWhiteSpace;
    AttributeList;
  end;

  Match(Ord('>'));
end;  {Tag}


procedure TjimHtmlParser.Data;
  var
    TitleStr : string;
begin {Data}
  TitleStr := '';

  while (FLookahead.AsString <> '<') and
        (FLookahead.TokenType <> ttEndOfDoc) do begin
    // Collect the title string. It is ok to search like this because no other
    // tags are allowed in a title
    if CompareText(FLastTag,'Title') = 0 then begin
      TitleStr := TitleStr + FLookahead.AsString;
    end;

    Match(FLookahead.TokenType);
  end;

  if TitleStr > '' then begin
    FSymbolTable.AddSymbol(stTitle,TitleStr);
  end;
end;  {Data}


procedure TjimHtmlParser.TagName;
begin {TagName}
  FLastTag := FLastTag + Identifier;

  if FLastTag = '!--' then begin
    // In a comment tag. Treat this specially by ignoring all characters
    // until the end of the comment tag
    repeat
      if FLookahead.AsString = '-' then begin
        FLastTag := FLastTag + FLookahead.AsString;
      end else begin
        FLastTag := '';
      end;

      Match(FLookahead.TokenType);
    until FLastTag = '--';
  end else if CompareText(FLastTag,'META') = 0 then begin
    // In a META tag. There is all sorts of rubbish here, so consume it all
    // until the end of the tag
    while FLookahead.AsString <> '>' do begin
      Match(FLookahead.TokenType);
    end;
  end;
end;  {TagName}


procedure TjimHtmlParser.AttributeList;
  var
    FLastAttribute : string;
    FLastValue     : string;
begin {AttributeList}
  while FLookahead.AsString <> '>' do begin
    FLastAttribute := AttributeName;
    ConsumeWhiteSpace;

    if FLookahead.AsString = '=' then begin
      Match(Ord('='));
      ConsumeWhiteSpace;
      FLastValue := Value;
      ConsumeWhiteSpace;

      // Should only get here if FLastAttribute is not an empty string
      if (CompareText(FLastTag,'BASE') = 0) and
         (CompareText('HREF',FLastAttribute) = 0) then begin
        // Special case when found the HREF attribute of a BASE tag
        FSymbolTable.AddSymbol(stBase,FLastValue);
      end else if (CompareText(FLastTag,'IMG') = 0) and
         (CompareText('SRC',FLastAttribute) = 0) then begin
        // Found an image
        FSymbolTable.AddSymbol(stImage,FLastValue);
      end else if ((CompareText(FLastTag,'A') = 0) or
                   (CompareText(FLastTag,'AREA') = 0) or
                   (CompareText(FLastTag,'LINK') = 0)) and
                  (CompareText('HREF',FLastAttribute) = 0) then begin
        // Found an ordinary link
        FSymbolTable.AddSymbol(stLink,FLastValue);
      end else if (CompareText(FLastTag,'FRAME') = 0) and
                  (CompareText('SRC',FLastAttribute) = 0) then begin
        // Found an ordinary link
        FSymbolTable.AddSymbol(stLink,FLastValue);
      end;
    end;
  end;
end;  {AttributeList}


function TjimHtmlParser.AttributeName : string;
begin {AttributeName}
  Result := '';

  if FLookahead.AsString = '"' then begin
    Result := QuotedValue;
  end else begin
    Result := Identifier;
  end;
end;  {AttributeName}


function TjimHtmlParser.Value : string;
begin {Value}
  Result := '';

  if FLookahead.AsString = '"' then begin
    Result := QuotedValue;
  end else begin
    Result := PlainValue;
  end;
end;  {Value}


function TjimHtmlParser.Identifier : string;
  const
      IdentifierSet = ['A'..'Z','a'..'z','0'..'9','-','!',':','/'];
begin {Identifier}
  Result := '';

  if (Length(FLookahead.AsString) >= 1) and
     (not (FLookahead.AsString[1] in IdentifierSet)) then begin
    raise EjimHtmlParserError.Create('HTML syntax error. Expected identifier, ' +
                                     'but got : ' + FLookahead.AsString +
                                     ' in tag ' + FLastTag);
  end;

  repeat
    Result := Result + FLookahead.AsString;

    if Result = '!--' then begin
      // Found a comment tag. Some people eg Microsoft, don't put a space after
      // this part of the tag
      Exit;
    end;

    Match(FLookahead.TokenType);
  until not (FLookahead.AsString[1] in IdentifierSet);
end;  {Identifier}


function TjimHtmlParser.QuotedValue : string;
begin {QuotedValue}
  Result := '';
  Match(Ord('"'));

  while FLookahead.AsString <> '"' do begin
    Result := Result + FLookahead.AsString;
    Match(FLookahead.TokenType);
  end;

  Match(Ord('"'));
end;  {QuotedValue}


function TjimHtmlParser.PlainValue : string;
  const
      PlainValueSet = ['A'..'Z','a'..'z','0'..'9','-','.','+','-',':','/','?',
                       ''''];
begin {PlainValue}
  Result := '';

  if (Length(FLookahead.AsString) >= 1) and
     (not (FLookahead.AsString[1] in PlainValueSet)) then begin
    raise EjimHtmlParserError.Create('HTML syntax error. Expected plain value, ' +
                                     'but got : ' + FLookahead.AsString +
                                     ' in tag ' + FLastTag);
  end;

  repeat
    Result := Result + FLookahead.AsString;
    Match(FLookahead.TokenType);
  until not (FLookahead.AsString[1] in PlainValueSet);
end;  {PlainValue}


constructor TjimHtmlParser.Create;
begin {Create}
  FLookahead   := TjimToken.Create;
  FLexAnalyser := TjimLexicalAnalyser.Create;
  FSymbolTable := TjimSymbolTable.Create(TjimSymbol);
end;  {Create}


destructor TjimHtmlParser.Destroy;
begin {Destroy}
  FLookahead.Free;
  FLexAnalyser.Free;
  FSymbolTable.Free;
end;  {Destroy}


procedure TjimHtmlParser.Parse(const DocString : string);
begin {Parse}
  if DocString = '' then begin
    Exit;
  end;

  FLastTag          := '';
  FLexAnalyser.Text := DocString;
  FLexAnalyser.GetNextToken(FLookahead);
  Document;
end;  {Parse}


end.
