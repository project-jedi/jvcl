{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSimpleXML.PAS, released on 2002-06-03

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Christophe Paris.
                Florent Ouchet (move from the JVCL to the JCL).

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues: This component does not parse the !DOCTYPE tags but preserves them
-----------------------------------------------------------------------------}
// $Id$

unit JvSimpleXml;

{$I jvcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, // Delphi 2005 inline
  {$ENDIF MSWINDOWS}
  SysUtils, Classes, Variants,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclSimpleXml, JclStreams;

type
  TJvOnSimpleXMLParsed = TJclOnSimpleXMLParsed;
  TJvOnValueParsed = TJclOnValueParsed;
  TJvOnSimpleProgress = TJclOnSimpleProgress;

  TJvSimpleXMLElem = TJclSimpleXMLElem;
  TJvSimpleXMLElemCData = TJclSimpleXMLElemCData;
  TJvSimpleXMLElemClass = TJclSimpleXMLElemClass;
  TJvSimpleXMLElemClassic = TJclSimpleXMLElemClassic;
  TJvSimpleXMLElemComment = TJclSimpleXMLElemComment;
  TJvSimpleXMLElemCompare = TJclSimpleXMLElemCompare;
  TJvSimpleXMLElemDocType = TJclSimpleXMLElemDocType;
  TJvSimpleXMLElemHeader = TJclSimpleXMLElemHeader;
  TJvSimpleXMLElemText = TJclSimpleXMLElemText;
  TJvSimpleXMLElems = TJclSimpleXMLElems;
  TJvSimpleXMLElemSheet = TJclSimpleXMLElemSheet;
  TJvSimpleXMLElemsProlog = TJclSimpleXMLElemsProlog;
  EJvSimpleXMLError = EJclSimpleXMLError;
  TJvSimpleXMLProp = TJclSimpleXMLProp;
  TJvSimpleXMLProps = TJclSimpleXMLProps;

  //Those hash stuffs are for future use only
  //Plans are to replace current hash by this mechanism
  TJvHashKind = TJclHashKind;

  TJvHashElem = TJclHashElem;
  PJvHashElem = PJclHashElem;
  TJvHashRecord = TJclHashRecord;
  PJvHashRecord = PJclHashRecord;
  TJvHashList = TJclHashList;
  PJvHashList = PJclHashList;

  TJvSimpleXMLOptions = TJclSimpleXMLOptions;

const
  sxoAutoCreate = JclSimpleXml.sxoAutoCreate;
  sxoAutoIndent = JclSimpleXml.sxoAutoIndent;
  sxoAutoEncodeValue = JclSimpleXml.sxoAutoEncodeValue;
  sxoAutoEncodeEntity = JclSimpleXml.sxoAutoEncodeEntity;
  sxoDoNotSaveProlog = JclSimpleXml.sxoDoNotSaveProlog;
  sxoTrimPrecedingTextWhitespace = JclSimpleXml.sxoTrimPrecedingTextWhitespace;

type
  TJvSimpleXMLEncodeEvent = TJclSimpleXMLEncodeEvent;
  TJvSimpleXMLEncodeStreamEvent = TJclSimpleXMLEncodeStreamEvent;

  // to have access to the protected methods
  TJclHackSimpleXML = class(TJclSimpleXML)
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvSimpleXML = class(TComponent)
  private
    FJclSimpleXML: TJclHackSimpleXML;
    function GetFileName: TFileName;
    function GetIndentString: string;
    function GetOnDecodeStream: TJvSimpleXMLEncodeStreamEvent;
    function GetOnDecodeValue: TJvSimpleXMLEncodeEvent;
    function GetOnEncodeStream: TJvSimpleXMLEncodeStreamEvent;
    function GetOnEncodeValue: TJvSimpleXMLEncodeEvent;
    function GetOnLoadProgress: TJvOnSimpleProgress;
    function GetOnSaveProgress: TJvOnSimpleProgress;
    function GetOnTagParsed: TJvOnSimpleXMLParsed;
    function GetOnValueParsed: TJvOnValueParsed;
    function GetOptions: TJvSimpleXMLOptions;
    function GetProlog: TJvSimpleXMLElemsProlog;
    function GetRoot: TJvSimpleXMLElemClassic;
    procedure SetOnDecodeStream(const Value: TJvSimpleXMLEncodeStreamEvent);
    procedure SetOnDecodeValue(const Value: TJvSimpleXMLEncodeEvent);
    procedure SetOnEncodeStream(const Value: TJvSimpleXMLEncodeStreamEvent);
    procedure SetOnEncodeValue(const Value: TJvSimpleXMLEncodeEvent);
    procedure SetOnLoadProgress(const Value: TJvOnSimpleProgress);
    procedure SetOnSaveProgress(const Value: TJvOnSimpleProgress);
    procedure SetOnTagParsed(const Value: TJvOnSimpleXMLParsed);
    procedure SetOnValueParsed(const Value: TJvOnValueParsed);
    procedure SetOptions(const Value: TJvSimpleXMLOptions);
    procedure SetProlog(const Value: TJvSimpleXMLElemsProlog);
  protected
    procedure SetIndentString(const Value: string);
    procedure SetRoot(const Value: TJclSimpleXMLElemClassic);
    procedure SetFileName(Value: TFileName);
    procedure DoLoadProgress(const APosition, ATotal: Integer);
    procedure DoSaveProgress;
    procedure DoTagParsed(const AName: string);
    procedure DoValueParsed(const AName, AValue: string);
    procedure DoEncodeValue(var Value: string); virtual;
    procedure DoDecodeValue(var Value: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromString(const Value: string);
    procedure LoadFromFile(const FileName: TFileName; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP);
    procedure LoadFromStream(Stream: TStream; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP);
    procedure LoadFromResourceName(Instance: THandle; const ResName: string);
    procedure SaveToFile(FileName: TFileName; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP);
    procedure SaveToStream(Stream: TStream; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP);
    function SaveToString: string;
    property Prolog: TJvSimpleXMLElemsProlog read GetProlog write SetProlog;
    property Root: TJvSimpleXMLElemClassic read GetRoot write SetRoot;
    property XMLData: string read SaveToString write LoadFromString;
  published
    property FileName: TFileName read GetFileName write SetFileName;
    property IndentString: string read GetIndentString write SetIndentString;
    property Options: TJvSimpleXMLOptions read GetOptions write SetOptions default [sxoAutoIndent, sxoAutoEncodeValue, sxoAutoEncodeEntity];
    property OnSaveProgress: TJvOnSimpleProgress read GetOnSaveProgress write SetOnSaveProgress;
    property OnLoadProgress: TJvOnSimpleProgress read GetOnLoadProgress write SetOnLoadProgress;
    property OnTagParsed: TJvOnSimpleXMLParsed read GetOnTagParsed write SetOnTagParsed;
    property OnValueParsed: TJvOnValueParsed read GetOnValueParsed write SetOnValueParsed;
    property OnEncodeValue: TJvSimpleXMLEncodeEvent read GetOnEncodeValue write SetOnEncodeValue;
    property OnDecodeValue: TJvSimpleXMLEncodeEvent read GetOnDecodeValue write SetOnDecodeValue;
    property OnEncodeStream: TJvSimpleXMLEncodeStreamEvent read GetOnEncodeStream write SetOnEncodeStream;
    property OnDecodeStream: TJvSimpleXMLEncodeStreamEvent read GetOnDecodeStream write SetOnDecodeStream;
  end;

type
  TXMLVariant = JclSimpleXml.TXMLVariant {$IFDEF COMPILER8_UP} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use JclSimpleXml.TXMLVariant' {$ENDIF} {$ENDIF COMPILER8_UP};

procedure XMLCreateInto(var ADest: Variant; const AXML: TJvSimpleXMLElem); deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use JclSimpleXml.XMLCreateInto' {$ENDIF};
function XMLCreate(const AXML: TJvSimpleXMLElem): Variant; overload; deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use JclSimpleXml.XMLCreate' {$ENDIF};
function XMLCreate: Variant; overload; deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use JclSimpleXml.XMLCreate' {$ENDIF};

function VarXML: TVarType; deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use JclSimpleXml.VarXML' {$ENDIF};

// Encodes a string into an internal format:
// any character <= #127 is preserved
// all other characters are converted to hex notation except
// for some special characters that are converted to XML entities
function SimpleXMLEncode(const S: string): string; {$IFDEF SUPPORTS_DEPRECATED} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use JclSimpleXml.SimpleXMLEncode' {$ENDIF} ; {$ENDIF}
// Decodes a string encoded with SimpleXMLEncode:
// any character <= #127 is preserved
// all other characters and substrings are converted from
// the special XML entities to characters or from hex to characters
// NB! Setting TrimBlanks to true will slow down the process considerably
procedure SimpleXMLDecode(var S: string; TrimBlanks: Boolean); {$IFDEF SUPPORTS_DEPRECATED} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use JclSimpleXml.SimpleXMLDecode' {$ENDIF} ; {$ENDIF}

function XMLEncode(const S: string): string; {$IFDEF SUPPORTS_DEPRECATED} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use JclSimpleXml.XMLEncode' {$ENDIF} ; {$ENDIF}
function XMLDecode(const S: string): string; {$IFDEF SUPPORTS_DEPRECATED} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use JclSimpleXml.XMLDecode' {$ENDIF} ; {$ENDIF}

// Encodes special characters (', ", <, > and &) into XML entities (@apos;, &quot;, &lt;, &gt; and &amp;)
function EntityEncode(const S: string): string; {$IFDEF SUPPORTS_DEPRECATED} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use JclSimpleXml.EntityEncode' {$ENDIF} ; {$ENDIF}
// Decodes XML entities (@apos;, &quot;, &lt;, &gt; and &amp;) into special characters (', ", <, > and &)
function EntityDecode(const S: string): string; {$IFDEF SUPPORTS_DEPRECATED} deprecated {$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'Use JclSimpleXml.EntityDecode' {$ENDIF} ; {$ENDIF}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

function EntityEncode(const S: string): string;
begin
  Result := JclSimpleXml.EntityEncode(S);
end;

function EntityDecode(const S: string): string;
begin
  Result := JclSimpleXml.EntityDecode(S);
end;

function SimpleXMLEncode(const S: string): string;
begin
  Result := JclSimpleXml.SimpleXMLEncode(S);
end;

procedure SimpleXMLDecode(var S: string; TrimBlanks: Boolean);
begin
  JclSimpleXml.SimpleXMLDecode(S, TrimBlanks);
end;

function XMLEncode(const S: string): string;
begin
  Result := JclSimpleXml.SimpleXMLEncode(S);
end;

function XMLDecode(const S: string): string;
begin
  Result := S;
  JclSimpleXml.SimpleXMLDecode(Result, False);
end;

function VarXML: TVarType;
begin
  Result := JclSimpleXml.VarXML;
end;

procedure XMLCreateInto(var ADest: Variant; const AXML: TJvSimpleXMLElem);
begin
  JclSimpleXml.XMLCreateInto(ADest, AXML);
end;

function XMLCreate(const AXML: TJvSimpleXMLElem): Variant;
begin
  Result := JclSimpleXml.XMLCreate(AXML);
end;

function XMLCreate: Variant;
begin
  Result := JclSimpleXml.XMLCreate;
end;

//=== { TJvSimpleXML } =======================================================

constructor TJvSimpleXML.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FJclSimpleXML := TJclHackSimpleXML.Create;
end;

destructor TJvSimpleXML.Destroy;
begin
  FJclSimpleXML.Free;
  inherited Destroy;
end;

procedure TJvSimpleXML.DoDecodeValue(var Value: string);
begin
  FJclSimpleXML.DoDecodeValue(Value);
end;

procedure TJvSimpleXML.DoEncodeValue(var Value: string);
begin
  FJclSimpleXML.DoEncodeValue(Value);
end;

procedure TJvSimpleXML.DoLoadProgress(const APosition, ATotal: Integer);
begin
  FJclSimpleXML.DoLoadProgress(APosition, ATotal);
end;

procedure TJvSimpleXML.DoSaveProgress;
begin
  FJclSimpleXML.DoSaveProgress;
end;

procedure TJvSimpleXML.DoTagParsed(const AName: string);
begin
  FJclSimpleXML.DoTagParsed(AName);
end;

procedure TJvSimpleXML.DoValueParsed(const AName, AValue: string);
begin
  FJclSimpleXML.DoValueParsed(AName, AValue);
end;

function TJvSimpleXML.GetFileName: TFileName;
begin
  Result := FJclSimpleXML.FileName;
end;

function TJvSimpleXML.GetIndentString: string;
begin
  Result := FJclSimpleXML.IndentString;
end;

function TJvSimpleXML.GetOnDecodeStream: TJvSimpleXMLEncodeStreamEvent;
begin
  Result := FJclSimpleXML.OnDecodeStream;
end;

function TJvSimpleXML.GetOnDecodeValue: TJvSimpleXMLEncodeEvent;
begin
  Result := FJclSimpleXML.OnDecodeValue;
end;

function TJvSimpleXML.GetOnEncodeStream: TJvSimpleXMLEncodeStreamEvent;
begin
  Result := FJclSimpleXML.OnDecodeStream;
end;

function TJvSimpleXML.GetOnEncodeValue: TJvSimpleXMLEncodeEvent;
begin
  Result := FJclSimpleXML.OnEncodeValue;
end;

function TJvSimpleXML.GetOnLoadProgress: TJvOnSimpleProgress;
begin
  Result := FJclSimpleXML.OnLoadProgress;
end;

function TJvSimpleXML.GetOnSaveProgress: TJvOnSimpleProgress;
begin
  Result := FJclSimpleXML.OnSaveProgress;
end;

function TJvSimpleXML.GetOnTagParsed: TJvOnSimpleXMLParsed;
begin
  Result := FJclSimpleXML.OnTagParsed;
end;

function TJvSimpleXML.GetOnValueParsed: TJvOnValueParsed;
begin
  Result := FJclSimpleXML.OnValueParsed;
end;

function TJvSimpleXML.GetOptions: TJvSimpleXMLOptions;
begin
  Result := FJclSimpleXML.Options;
end;

function TJvSimpleXML.GetProlog: TJvSimpleXMLElemsProlog;
begin
  Result := FJclSimpleXML.Prolog;
end;

function TJvSimpleXML.GetRoot: TJvSimpleXMLElemClassic;
begin
  Result := FJclSimpleXML.Root;
end;

procedure TJvSimpleXML.LoadFromFile(const FileName: TFileName; Encoding: TJclStringEncoding = seAuto; CodePage: Word =
    CP_ACP);
begin
  FJclSimpleXML.LoadFromFile(FileName, Encoding, CodePage);
end;

procedure TJvSimpleXML.LoadFromResourceName(Instance: THandle;
  const ResName: string);
begin
  FJclSimpleXML.LoadFromResourceName(Instance, ResName);
end;

procedure TJvSimpleXML.LoadFromStream(Stream: TStream; Encoding: TJclStringEncoding = seAuto; CodePage: Word = CP_ACP);
begin
  FJclSimpleXML.LoadFromStream(Stream, Encoding, CodePage);
end;

procedure TJvSimpleXML.LoadFromString(const Value: string);
begin
  FJclSimpleXML.LoadFromString(Value);
end;

procedure TJvSimpleXML.SaveToFile(FileName: TFileName; Encoding: TJclStringEncoding; CodePage: Word);
begin
  FJclSimpleXML.SaveToFile(FileName, Encoding, CodePage);
end;

procedure TJvSimpleXML.SaveToStream(Stream: TStream; Encoding: TJclStringEncoding; CodePage: Word);
begin
  FJclSimpleXML.SaveToStream(Stream, Encoding, CodePage);
end;

function TJvSimpleXML.SaveToString: string;
begin
  Result := FJclSimpleXML.SaveToString;
end;

procedure TJvSimpleXML.SetFileName(Value: TFileName);
begin
  FJclSimpleXML.FileName := Value;
end;

procedure TJvSimpleXML.SetIndentString(const Value: string);
begin
  FJclSimpleXML.IndentString := Value;
end;

procedure TJvSimpleXML.SetOnDecodeStream(
  const Value: TJvSimpleXMLEncodeStreamEvent);
begin
  FJclSimpleXML.OnDecodeStream := Value;
end;

procedure TJvSimpleXML.SetOnDecodeValue(const Value: TJvSimpleXMLEncodeEvent);
begin
  FJclSimpleXML.OnDecodeValue := Value;
end;

procedure TJvSimpleXML.SetOnEncodeStream(
  const Value: TJvSimpleXMLEncodeStreamEvent);
begin
  FJclSimpleXML.OnEncodeStream := Value;
end;

procedure TJvSimpleXML.SetOnEncodeValue(const Value: TJvSimpleXMLEncodeEvent);
begin
  FJclSimpleXML.OnEncodeValue := Value;
end;

procedure TJvSimpleXML.SetOnLoadProgress(const Value: TJvOnSimpleProgress);
begin
  FJclSimpleXML.OnLoadProgress := Value;
end;

procedure TJvSimpleXML.SetOnSaveProgress(const Value: TJvOnSimpleProgress);
begin
  FJclSimpleXML.OnSaveProgress := Value;
end;

procedure TJvSimpleXML.SetOnTagParsed(const Value: TJvOnSimpleXMLParsed);
begin
  FJclSimpleXML.OnTagParsed := Value;
end;

procedure TJvSimpleXML.SetOnValueParsed(const Value: TJvOnValueParsed);
begin
  FJclSimpleXML.OnValueParsed := Value;
end;

procedure TJvSimpleXML.SetOptions(const Value: TJvSimpleXMLOptions);
begin
  FJclSimpleXML.Options := Value;
end;

procedure TJvSimpleXML.SetProlog(const Value: TJvSimpleXMLElemsProlog);
begin
  FJclSimpleXML.Prolog := Value;
end;

procedure TJvSimpleXML.SetRoot(const Value: TJclSimpleXMLElemClassic);
begin
  FJclSimpleXML.Root := Value;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
