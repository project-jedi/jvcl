{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvID3v2Base.PAS, released on 2003-04-16.

The Initial Developer of the Original Code is Remko Bonte [remkobonte att myrealbox dott com]
Portions created by Remko Bonte are Copyright (C) 2003 Remko Bonte.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  * Encryption, compression not supported
  * Footer in v2.4 tags not supported
  * Some tags are not supported, see var DefaultFrameClasses. Values nil in that
    list indicate not supported frames.
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvID3v2Base;

interface

uses
  Classes, SysUtils,
  {$IFDEF VCL}
  JclUnicode,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  JvQWStrUtils,
  {$ENDIF VisualCLX}
  JvComponent, JvID3v2Types, JvID3v1;

const
  { Only v2.2, v2.3 and v2.4 are supported }
  CSupportedVersions = [ive2_2, ive2_3, ive2_4];

type
  EJvID3Error = class(Exception);

  TJvID3ActivateChangeEvent = procedure(Sender: TObject; Activated: Boolean) of object;

  TJvID3HandleError = (heAutoCorrect, heRaise, heBoolean);

  TJvMPEGLayer = (mlNotDefined, mlLayerIII, mlLayerII, mlLayerI);
  TJvMPEGVersion = (mvVersion25, mvReserved, mvVersion2, mvVersion1);
  TJvMPEGChannelMode = (mcStereo, mcJointStereo, mcDualChannel, mcSingleChannel);
  TJvMPEGBit = (mbProtection, mbPrivate, mbCopyrighted, mbOriginal);
  TJvMPEGBits = set of TJvMPEGBit;
  TJvMPEGEmphasis = (meNone, me5015ms, meReserved, meCCITJ17);
  TJvMPEGModeExtension = (meModeExt0, meModeExt1, meModeExt2, meModeExt3);

  TJvID3ControllerOption = (coAutoCorrect, coRemoveEmptyFrames);
  TJvID3ControllerOptions = set of TJvID3ControllerOption;

  TJvID3Event = (
    { Fired when the content of 1 or more frames in a tag changes }
    ideFrameChange,
    { Fired when the whole tag has changed, because of reading/writing }
    ideID3Change,
    { Fired when frames are added, deleted etc. }
    ideFrameListChange);

  TJvID3Controller = class;

  TJvID3Stream = class(TMemoryStream)
  private
    FReadingFrame: Boolean;
    FWritingFrame: Boolean;
    FSourceEncoding: TJvID3Encoding;
    FDestEncoding: TJvID3Encoding;

    FAllowedEncodings: TJvID3Encodings;

    FStartPosition: Integer;
    FCurrentFrameSize: Integer;
    procedure MoveToNextFrame;
    function GetBytesTillEndOfTag: Longint;
    function GetBytesTillEndOfFrame: Longint;
    procedure UpdateDestEncoding;
    procedure SetSourceEncoding(const Value: TJvID3Encoding);
  protected
    { ISO-8859-1 }
    function ReadStringA(var SA: string): Longint;
    function ReadUserStringA(var SA1, SA2: string): Longint;
    function WriteStringA(const SA: string): Longint;
    function WriteUserStringA(const SA1, SA2: string): Longint;
    function WriteTerminatorA: Longint;
    { UTF-16 & UTF-16BE }
    function ReadStringW(var SW: WideString): Longint;
    function ReadUserStringW(var SW1, SW2: WideString): Longint;
    function WriteStringW(const SW: WideString): Longint;
    function WriteUserStringW(const SW1, SW2: WideString): Longint;
    function WriteTerminatorW: Longint;
    { UTF-8 }
    function ReadStringUTF8(var SW: WideString): Longint;
    function ReadUserStringUTF8(var SW1, SW2: WideString): Longint;
    function WriteStringUTF8(const SW: WideString): Longint;
    function WriteUserStringUTF8(const SW1, SW2: WideString): Longint;
  public
    procedure BeginReadFrame(const AFrameSize: Integer);
    procedure BeginWriteFrame(const AFrameSize: Integer);

    procedure EndReadFrame;
    procedure EndWriteFrame;

    { Inits FAllowedEncodings depending on the wanted version and encoding }
    procedure InitAllowedEncodings(const AVersion: TJvID3Version;
      const AEncoding: TJvID3ForceEncoding);

    { Checks whether ACount bytes can be read }
    function CanRead(const ACount: Cardinal): Boolean;
    { Checks whether we are still in the frame }
    function InFrame(P: PChar): Boolean;

    { Read }
    function ReadDate(var ADate: TDateTime): Longint;
    function ReadLanguage(var Language: string): Longint;
    function ReadNumber(var AValue: Cardinal): Longint;
    function ReadEnc(var AEncoding: TJvID3Encoding): Longint;
    function ReadStringEnc(var S: TJvID3StringPair): Longint;
    function ReadUserString(var S1, S2: TJvID3StringPair): Longint;
    { Only for v2.2 }
    function ReadFixedNumber3(var AValue: Cardinal): Longint;
    { Only for v2.3 }
    function ReadFixedNumber(var AValue: Cardinal): Longint;
    { Only for v2.4 }
    function ReadSyncSafeInteger(var AInt: Cardinal): Longint; overload;
    function ReadSyncSafeInteger(var AInt: Cardinal; const ASize: Byte): Longint; overload;
    function ReadSyncSafeInteger(var AInt: Int64; const ASize: Byte = 4): Longint; overload;

    procedure ReadFromStream(AStream: TStream; const ASize: Integer);

    { Write }
    function WriteDate(const ADate: TDateTime): Longint;
    function WriteLanguage(const Language: string): Longint;
    function WriteNumber(AValue: Cardinal): Longint;
    function WriteEnc: Longint;
    function WritePadding(const Count: Longint): Longint;
    function WriteStringEnc(const S: TJvID3StringPair): Longint;
    function WriteUserString(const S1, S2: TJvID3StringPair): Longint;
    function WriteTerminatorEnc: Longint;
    { Only for v2.2 }
    function WriteFixedNumber3(AValue: Cardinal): Longint;
    { Only for v2.3 }
    function WriteFixedNumber(AValue: Cardinal): Longint;
    { Only for v2.4 }
    function WriteSyncSafeInteger(const AInt: Int64; const ASize: Byte = 4): Longint; overload;
    function WriteSyncSafeInteger(const AInt: Cardinal; const ASize: Byte): Longint; overload;
    function WriteSyncSafeInteger(const AInt: Cardinal): Longint; overload;

    property BytesTillEndOfFrame: Longint read GetBytesTillEndOfFrame;
    property BytesTillEndOfTag: Longint read GetBytesTillEndOfTag;

    { SourceEncoding =
        - When reading: encoding of the ID3 stream
        - When writing: encoding of current frame in the TJvID3Controller }
    property SourceEncoding: TJvID3Encoding read FSourceEncoding write SetSourceEncoding;
    { DestEncoding =
        - When reading: encoding of current frame in the TJvID3Controller
        - When writing: encoding of the ID3 stream }
    property DestEncoding: TJvID3Encoding read FDestEncoding;
    property AllowedEncodings: TJvID3Encodings read FAllowedEncodings;
  end;

  TJvID3Frame = class;
  TJvID3Frames = class;

  TJvID3FrameClass = class of TJvID3Frame;

  { Base component for TJvID3Header & TJvID3ExtendedHeader }
  TJvID3Base = class(TPersistent)
  private
    FController: TJvID3Controller;
    function GetStream: TJvID3Stream;
  protected
    procedure Read; virtual; abstract;
    procedure Write; virtual; abstract;
    procedure Reset; virtual; abstract;

    property Stream: TJvID3Stream read GetStream;
  public
    constructor Create(AController: TJvID3Controller); virtual;
    procedure AfterConstruction; override;
    procedure ChangeToVersion(const ANewVersion: TJvID3Version); virtual; abstract;
    procedure Assign(Source: TPersistent); override;
    property Controller: TJvID3Controller read FController;
  end;

  TJvID3Header = class(TJvID3Base)
  private
    FFlags: TJvID3HeaderFlags;
    FHasTag: Boolean;
    FMajorVersion: Byte;
    FRevisionNumber: Byte;
    FSize: Cardinal;
    procedure SetFlags(const Value: TJvID3HeaderFlags);
  protected
    procedure Read; override;
    procedure Write; override;
    procedure Reset; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure ChangeToVersion(const ANewVersion: TJvID3Version); override;
  published
    property MajorVersion: Byte read FMajorVersion;
    property RevisionNumber: Byte read FRevisionNumber;
    property HasTag: Boolean read FHasTag;
    property Flags: TJvID3HeaderFlags read FFlags write SetFlags;
    property Size: Cardinal read FSize;
  end;

  TJvID3ExtendedHeader = class(TJvID3Base)
  private
    FFlags: TJvID3HeaderExtendedFlags;
    FRestrictions: TJvID3Restrictions;
    FSizeOfPadding: Cardinal;
    FTotalFrameCRC: Cardinal;
    function GetSize: Cardinal;
    function GetSizeForVersion(const AVersion: TJvID3Version): Cardinal;
    procedure SetFlags(const Value: TJvID3HeaderExtendedFlags);
  protected
    procedure Read; override;
    procedure Write; override;
    procedure Reset; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure ChangeToVersion(const ANewVersion: TJvID3Version); override;
    property Size: Cardinal read GetSize;
  published
    property TotalFrameCRC: Cardinal read FTotalFrameCRC write FTotalFrameCRC;
    property SizeOfPadding: Cardinal read FSizeOfPadding;
    property Flags: TJvID3HeaderExtendedFlags read FFlags write SetFlags;
  end;

  { Base class for all frames }
  { TODO : Change to TPersistent? }
  TJvID3Frame = class(TComponent)
  private
    FController: TJvID3Controller;
    FFrames: TJvID3Frames;
    FFrameID: TJvID3FrameID;
    FFrameIDStr: string;
    FFrameSize: Cardinal;

    FDataLengthIndicator: Cardinal; { v2.4 }
    FDecompressedSize: Cardinal;
    FEncoding: TJvID3Encoding;
    FEncryptionID: Byte;
    FFlags: TJvID3FrameHeaderFlags;
    FGroupID: Byte;

    function GetFrameName: string;
    function GetFrameIDStrForVersion(const Version: TJvID3Version): string;
    function GetIndex: Integer;
    function GetStream: TJvID3Stream;
    procedure SetController(const AController: TJvID3Controller);
    procedure SetEncoding(const Value: TJvID3Encoding);
    procedure SetFlags(const Value: TJvID3FrameHeaderFlags);
    procedure SetFrameID(const Value: TJvID3FrameID);
    procedure SetFrameName(NewFrameName: string);
    procedure SetIndex(const Value: Integer);
  protected
    procedure Read;
    procedure Write;

    procedure ReadEncoding;
    procedure ReadFrame; virtual; abstract;
    procedure ReadFrameHeader;
    procedure WriteEncoding;
    procedure WriteFrame; virtual; abstract;
    procedure WriteFrameHeader(const AFrameSize: Cardinal);
    procedure WriteID;

    procedure ChangeToVersion(const ANewVersion: TJvID3Version); virtual;
    function SupportsVersion(const AVersion: TJvID3Version): Boolean; virtual;

    { Checks whether this frame is empty, thus can be removed }
    function GetIsEmpty: Boolean; virtual;

    { Checks whether there are no other frames with the same unique
      identifier as this frame }
    function CheckIsUnique: Boolean;

    procedure CheckFrameID(const AFrameID: TJvID3FrameID);
    procedure CheckFrameIDStr(const S: string);

    { Checks whether Frame has the same unique identifier as this frame }
    function SameUniqueIDAs(const Frame: TJvID3Frame): Boolean; virtual;

    function GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal; virtual; abstract;
    procedure UpdateFrameSize;

    procedure DataChanged;
    procedure Changed; virtual;

    procedure Error(const Msg: string);
    procedure ErrorFmt(const Msg: string; const Args: array of const);

    property Stream: TJvID3Stream read GetStream;
  public
    constructor Create(AOwner: TComponent; const AFrameID: TJvID3FrameID;
      const AFrameIDStr: string = ''); reintroduce; virtual;
    destructor Destroy; override;

    class function CanAddFrame(AController: TJvID3Controller; AFrameID: TJvID3FrameID): Boolean; virtual;
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; virtual;

    procedure Assign(Source: TPersistent); override;
    procedure Clear; virtual;
    property Controller: TJvID3Controller read FController write SetController stored False;
    property FrameSize: Cardinal read FFrameSize;

    property IsEmpty: Boolean read GetIsEmpty;
  published
    property Encoding: TJvID3Encoding read FEncoding write SetEncoding;
    property EncryptionID: Byte read FEncryptionID write FEncryptionID;
    property Flags: TJvID3FrameHeaderFlags read FFlags write SetFlags;
    property FrameID: TJvID3FrameID read FFrameID write SetFrameID;
    property FrameName: string read GetFrameName write SetFrameName;
    property GroupID: Byte read FGroupID write FGroupID;
    property Index: Integer read GetIndex write SetIndex stored False;
  end;

  TJvID3Frames = class(TJvID3Base)
  private
    FList: TList;
  protected
    procedure Changed;

    procedure CheckCanAddFrame(FrameID: TJvID3FrameID);

    procedure Read; override;
    procedure Write; override;
    procedure Reset; override;

    function GetCount: Integer;
    function GetFrame(Index: Integer): TJvID3Frame;
    procedure SetFrame(Index: Integer; Value: TJvID3Frame);
    procedure SetFrameIndex(Frame: TJvID3Frame; Value: Integer);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure Assign(Source: TPersistent); override;

    procedure Add(Frame: TJvID3Frame);
    procedure Clear;
    function FindFrame(const FrameName: string): TJvID3Frame; overload;
    function FindFrame(const FrameID: TJvID3FrameID): TJvID3Frame; overload;
    function FrameByName(const FrameName: string): TJvID3Frame;
    function FrameByID(const FrameID: TJvID3FrameID): TJvID3Frame;
    procedure GetFrameNames(List: TStrings);
    function GetFrameIDs: TJvID3FrameIDs;

    procedure ChangeToVersion(const ANewVersion: TJvID3Version); override;
    function IndexOf(Frame: TJvID3Frame): Integer;
    function CheckIsUnique(Frame: TJvID3Frame): Boolean;
    function CheckFrames(const HandleError: TJvID3HandleError): Boolean;
    procedure RemoveEmptyFrames;
    procedure Remove(Frame: TJvID3Frame);
    property Count: Integer read GetCount;
    property Frames[Index: Integer]: TJvID3Frame read GetFrame write SetFrame; default;
  end;

  { MCDI - fiCDID - Music CD identifier

    There may only be one 'MCDI' frame in each tag.
  }

  TJvID3BinaryFrame = class(TJvID3Frame)
  private
    FData: PChar;
    FDataSize: Cardinal;
  protected
    procedure ReadData(ASize: Cardinal); virtual;
    procedure WriteData; virtual;

    procedure ReadFrame; override;
    procedure WriteFrame; override;

    function GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal; override;
    function GetIsEmpty: Boolean; override;

    function SameUniqueIDAs(const Frame: TJvID3Frame): Boolean; override;
  public
    class function CanAddFrame(AController: TJvID3Controller; AFrameID: TJvID3FrameID): Boolean; override;
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    class function Find(AController: TJvID3Controller; const AFrameID: TJvID3FrameID): TJvID3BinaryFrame;
    class function FindOrCreate(AController: TJvID3Controller; const AFrameID: TJvID3FrameID): TJvID3BinaryFrame;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function SetData(P: Pointer; const Size: Cardinal): Boolean;
    function GetData(P: Pointer; const Size: Cardinal): Boolean;

    procedure LoadFromFile(const FileName: string); virtual;
    procedure SaveToFile(const FileName: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;

    property DataSize: Cardinal read FDataSize;
  end;

  TJvID3SkipFrame = class(TJvID3BinaryFrame)
  protected
    procedure ChangeToVersion(const ANewVersion: TJvID3Version); override;
  end;

  { IPLS - fiInvolvedPeople - Involved people list

    There may only be one "IPLS" frame in each tag.

    TIPL - fiInvolvedPeople2 - Involved people list
    TMCL - fiMusicianCreditList - Musician credits list

    There may only be one text information frame of its kind in an tag
  }

  TJvID3DoubleListFrame = class(TJvID3Frame)
  private
    FList: TStringList;
    FListW: TWideStringList;
    function GetNameA(Index: Integer): string;
    function GetNameW(Index: Integer): string;
    function GetValueA(Index: Integer): string;
    function GetValueW(Index: Integer): string;
    function GetList: TStrings;
    function GetListW: TWideStrings;
    procedure SetList(const Value: TStrings);
    procedure SetListW(const Value: TWideStrings);
    procedure ListChanged(Sender: TObject);
  protected
    procedure ReadFrame; override;
    procedure WriteFrame; override;

    function GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal; override;
    function GetIsEmpty: Boolean; override;

    procedure ChangeToVersion(const ANewVersion: TJvID3Version); override;
    function SupportsVersion(const AVersion: TJvID3Version): Boolean; override;
    function SameUniqueIDAs(const Frame: TJvID3Frame): Boolean; override;
  public
    class function CanAddFrame(AController: TJvID3Controller; AFrameID: TJvID3FrameID): Boolean; override;
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    class function Find(AController: TJvID3Controller; const AFrameID: TJvID3FrameID): TJvID3DoubleListFrame;
    class function FindOrCreate(AController: TJvID3Controller; const AFrameID: TJvID3FrameID): TJvID3DoubleListFrame;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Names[Index: Integer]: string read GetNameA;
    property NamesW[Index: Integer]: string read GetNameW;
    property Values[Index: Integer]: string read GetValueA;
    property ValuesW[Index: Integer]: string read GetValueW;
  published
    property List: TStrings read GetList write SetList;
    property ListW: TWideStrings read GetListW write SetListW;
  end;

  { COMM - fiComment - Comments

    There may be more than one comment frame in each tag, but only one with
    the same language and content descriptor.

    USLT - fiUnsyncedLyrics - Unsynchronized lyric/text transcription

    There may be more than one 'Unsynchronised lyrics/text transcription' frame
    in each tag, but only one with the same language and content descriptor.
  }

  TJvID3ContentFrame = class(TJvID3Frame)
  private
    FLanguage: string;
    FText: TJvID3StringPair;
    FDescription: TJvID3StringPair;
    procedure SetText(const Value: string);
    procedure SetTextW(const Value: WideString);
    procedure SetLanguage(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetDescriptionW(const Value: WideString);
  protected
    procedure ReadFrame; override;
    procedure WriteFrame; override;

    function GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal; override;
    function GetIsEmpty: Boolean; override;

    function SameUniqueIDAs(const Frame: TJvID3Frame): Boolean; override;
  public
    class function CanAddFrame(AController: TJvID3Controller; AFrameID: TJvID3FrameID): Boolean; override;
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    class function Find(AController: TJvID3Controller; const AFrameID: TJvID3FrameID): TJvID3ContentFrame;
    class function FindOrCreate(AController: TJvID3Controller; const AFrameID: TJvID3FrameID): TJvID3ContentFrame;
  published
    property Language: string read FLanguage write SetLanguage;
    property Description: string read FDescription.SA write SetDescription;
    property DescriptionW: WideString read FDescription.SW write SetDescriptionW;
    property Text: string read FText.SA write SetText;
    property TextW: WideString read FText.SW write SetTextW;
  end;

  { GEOB - fiGeneralObject - General encapsulated object

    There may be more than one "GEOB" frame in each tag, but only one with the
    same content descriptor
  }

  TJvID3GeneralObjFrame = class(TJvID3BinaryFrame)
  private
    FContentDescription: TJvID3StringPair;
    FMIMEType: string;
    FFileName: TJvID3StringPair;
    procedure SetContentDescription(const Value: string);
    procedure SetContentDescriptionW(const Value: WideString);
    procedure SetFileName(const Value: string);
    procedure SetFileNameW(const Value: WideString);
    procedure SetMIMEType(const Value: string);
  protected
    procedure ReadFrame; override;
    procedure WriteFrame; override;

    function GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal; override;
    function GetIsEmpty: Boolean; override;

    function SameUniqueIDAs(const Frame: TJvID3Frame): Boolean; override;
  public
    class function CanAddFrame(AController: TJvID3Controller; AFrameID: TJvID3FrameID): Boolean; override;
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    class function Find(AController: TJvID3Controller): TJvID3GeneralObjFrame; overload;
    class function Find(AController: TJvID3Controller; const AContentDescription: string): TJvID3GeneralObjFrame;
      overload;
    class function FindW(AController: TJvID3Controller; const AContentDescription: WideString): TJvID3GeneralObjFrame;
    class function FindOrCreate(AController: TJvID3Controller): TJvID3GeneralObjFrame; overload;
    class function FindOrCreate(AController: TJvID3Controller; const AContentDescription: string):
      TJvID3GeneralObjFrame; overload;
    class function FindOrCreateW(AController: TJvID3Controller; const AContentDescription: WideString):
      TJvID3GeneralObjFrame;
  published
    property MIMEType: string read FMIMEType write SetMIMEType;
    property FileName: string read FFileName.SA write SetFileName;
    property FileNameW: WideString read FFileName.SW write SetFileNameW;
    property ContentDescription: string read FContentDescription.SA write SetContentDescription;
    property ContentDescriptionW: WideString read FContentDescription.SW write SetContentDescriptionW;
  end;

  { POPM - fiPopularimeter - Popularimeter

    There may be more than one "POPM" frame in each tag, but only one with the
    same email address.
  }

  TJvID3PopularimeterFrame = class(TJvID3Frame)
  private
    FRating: Byte;
    FCounter: Cardinal;
    FEMailAddress: string;
    procedure SetCounter(const Value: Cardinal);
    procedure SetEMailAddress(const Value: string);
    procedure SetRating(const Value: Byte);
  protected
    procedure ReadFrame; override;
    procedure WriteFrame; override;

    function GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal; override;
    function GetIsEmpty: Boolean; override;

    function SameUniqueIDAs(const Frame: TJvID3Frame): Boolean; override;
  public
    class function CanAddFrame(AController: TJvID3Controller; AFrameID: TJvID3FrameID): Boolean; override;
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    class function Find(AController: TJvID3Controller): TJvID3PopularimeterFrame; overload;
    class function Find(AController: TJvID3Controller; const AEmailAddress: string): TJvID3PopularimeterFrame; overload;
    class function FindOrCreate(AController: TJvID3Controller): TJvID3PopularimeterFrame; overload;
    class function FindOrCreate(AController: TJvID3Controller; const AEmailAddress: string): TJvID3PopularimeterFrame; overload;
  published
    property EMailAddress: string read FEMailAddress write SetEMailAddress;
    property Rating: Byte read FRating write SetRating;
    property Counter: Cardinal read FCounter write SetCounter;
  end;

  { PCNT - fiPlayCounter - Play counter

    There may only be one "PCNT" frame in each tag.
  }

  TJvID3PlayCounterFrame = class(TJvID3Frame)
  private
    FCounter: Cardinal;
    procedure SetCounter(const Value: Cardinal);
  protected
    procedure ReadFrame; override;
    procedure WriteFrame; override;

    function GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal; override;
    function GetIsEmpty: Boolean; override;

    function SameUniqueIDAs(const Frame: TJvID3Frame): Boolean; override;
  public
    class function CanAddFrame(AController: TJvID3Controller; AFrameID: TJvID3FrameID): Boolean; override;
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    class function Find(AController: TJvID3Controller): TJvID3PlayCounterFrame;
    class function FindOrCreate(AController: TJvID3Controller): TJvID3PlayCounterFrame;
  published
    property Counter: Cardinal read FCounter write SetCounter;
  end;

  { AENC - fiAudioCrypto - Audio encryption

    There may be more than one "AENC" frames in a tag, but only one with
    the same 'Owner identifier'.
  }

  TJvID3AudioEncryptionFrame = class(TJvID3BinaryFrame)
  private
    FOwnerID: string;
    FPreviewStart: Word;
    FPreviewLength: Word;
    procedure SetOwnerID(const Value: string);
    procedure SetPreviewLength(const Value: Word);
    procedure SetPreviewStart(const Value: Word);
  protected
    procedure ReadFrame; override;
    procedure WriteFrame; override;

    function GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal; override;
    function GetIsEmpty: Boolean; override;
    function SameUniqueIDAs(const Frame: TJvID3Frame): Boolean; override;
  public
    class function CanAddFrame(AController: TJvID3Controller; AFrameID: TJvID3FrameID): Boolean; override;
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    class function Find(AController: TJvID3Controller; const AOwnerID: string): TJvID3AudioEncryptionFrame;
    class function FindOrCreate(AController: TJvID3Controller; const AOwnerID: string): TJvID3AudioEncryptionFrame;
  published
    property OwnerID: string read FOwnerID write SetOwnerID;
    property PreviewStart: Word read FPreviewStart write SetPreviewStart;
    property PreviewLength: Word read FPreviewLength write SetPreviewLength;
  end;

  { USER - fiTermsOfUse - Terms of use

    There may only be one "USER" frame in a tag.
  }

  TJvID3TermsOfUseFrame = class(TJvID3Frame)
  private
    FText: TJvID3StringPair;
    FLanguage: string;
    procedure SetLanguage(const Value: string);
    procedure SetText(const Value: string);
    procedure SetTextW(const Value: WideString);
  protected
    procedure ReadFrame; override;
    procedure WriteFrame; override;

    function GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal; override;
    function GetIsEmpty: Boolean; override;

    function SupportsVersion(const AVersion: TJvID3Version): Boolean; override;
    function SameUniqueIDAs(const Frame: TJvID3Frame): Boolean; override;
  public
    class function CanAddFrame(AController: TJvID3Controller; AFrameID: TJvID3FrameID): Boolean; override;
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    class function Find(AController: TJvID3Controller): TJvID3TermsOfUseFrame;
    class function FindOrCreate(AController: TJvID3Controller): TJvID3TermsOfUseFrame;
  published
    property Language: string read FLanguage write SetLanguage;
    property Text: string read FText.SA write SetText;
    property TextW: WideString read FText.SW write SetTextW;
  end;

  { OWNE - fiOwnership - Ownership frame

    There may only be one "OWNE" frame in a tag.
  }

  TJvID3OwnershipFrame = class(TJvID3Frame)
  private
    FPricePayed: string;
    FSeller: TJvID3StringPair;
    FDateOfPurch: TDateTime;
    procedure SetDateOfPurch(const Value: TDateTime);
    procedure SetPricePayed(const Value: string);
    procedure SetSeller(const Value: string);
    procedure SetSellerW(const Value: WideString);
  protected
    procedure ReadFrame; override;
    procedure WriteFrame; override;

    function GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal; override;
    function GetIsEmpty: Boolean; override;

    function SupportsVersion(const AVersion: TJvID3Version): Boolean; override;
    function SameUniqueIDAs(const Frame: TJvID3Frame): Boolean; override;
  public
    class function CanAddFrame(AController: TJvID3Controller; AFrameID: TJvID3FrameID): Boolean; override;
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    class function Find(AController: TJvID3Controller): TJvID3OwnershipFrame;
    class function FindOrCreate(AController: TJvID3Controller): TJvID3OwnershipFrame;
  published
    property PricePayed: string read FPricePayed write SetPricePayed;
    property DateOfPurch: TDateTime read FDateOfPurch write SetDateOfPurch;
    property Seller: string read FSeller.SA write SetSeller;
    property SellerW: WideString read FSeller.SW write SetSellerW;
  end;

  { APIC - fiPicture - Attached picture

    There may be several pictures attached to one file, each in their individual
    "APIC" frame, but only one with the same content descriptor(*). There may only
    be one picture with the picture type declared as picture type $01 and $02 (**)
    respectively.

    (*) content descriptor = FPictureType, FDescription
    (**) $01 = ptFileIcon; $02 = ptOtherFileIcon
  }

  TJvID3PictureFrame = class(TJvID3BinaryFrame)
  private
    FMIMEType: string;
    FPictureType: TJvID3PictureType;
    FDescription: TJvID3StringPair;
    FURL: string;
    procedure SetDescription(const Value: string);
    procedure SetDescriptionW(const Value: WideString);
    procedure SetMIMEType(const Value: string);
    procedure SetURL(const Value: string);
  protected
    procedure ReadFrame; override;
    procedure WriteFrame; override;

    function GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal; override;
    function GetIsEmpty: Boolean; override;

    function SameUniqueIDAs(const Frame: TJvID3Frame): Boolean; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    class function CanAddFrame(AController: TJvID3Controller; AFrameID: TJvID3FrameID): Boolean; override;
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    class function Find(AController: TJvID3Controller; const AType: TJvID3PictureType): TJvID3PictureFrame;
    class function FindOrCreate(AController: TJvID3Controller; const AType: TJvID3PictureType): TJvID3PictureFrame;
  published
    property MIMEType: string read FMIMEType write SetMIMEType;
    property PictureType: TJvID3PictureType read FPictureType write FPictureType;
    property Description: string read FDescription.SA write SetDescription;
    property DescriptionW: WideString read FDescription.SW write SetDescriptionW;
    { Only used when MIMEType = '-->' }
    property URL: string read FURL write SetURL;
  end;

  TJvID3CustomTextFrame = class(TJvID3Frame)
  protected
    procedure GetText(var AText: TJvID3StringPair); virtual; abstract;
    procedure NewText(const ANewText: TJvID3StringPair); virtual; abstract;

    procedure ReadFrame; override;
    procedure WriteFrame; override;

    function GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal; override;
    function GetIsEmpty: Boolean; override;

    function SupportsVersion(const AVersion: TJvID3Version): Boolean; override;
    function SameUniqueIDAs(const Frame: TJvID3Frame): Boolean; override;
  public
    class function CanAddFrame(AController: TJvID3Controller; AFrameID: TJvID3FrameID): Boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
  end;

  TJvID3SimpleListFrame = class(TJvID3CustomTextFrame)
  private
    FList: TStringList;
    FListW: TWideStringList;
    function GetList: TStrings;
    function GetListW: TWideStrings;
    procedure SetList(const Value: TStrings);
    procedure SetListW(const Value: TWideStrings);
    function GetSeparator: Char;
    function GetSeparatorW: WideChar;
    function GetFixedStringLength: Integer;
    procedure ListChanged(Sender: TObject);
  protected
    procedure GetText(var AText: TJvID3StringPair); override;
    procedure NewText(const ANewText: TJvID3StringPair); override;

    function GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal; override;
  public
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; override;
    class function Find(AController: TJvID3Controller; const AFrameID: TJvID3FrameID): TJvID3SimpleListFrame;
    class function FindOrCreate(AController: TJvID3Controller; const AFrameID: TJvID3FrameID): TJvID3SimpleListFrame;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property FixedStringLength: Integer read GetFixedStringLength;
    property Separator: Char read GetSeparator;
    property SeparatorW: WideChar read GetSeparatorW;
  published
    property List: TStrings read GetList write SetList;
    property ListW: TWideStrings read GetListW write SetListW;
  end;

  TJvID3NumberFrame = class(TJvID3CustomTextFrame)
  private
    FValue: Cardinal;
    procedure SetValue(const Value: Cardinal);
  protected
    procedure GetText(var AText: TJvID3StringPair); override;
    procedure NewText(const ANewText: TJvID3StringPair); override;
    procedure ChangeToVersion(const ANewVersion: TJvID3Version); override;
    function GetIsEmpty: Boolean; override;
  public
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; override;
    class function Find(AController: TJvID3Controller; const AFrameID: TJvID3FrameID): TJvID3NumberFrame;
    class function FindOrCreate(AController: TJvID3Controller; const AFrameID: TJvID3FrameID): TJvID3NumberFrame;
  published
    property Value: Cardinal read FValue write SetValue;
  end;

  TJvID3TimestampFrame = class(TJvID3CustomTextFrame)
  private
    FValue: TDateTime;
    procedure SetValue(const Value: TDateTime);
  protected
    procedure GetText(var AText: TJvID3StringPair); override;
    procedure NewText(const ANewText: TJvID3StringPair); override;
    procedure ChangeToVersion(const ANewVersion: TJvID3Version); override;
  public
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; override;
    class function Find(AController: TJvID3Controller; const AFrameID: TJvID3FrameID): TJvID3TimestampFrame;
    class function FindOrCreate(AController: TJvID3Controller; const AFrameID: TJvID3FrameID): TJvID3TimestampFrame;
  published
    property Value: TDateTime read FValue write SetValue;
  end;

  TJvID3TextFrame = class(TJvID3CustomTextFrame)
  private
    FText: TJvID3StringPair;
    procedure SetText(const Value: string);
    procedure SetTextW(const Value: WideString);
  protected
    procedure GetText(var AText: TJvID3StringPair); override;
    procedure NewText(const ANewText: TJvID3StringPair); override;
    procedure ChangeToVersion(const ANewVersion: TJvID3Version); override;
  public
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; override;
    class function Find(AController: TJvID3Controller; const AFrameID: TJvID3FrameID): TJvID3TextFrame;
    class function FindOrCreate(AController: TJvID3Controller; const AFrameID: TJvID3FrameID): TJvID3TextFrame;
  published
    property Text: string read FText.SA write SetText;
    property TextW: WideString read FText.SW write SetTextW;
  end;

  TJvID3URLFrame = class(TJvID3Frame)
  private
    FURL: string;
    procedure SetURL(const Value: string);
  protected
    procedure ReadFrame; override;
    procedure WriteFrame; override;

    function GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal; override;
    function GetIsEmpty: Boolean; override;

    function SameUniqueIDAs(const Frame: TJvID3Frame): Boolean; override;
  public
    class function CanAddFrame(AController: TJvID3Controller; AFrameID: TJvID3FrameID): Boolean; override;
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    class function Find(AController: TJvID3Controller; const AFrameID: TJvID3FrameID): TJvID3URLFrame;
    class function FindOrCreate(AController: TJvID3Controller; const AFrameID: TJvID3FrameID): TJvID3URLFrame;
  published
    property URL: string read FURL write SetURL;
  end;

  { TXXX - fiUserText - User defined text information
  }

  TJvID3UserFrame = class(TJvID3Frame)
  private
    FValue: TJvID3StringPair;
    FDescription: TJvID3StringPair;
    procedure SetDescription(const Value: string);
    procedure SetDescriptionW(const Value: WideString);
    procedure SetValue(const Value: string);
    procedure SetValueW(const Value: WideString);
  protected
    procedure ReadFrame; override;
    procedure WriteFrame; override;

    function GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal; override;
    function GetIsEmpty: Boolean; override;
  public
    class function CanAddFrame(AController: TJvID3Controller; AFrameID: TJvID3FrameID): Boolean; override;
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    class function Find(AController: TJvID3Controller; const Index: Integer): TJvID3UserFrame;
    class function FindOrCreate(AController: TJvID3Controller; const Index: Integer): TJvID3UserFrame;
  published
    property Description: string read FDescription.SA write SetDescription;
    property DescriptionW: WideString read FDescription.SW write SetDescriptionW;
    property Value: string read FValue.SA write SetValue;
    property ValueW: WideString read FValue.SW write SetValueW;
  end;

  { WXXX - fiWWWUser - User defined URL link
  }

  TJvID3URLUserFrame = class(TJvID3Frame)
  private
    FDescription: TJvID3StringPair;
    FURL: string;
    procedure SetDescription(const Value: string);
    procedure SetDescriptionW(const Value: WideString);
    procedure SetURL(const Value: string);
  protected
    procedure ReadFrame; override;
    procedure WriteFrame; override;

    function GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal; override;
    function GetIsEmpty: Boolean; override;
  public
    class function CanAddFrame(AController: TJvID3Controller; AFrameID: TJvID3FrameID): Boolean; override;
    function CheckFrame(const HandleError: TJvID3HandleError): Boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;

    class function Find(AController: TJvID3Controller; const Index: Integer): TJvID3URLUserFrame;
    class function FindOrCreate(AController: TJvID3Controller; const Index: Integer): TJvID3URLUserFrame;
  published
    property Description: string read FDescription.SA write SetDescription;
    property DescriptionW: WideString read FDescription.SW write SetDescriptionW;
    property URL: string read FURL write SetURL;
  end;

  TJvID3FileInfo = class(TPersistent)
  private
    FAudioSize: Int64;
    FBitrate: Integer;
    FBits: TJvMPEGBits;
    FChannelMode: TJvMPEGChannelMode;
    FEmphasis: TJvMPEGEmphasis;
    FFileSize: Int64;
    FFrameCount: Integer;
    FFrameLengthInBytes: Integer;
    FHasID3v1Tag: Boolean;
    FHeaderFoundAt: Int64;
    FIsVBR: Boolean;
    FLayer: TJvMPEGLayer;
    FLengthInSec: Integer;
    FModeExtension: TJvMPEGModeExtension;
    FPaddingLength: Integer;
    FSamplingRateFrequency: Integer;
    FVersion: TJvMPEGVersion;
    function GetIsValid: Boolean;
  protected
    procedure Calc;
    procedure ParseMPEGTag(AMPEGTag: PChar);
    procedure ParseVbrTag(AMPEGTag: PChar);
    procedure Reset;
  public
    procedure Read(AStream: TStream; const Offset: Int64);

    property Bitrate: Integer read FBitrate;
    property Bits: TJvMPEGBits read FBits;
    property ChannelMode: TJvMPEGChannelMode read FChannelMode;
    property Emphasis: TJvMPEGEmphasis read FEmphasis;
    property FileSize: Int64 read FFileSize;
    property FrameCount: Integer read FFrameCount;
    property FrameLengthInBytes: Integer read FFrameLengthInBytes;
    property HeaderFoundAt: Int64 read FHeaderFoundAt;
    property IsValid: Boolean read GetIsValid;
    property IsVbr: Boolean read FIsVbr;
    property Layer: TJvMPEGLayer read FLayer;
    property LengthInSec: Integer read FLengthInSec;
    property ModeExtension: TJvMPEGModeExtension read FModeExtension;
    property SamplingRateFrequency: Integer read FSamplingRateFrequency;
    property Version: TJvMPEGVersion read FVersion;
  end;

  TJvID3ControllerDesigner = class(TObject)
  private
    FController: TJvID3Controller;
  public
    constructor Create(Controller: TJvID3Controller);
    destructor Destroy; override;
    procedure BeginDesign;
    procedure ID3Event(Event: TJvID3Event; Info: Longint); virtual;
    procedure EndDesign;
    property Controller: TJvID3Controller read FController;
  end;

  TJvID3ControllerState = (icsReading, icsWriting, icsUsingTempStream);
  TJvID3ControllerStates = set of TJvID3ControllerState;

  TJvID3Controller = class(TJvComponent)
  private
    FState: TJvID3ControllerStates;
    FStream: TJvID3Stream;
    FTempStream: TJvID3Stream;
    FFrames: TJvID3Frames;
    FClients: TList;
    FActivateEvents: TList;

    FFileInfo: TJvID3FileInfo;
    FHeader: TJvID3Header;
    FExtendedHeader: TJvID3ExtendedHeader;
    FActive: Boolean;
    FStreamedActive: Boolean;
    FFileName: TFileName;
    FDesigner: TJvID3ControllerDesigner;
    FModified: Boolean;
    FOptions: TJvID3ControllerOptions;
    FWriteEncodingAs: TJvID3ForceEncoding;
    FReadEncodingAs: TJvID3ForceEncoding;
    FReadVersionAs: TJvID3ForceVersion;
    FWriteVersionAs: TJvID3ForceVersion;
    FUpdateCount: Integer;
    function GetFrameCount: Integer;
    function GetReadVersion: TJvID3Version;
    function GetTagSize: Cardinal;
    function GetVersion: TJvID3Version;
    function GetWriteVersion: TJvID3Version;
    procedure SetActive(const Value: Boolean);
    procedure SetExtendedHeader(const Value: TJvID3ExtendedHeader);
    procedure SetFileName(const Value: TFileName);
    procedure SetHeader(const Value: TJvID3Header);
    procedure SetReadEncodingAs(const Value: TJvID3ForceEncoding);
    procedure SetReadVersionAs(const Value: TJvID3ForceVersion);
    procedure SetVersion(NewVersion: TJvID3Version);
    procedure SetWriteEncodingAs(const Value: TJvID3ForceEncoding);
    procedure SetWriteVersionAs(const Value: TJvID3ForceVersion);
  protected
    class function GetFrameClass(const FrameID: TJvID3FrameID): TJvID3FrameClass; virtual;
    procedure SetModified(Value: Boolean);
    procedure ChangeToVersion(const ANewVersion: TJvID3Version);

    procedure CheckFrameClass(FrameClass: TJvID3FrameClass; const AFrameID: TJvID3FrameID);

    procedure RegisterClient(Client: TObject; Event: TJvID3ActivateChangeEvent = nil); virtual;
    procedure SendActivateEvent(Activated: Boolean);
    procedure UnRegisterClient(Client: TObject); virtual;

    procedure ID3Event(Event: TJvID3Event; Info: Longint); virtual;

    procedure BeginReading;
    procedure EndReading;
    procedure BeginWriting;
    procedure EndWriting;
    procedure BeginUseTempStream;
    procedure EndUseTempStream;

    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);

    procedure DoOpen; virtual;
    procedure DoClose; virtual;

    procedure Loaded; override;

    procedure ApplyUnsynchronisationSchemeOnCurrentStream;

    { Temporary stream functions }
    function GetTempStreamSize: Cardinal;
    procedure RemoveUnsynchronisationSchemeToTempStream(const ASize: Integer);
    procedure WriteTempStream;

    property Header: TJvID3Header read FHeader write SetHeader stored False;
    property ExtendedHeader: TJvID3ExtendedHeader read FExtendedHeader write SetExtendedHeader stored False;
    property FileInfo: TJvID3FileInfo read FFileInfo;
    property ReadEncodingAs: TJvID3ForceEncoding read FReadEncodingAs write SetReadEncodingAs default ifeDontCare;
    property WriteEncodingAs: TJvID3ForceEncoding read FWriteEncodingAs write SetWriteEncodingAs default ifeDontCare;
    property ReadVersionAs: TJvID3ForceVersion read FReadVersionAs write SetReadVersionAs default ifvDontCare;
    property WriteVersionAs: TJvID3ForceVersion read FWriteVersionAs write SetWriteVersionAs default ifvDontCare;
    property Options: TJvID3ControllerOptions read FOptions write FOptions default [coAutoCorrect,
      coRemoveEmptyFrames];
    property Version: TJvID3Version read GetVersion write SetVersion stored False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Open;
    procedure Commit;
    procedure Erase;
    procedure Close;

    { Indicates whether a frame of type AFrameID can be added to the tag. For
      example there may not be more than 1 text frame with the same frame
      id - for example fiAlbum - in the tag. }
    function CanAddFrame(const AFrameID: TJvID3FrameID): Boolean;
    { Indicates whether tag has has a frame of type AFrameID }
    function HasFrame(const AFrameID: TJvID3FrameID): Boolean;
    { Adds a frame of type AFrameID to the tag }
    function AddFrame(const AFrameID: TJvID3FrameID): TJvID3Frame;
    function FindFirstFrame(const AFrameID: TJvID3FrameID;
      var Frame: TJvID3Frame): Boolean;
    function FindNextFrame(const AFrameID: TJvID3FrameID; var From: TJvID3Frame): Boolean;
    { Returns the nr. of frames of type AFrameID in the tag }
    function GetFrameCountFor(const AFrameID: TJvID3FrameID): Cardinal;

    function CopyToID3v1(const DoOverwrite: Boolean = True): Boolean;
    procedure CopyToID3v1Ctrl(AID3v1: TJvID3v1; const DoOverwrite: Boolean = True);
    function CopyFromID3v1(const DoOverwrite: Boolean = True): Boolean; 
    procedure CopyFromID3v1Ctrl(AID3v1: TJvID3v1; const DoOverwrite: Boolean = True);

    procedure EnsureExists(const FrameIDs: TJvID3FrameIDs);

    property Designer: TJvID3ControllerDesigner read FDesigner;
    property TagSize: Cardinal read GetTagSize;
    property Modified: Boolean read FModified;
    property FrameCount: Integer read GetFrameCount;
    property Frames: TJvID3Frames read FFrames;
    property WriteVersion: TJvID3Version read GetWriteVersion;
    property ReadVersion: TJvID3Version read GetReadVersion;
  published
    property Active: Boolean read FActive write SetActive;
    property FileName: TFileName read FFileName write SetFileName;
  end;

procedure ID3Error(const Msg: string; Component: TComponent = nil);
procedure ID3ErrorFmt(const Msg: string; const Args: array of const;
  Component: TComponent = nil);
function CreateUniqueName(AController: TJvID3Controller; const FrameName: string;
  FrameClass: TJvID3FrameClass; Component: TComponent): string;
procedure GetID3v2Version(const AFileName: string; var HasTag: Boolean;
  var Version: TJvID3Version);
function ExtToMIMEType(const Ext: string): string;
function MIMETypeToExt(const MIMEType: string): string;
function GenreToNiceGenre(const AGenre: string): string;
function NiceGenreToGenre(const ANiceGenre: string): string;

implementation

uses
  Graphics, Windows,
  {$IFNDEF COMPILER6_UP}
  Forms,
  {$ENDIF COMPILER6_UP}
  JclBase, JclFileUtils, JclLogic, JclDateTime,
  JvConsts, JvResources;

type
  TJvID3StringList = class(TStringList)
  public
    function GetSeparatedText(const Separator: string): string;
  end;

const
  CMapBitrate: array [Boolean, TJvMPEGLayer] of Byte =
   (
    { ?? - III - II -  I }
    ( $00, $02, $01, $00), // V1
    ( $00, $04, $04, $03) // V2/V3
   );

  CFreeBitrate = -2;

  CBadBitrate = -1;

  CBitrate: array [$00..$04, $00..$0F] of Integer =
   (
    (CFreeBitrate, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448, CBadBitrate),
    (CFreeBitrate, 32, 48, 56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320, 384, CBadBitrate),
    (CFreeBitrate, 32, 40, 48,  56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320, CBadBitrate),
    (CFreeBitrate, 32, 48, 56,  64,  80,  96, 112, 128, 144, 160, 176, 192, 224, 256, CBadBitrate),
    (CFreeBitrate,  8, 16, 24,  32,  40,  48,  56,  64,  80,  96, 112, 128, 144, 160, CBadBitrate)
   );

  CSamplingFrequency: array [TJvMPEGVersion, $00..$03] of Integer =
   (
    (11025, 12000,  8000, -1), // mvVersion25,
    (    0,     0,     0,  0), // mvReserved,
    (22050, 24000, 16000, -1), // mvVersion2,
    (44100, 48000, 32000, -1)  // mvVersion1
   );

  CLayerArray: array [TJvMPEGLayer] of Integer =
   (
    1,           // mlNotDefined,
    144000,      // mlLayerIII,
    144000,      // mlLayerII,
    48000        // mlLayerI
   );

  cUnknownLanguage = 'XXX';
  cID3HeaderId = 'ID3';  // do not change case
  cChangeTagSizeFileNameTemplate = 'ChangeTagSize';
  cPictureFrameFileNameTemplate = 'TJvID3PictureFrame';
  cURLArrow = '-->';

var
  DefaultFrameClasses: array [TJvID3FrameID] of TJvID3FrameClass =
   (
    nil, { fiErrorFrame (special frame) }
    nil, { fiPaddingFrame (special frame) }
    TJvID3SkipFrame, { fiNoFrame (special frame) }
    TJvID3AudioEncryptionFrame, { fiAudioCrypto }
    TJvID3PictureFrame, { fiPicture }
    nil, { fiAudioSeekPoint (new in 2.4) }
    TJvID3ContentFrame, { fiComment }
    nil, { fiCommercial (new in 2.3) }
    nil, { fiCryptoReg (new in 2.3) }
    nil, { fiEqualization2 (new in 2.4) }
    nil, { fiEqualization (deprecated as of 2.4) }
    nil, { fiEventTiming }
    TJvID3GeneralObjFrame, { fiGeneralObject }
    nil, { fiGroupingReg (new in 2.3) }
    TJvID3DoubleListFrame, { fiInvolvedPeople (deprecated as of 2.4) }
    nil, { fiLinkedInfo }
    TJvID3BinaryFrame, { fiCDID }
    nil, { fiMPEGLookup }
    TJvID3OwnershipFrame, { fiOwnership (new in 2.3) }
    nil, { fiPrivate (new in 2.3) }
    TJvID3PlayCounterFrame, { fiPlayCounter }
    TJvID3PopularimeterFrame, { fiPopularimeter }
    nil, { fiPositionsync (new in 2.3) }
    nil, { fiBufferSize }
    nil, { fiVolumeAdj2 (new in 2.4) }
    nil, { fiVolumeAdj (deprecated as of 2.4) }
    nil, { fiReverb }
    nil, { fiSeekFrame (new in 2.4) }
    nil, { fiSignature (new in 2.4) }
    nil, { fiSyncedLyrics }
    nil, { fiSyncedTempo }
    TJvID3TextFrame, { fiAlbum }
    TJvID3NumberFrame, { fiBPM }
    TJvID3SimpleListFrame, { fiComposer }
    TJvID3TextFrame, { fiContentType }
    TJvID3TextFrame, { fiCopyright }
    TJvID3TextFrame, { fiDate (deprecated as of 2.4) }
    TJvID3TimeStampFrame, { fiEncodingTime (new in 2.4) }
    TJvID3NumberFrame, { fiPlaylistDelay }
    TJvID3TimeStampFrame, { fiOrigReleaseTime (new in 2.4) }
    TJvID3TimeStampFrame, { fiRecordingTime (new in 2.4) }
    TJvID3TimeStampFrame, { fiReleaseTime (new in 2.4) }
    TJvID3TimeStampFrame, { fiTaggingTime (new in 2.4) }
    TJvID3DoubleListFrame, { fiInvolvedPeople2 (new in 2.4) }
    TJvID3TextFrame, { fiEncodedBy }
    TJvID3SimpleListFrame, { fiLyricist }
    TJvID3TextFrame, { fiFileType }
    TJvID3TextFrame, { fiTime (deprecated as of 2.4) }
    TJvID3TextFrame, { fiContentGroup }
    TJvID3TextFrame, { fiTitle }
    TJvID3TextFrame, { fiSubTitle }
    TJvID3TextFrame, { fiInitialKey }
    TJvID3SimpleListFrame, { fiLanguage }
    TJvID3NumberFrame, { fiSongLen }
    TJvID3DoubleListFrame, { fiMusicianCreditList (new in 2.4) }
    TJvID3TextFrame, { fiMediaType }
    TJvID3TextFrame, { fiMood (new in 2.4) }
    TJvID3TextFrame, { fiOrigAlbum }
    TJvID3TextFrame, { fiOrigFileName }
    TJvID3SimpleListFrame, { fiOrigLyricist }
    TJvID3SimpleListFrame, { fiOrigArtist }
    TJvID3NumberFrame, { fiOrigYear (deprecated as of 2.4) }
    TJvID3TextFrame, { fiFileOwner (new in 2.3) }
    TJvID3SimpleListFrame, { fiLeadArtist }
    TJvID3TextFrame, { fiBand }
    TJvID3TextFrame, { fiConductor }
    TJvID3TextFrame, { fiMixArtist }
    TJvID3TextFrame, { fiPartInSet }
    TJvID3TextFrame, { fiProducedNotice (new in 2.4) }
    TJvID3TextFrame, { fiPublisher }
    TJvID3TextFrame, { fiTrackNum }
    TJvID3TextFrame, { fiRecordingDates (deprecated as of 2.4) }
    TJvID3TextFrame, { fiNetRadioStation }
    TJvID3TextFrame, { fiNetRadioOwner }
    TJvID3NumberFrame, { fiSize (deprecated as of 2.4) }
    TJvID3TextFrame, { fiAlbumSortOrder (new in 2.4) }
    TJvID3TextFrame, { fiPerformerSortOrder (new in 2.4) }
    TJvID3TextFrame, { fiTitleSortOrder (new in 2.4) }
    TJvID3TextFrame, { fiISRC }
    TJvID3TextFrame, { fiEncoderSettings (new in 2.3) }
    TJvID3TextFrame, { fiSetSubTitle (new in 2.4) }
    TJvID3UserFrame, { fiUserText }
    TJvID3NumberFrame, { fiYear (deprecated as of 2.4) }
    nil, { fiUniqueFileID }
    TJvID3TermsOfUseFrame, { fiTermsOfUse (new in 2.3) }
    TJvID3ContentFrame, { fiUnsyncedLyrics }
    TJvID3URLFrame, { fiWWWCommercialInfo }
    TJvID3URLFrame, { fiWWWCopyright }
    TJvID3URLFrame, { fiWWWAudioFile }
    TJvID3URLFrame, { fiWWWArtist }
    TJvID3URLFrame, { fiWWWAudioSource }
    TJvID3URLFrame, { fiWWWRadioPage }
    TJvID3URLFrame, { fiWWWPayment }
    TJvID3URLFrame, { fiWWWPublisher }
    TJvID3URLUserFrame, { fiWWWUser }
    nil, { fiMetaCrypto (only in 2.2) }
    nil { fiMetaCompressio (only in 2.2) }
   );

//=== Local procedures =======================================================

function LengthUTF8Str(const SW: WideString): Integer;
begin
  Result := Length(WideStringToUTF8(SW));
end;

function GetStringA(const SP: TJvID3StringPair; const Encoding: TJvID3Encoding): string;
begin
  { Returns a ansi string from the string holder SP, Encoding specifies
    whether the result string should be from the unicode or ansi part of SP }

  case Encoding of
    ienISO_8859_1:
      Result := SP.SA;
    ienUTF_16, ienUTF_16BE, ienUTF_8:
      Result := WideStringToStringEx(SP.SW, CP_ACP);
  else
    ID3Error(RsEID3UnknownEncoding);
  end;
end;

function GetStringW(const SP: TJvID3StringPair; const Encoding: TJvID3Encoding): WideString;
begin
  { Returns a unicode string from the string holder SP, Encoding specifies
    whether the result string should be from the unicode or ansi part of SP }

  case Encoding of
    ienISO_8859_1:
      Result := StringToWideStringEx(SP.SA, CP_ACP);
    ienUTF_16, ienUTF_16BE, ienUTF_8:
      Result := SP.SW;
  else
    ID3Error(RsEID3UnknownEncoding);
  end;
end;

function SameStringPair(const S1, S2: TJvID3StringPair; const Enc1, Enc2: TJvID3Encoding): Boolean;
var
  SW1, SW2: WideString;
begin
  { Compares two string pairs, without case sensitivity; it's used to check if
    2 frames have the same content descriptor; documentation is not clear whether
    this must be done with case sensitivity or not. }

  if (Enc1 = Enc2) and (Enc1 = ienISO_8859_1) then
    Result := AnsiSameStr(S1.SA, S2.SA)
  else
  begin
    if Enc1 = ienISO_8859_1 then
      SW1 := StringToWideStringEx(S1.SA, CP_ACP)
    else
      SW1 := S1.SW;

    if Enc2 = ienISO_8859_1 then
      SW2 := StringToWideStringEx(S2.SA, CP_ACP)
    else
      SW2 := S2.SW;

    Result := JclUnicode.StrICompW(PWideChar(SW1), PWideChar(SW2)) = 0;
  end;
end;

procedure SetStringA(var SP: TJvID3StringPair; const Encoding: TJvID3Encoding; const S: string);
begin
  { Stores the ansi string S in the string holder SP, Encoding specifies
    whether S should be stored in SP as unicode or ansi }

  case Encoding of
    ienISO_8859_1:
      SP.SA := S;
    ienUTF_16, ienUTF_16BE, ienUTF_8:
      SP.SW := StringToWideStringEx(S, CP_ACP);
  else
    ID3Error(RsEID3UnknownEncoding);
  end;
end;

function GetCharCount(const SP: TJvID3StringPair; const Encoding: TJvID3Encoding): Cardinal;
begin
  { Returns the nr. of characters of a string of a specific encoding }

  case Encoding of
    ienISO_8859_1:
      Result := Length(SP.SA);
    ienUTF_16, ienUTF_16BE, ienUTF_8:
      Result := Length(SP.SW);
  else
    Result := 0;
    ID3Error(RsEID3UnknownEncoding);
  end;
end;

(* make Delphi 5 compiler happy // andreas
function GetByteCount(const SP: TJvID3StringPair; const Encoding: TJvID3Encoding): Cardinal;
begin
  { Returns the nr. of bytes needed to store a string of a specific encoding }

  case Encoding of
    ienISO_8859_1:
      Result := GetCharCount(SP, Encoding);
    ienUTF_16:
      Result := GetCharCount(SP, Encoding) * 2 + 2 { BOM };
    ienUTF_16BE:
      Result := GetCharCount(SP, Encoding) * 2;
    ienUTF_8:
      Result := LengthUTF8Str(GetStringW(SP, Encoding));
  else
    Result := 0;
    ID3Error(RsEID3UnknownEncoding);
  end;
end;
*)

function LengthEnc(const S: TJvID3StringPair; const FromEnc, ToEnc: TJvID3Encoding): Cardinal;
begin
  { Calculates the length in bytes needed to store a string in a stream encoded as
    ToEnc; the string is encoded as FromEnc in the string pair S;
    Very similar to GetByteCount }

  case ToEnc of
    ienISO_8859_1:
      Result := GetCharCount(S, FromEnc);
    ienUTF_16:
      Result := 2 + 2 * GetCharCount(S, FromEnc);
    ienUTF_16BE:
      Result := 2 * GetCharCount(S, FromEnc);
    ienUTF_8:
      Result := LengthUTF8Str(GetStringW(S, FromEnc));
  else
    Result := 0;
    ID3Error(RsEID3UnknownEncoding);
  end;
end;

function LengthTerminatorEnc(const Encoding: TJvID3Encoding): Cardinal;
begin
  { Calculates the length in bytes needed to store a terminator in the encoding
    specified by Encoding }

  case Encoding of
    ienISO_8859_1, ienUTF_8:
      Result := 1;
    ienUTF_16, ienUTF_16BE:
      Result := 2;
  else
    Result := 0;
    ID3Error(RsEID3UnknownEncoding);
  end;
end;

procedure ClearStringPair(var SP: TJvID3StringPair);
begin
  with SP do
  begin
    SA := '';
    SW := '';
  end;
end;

procedure CopyStringPair(const Source: TJvID3StringPair; var Dest: TJvID3StringPair);
begin
  with Dest do
  begin
    SA := Source.SA;
    SW := Source.SW;
  end;
end;

procedure CopyLists(SourceA: TStrings; SourceW: TWideStrings; const SourceEnc: TJvID3Encoding;
  DestA: TStrings; DestW: TWideStrings; const DestEnc: TJvID3Encoding);
var
  I: Integer;
begin
  { Copies the strings in the stringlist SourceA or SourceW - depending on the
    value of SourceEnc - to the stringlist DestA or DestW - depending on the
    value of DestEnc }

  case SourceEnc of
    ienISO_8859_1:
      case DestEnc of
        ienISO_8859_1:
          for I := 0 to SourceA.Count - 1 do
            DestA.Add(SourceA[I]);
        ienUTF_16, ienUTF_16BE, ienUTF_8:
          for I := 0 to SourceA.Count - 1 do
            DestW.Add(StringToWideStringEx(SourceA[I], CP_ACP));
      else
        ID3Error(RsEID3UnknownEncoding);
      end;
    ienUTF_16, ienUTF_16BE, ienUTF_8:
      case DestEnc of
        ienISO_8859_1:
          for I := 0 to SourceW.Count - 1 do
            DestA.Add(WideStringToStringEx(SourceW[I], CP_ACP));
        ienUTF_16, ienUTF_16BE, ienUTF_8:
          for I := 0 to SourceW.Count - 1 do
            DestW.Add(SourceW[I]);
      else
        ID3Error(RsEID3UnknownEncoding);
      end;

  else
    ID3Error(RsEID3UnknownEncoding);
  end;
end;

function CheckIsEmpty(const SP: TJvID3StringPair; const Encoding: TJvID3Encoding): Boolean;
begin
  { Returns True when the string pair SP contains an empty string, otherwise False }

  case Encoding of
    ienISO_8859_1:
      Result := SP.SA = '';
    ienUTF_16, ienUTF_16BE, ienUTF_8:
      Result := SP.SW = '';
  else
    Result := False;
    ID3Error(RsEID3UnknownEncoding);
  end;
end;

function CheckIsURL(Frame: TJvID3Frame; var S: string; const HandleError: TJvID3HandleError): Boolean;
begin
  { Not implemented }
  Result := True;
end;

function CheckIsLanguageA(Frame: TJvID3Frame; var S: string; const HandleError: TJvID3HandleError): Boolean;
begin
  { The three byte language field, present in several frames, is used to
    describe the language of the frame's content, according to ISO-639-2
    [ISO-639-2]. The language should be represented in lower case. If the
    language is not known the string "XXX" should be used.
  }

  Result := (S = cUnknownLanguage) or ISO_639_2IsCode(S);

  if not Result then
    case HandleError of
      heAutoCorrect:
        { Note, don't set Result to True }
        S := cUnknownLanguage;
      heRaise:
        Frame.ErrorFmt(RsEID3InvalidLanguageValue, [S]);
    else
      Exit;
    end
  else
  if HandleError = heAutoCorrect then
    S := AnsiLowerCase(S);
end;

function CheckIsLanguageW(Frame: TJvID3Frame; var S: WideString; const HandleError: TJvID3HandleError): Boolean;
var
  SA: string;
begin
  { The three byte language field, present in several frames, is used to
    describe the language of the frame's content, according to ISO-639-2
    [ISO-639-2]. The language should be represented in lower case. If the
    language is not known the string "XXX" should be used.
  }

  SA := WideStringToStringEx(S, CP_ACP);
  Result := CheckIsLanguageA(Frame, SA, HandleError);
  if not Result and (HandleError = heAutoCorrect) then
    S := StringToWideStringEx(SA, CP_ACP);
end;

function CheckIsID3TimeA(Frame: TJvID3Frame; var S: string; const HandleError: TJvID3HandleError): Boolean;
var
  I1, I2: Integer;
begin
  { S must be in HHMM format (H = Hour; M = Minute), and may not be empty }
  Result := Length(S) = 4;

  if Result then
  begin
    I1 := StrToIntDef(Copy(S, 1, 2), -1);
    I2 := StrToIntDef(Copy(S, 3, 4), -1);
    Result := (I1 >= 0) and (I1 < 24) and (I2 >= 0) and (I2 < 60);
  end;

  if not Result then
    case HandleError of
      heAutoCorrect:
        { Note, don't set Result to True }
        S := '0000';
      heRaise:
        Frame.ErrorFmt(RsEID3InvalidTimeValue, [S]);
    end;
end;

function CheckIsID3Time(Frame: TJvID3Frame; var SP: TJvID3StringPair;
  const HandleError: TJvID3HandleError): Boolean;
var
  S: string;
begin
  S := GetStringA(SP, Frame.Encoding);
  Result := CheckIsID3TimeA(Frame, S, HandleError);
  if not Result and (HandleError = heAutoCorrect) then
    SetStringA(SP, Frame.Encoding, S);
end;

function CheckIsID3DateA(Frame: TJvID3Frame; var S: string; const HandleError: TJvID3HandleError): Boolean;
var
  I1, I2: Integer;
begin
  { S must be in DDMM format (D = Day; M = Month), and may not be empty }
  Result := Length(S) = 4;

  if Result then
  begin
    I1 := StrToIntDef(Copy(S, 1, 2), -1);
    I2 := StrToIntDef(Copy(S, 3, 4), -1);
    Result := (I1 >= 1) and (I1 < 32) and (I2 >= 1) and (I2 < 13);
  end;

  if not Result then
    case HandleError of
      heAutoCorrect:
        { Note, don't set Result to True }
        S := '0101';
      heRaise:
        Frame.ErrorFmt(RsEID3InvalidDateValue, [S]);
    end;
end;

function CheckIsID3Date(Frame: TJvID3Frame; var SP: TJvID3StringPair;
  const HandleError: TJvID3HandleError): Boolean;
var
  S: string;
begin
  S := GetStringA(SP, Frame.Encoding);
  Result := CheckIsID3DateA(Frame, S, HandleError);
  if not Result and (HandleError = heAutoCorrect) then
    SetStringA(SP, Frame.Encoding, S);
end;

function CheckMaxCharCount(Frame: TJvID3Frame; var SP: TJvID3StringPair;
  const MaxCharCount: Cardinal;
  const HandleError: TJvID3HandleError): Boolean;
begin
  Result := GetCharCount(SP, Frame.Encoding) <= MaxCharCount;
  if not Result then
    case HandleError of
      heAutoCorrect:
        case Frame.Encoding of
          ienISO_8859_1:
            SetLength(SP.SA, MaxCharCount);
          ienUTF_16, ienUTF_16BE, ienUTF_8:
            SetLength(SP.SW, MaxCharCount);
        else
          Frame.Error(RsEID3UnknownEncoding);
        end;
      heRaise:
        Frame.ErrorFmt(RsEID3StringTooLong, [GetStringA(SP, Frame.Encoding)]);
    end;
end;

function GetID3Date(const SP: TJvID3StringPair; const Encoding: TJvID3Encoding;
  const Year: Word = 0): TDateTime;
var
  S: string;
  Day, Month: Word;
begin
  { must be DDMM }
  S := GetStringA(SP, Encoding);
  if Length(S) = 4 then
  begin
    Day := StrToIntDef(Copy(S, 1, 2), 1);
    Month := StrToIntDef(Copy(S, 3, 4), 1);
  end
  else
  begin
    Day := 1;
    Month := 1;
  end;

  try
    Result := EncodeDate(Year, Month, Day);
  except
    on EConvertError do
      Result := 0;
  end;
end;

function CheckIsLanguageListA(Frame: TJvID3Frame; Strings: TStrings;
  const HandleError: TJvID3HandleError): Boolean;
var
  I: Integer;
  S: string;
  Ok: Boolean;
begin
  Result := True;
  for I := 0 to Strings.Count - 1 do
  begin
    S := Strings[I];
    Ok := CheckIsLanguageA(Frame, S, HandleError);
    Result := Result and Ok;
    if not Ok then
      if HandleError = heAutoCorrect then
        Strings[I] := S
      else
        Break;
  end;
end;

function CheckIsLanguageListW(Frame: TJvID3Frame; Strings: TWideStrings;
  const HandleError: TJvID3HandleError): Boolean;
var
  I: Integer;
  S: WideString;
  Ok: Boolean;
begin
  Result := True;
  for I := 0 to Strings.Count - 1 do
  begin
    S := Strings[I];
    Ok := CheckIsLanguageW(Frame, S, HandleError);
    Result := Result and Ok;
    if not Ok then
      if HandleError = heAutoCorrect then
        Strings[I] := S
      else
        Break;
  end;
end;

function CheckListA(Frame: TJvID3Frame; Strings: TStrings; const ASeparator: Char;
  const HandleError: TJvID3HandleError): Boolean;
var
  I: Integer;
  S: string;
  LPos: Integer;
begin
  Result := True;
  if ASeparator = #0 then
    Exit;

  for I := 0 to Strings.Count - 1 do
  begin
    S := Strings[I];
    LPos := Pos(ASeparator, S);
    Result := Result and (LPos = 0);
    if LPos > 0 then
      case HandleError of
        heAutoCorrect:
          begin
            repeat
              Delete(S, LPos, 1);
              LPos := Pos(ASeparator, S);
            until LPos = 0;

            Strings[I] := S;
          end;
        heRaise:
          Frame.ErrorFmt(RsEID3InvalidCharInList, [ASeparator, S]);
      else
        Break;
      end;
  end;
end;

function CheckListW(Frame: TJvID3Frame; Strings: TWideStrings;
  const ASeparator: WideChar; const HandleError: TJvID3HandleError): Boolean;
var
  I: Integer;
  S: WideString;
  LPos: Integer;
begin
  Result := True;
  if ASeparator = WideNull then
    Exit;

  for I := 0 to Strings.Count - 1 do
  begin
    S := Strings[I];
    LPos := Pos(ASeparator, S);
    Result := Result and (LPos = 0);
    if LPos > 0 then
      case HandleError of
        heAutoCorrect:
          begin
            repeat
              Delete(S, LPos, 1);
              LPos := Pos(ASeparator, S);
            until LPos = 0;

            Strings[I] := S;
          end;
        heRaise:
          Frame.ErrorFmt(RsEID3InvalidCharInList, [ASeparator, S]);
      else
        Break;
      end;
  end;
end;

function GetID3Time(const SP: TJvID3StringPair; const Encoding: TJvID3Encoding;
  const Sec: Word = 0; MSec: Word = 0): TDateTime;
var
  S: string;
  Hour, Min: Word;
begin
  { must be HHMM }
  S := GetStringA(SP, Encoding);
  if Length(S) = 4 then
  begin
    Hour := StrToIntDef(Copy(S, 1, 2), 0);
    Min := StrToIntDef(Copy(S, 3, 4), 0);
  end
  else
  begin
    Hour := 0;
    Min := 0;
  end;

  try
    Result := EncodeTime(Hour, Min, Sec, MSec);
  except
    on EConvertError do
      Result := 0;
  end;
end;

function CheckIsID3PartInSetA(Frame: TJvID3Frame; var S: string; const HandleError: TJvID3HandleError): Boolean;
var
  P: Integer;
  I1, I2: Integer;
begin
  { S must be in N1/N2 or N format (N, N1, N2 = some number, ie [0..9]*,
    but may be empty }

  if S = '' then
  begin
    Result := True;
    Exit;
  end;

  P := Pos('/', S);
  if P > 1 then
  begin
    I1 := StrToIntDef(Copy(S, 1, P - 1), -1);
    I2 := StrToIntDef(Copy(S, P + 1, MaxInt), -1);
    Result := (I1 > -1) and (I2 > -1);
  end
  else
    Result := StrToIntDef(S, -1) > -1;

  if not Result then
    case HandleError of
      heAutoCorrect:
        { Note, don't set Result to True }
        S := '';
      heRaise:
        Frame.ErrorFmt(RsEID3InvalidPartInSetValue, [S]);
    end;
end;

function CheckIsID3PartInSet(Frame: TJvID3Frame; var SP: TJvID3StringPair;
  const HandleError: TJvID3HandleError): Boolean;
var
  S: string;
begin
  S := GetStringA(SP, Frame.Encoding);
  Result := CheckIsID3PartInSetA(Frame, S, HandleError);
  if not Result and (HandleError = heAutoCorrect) then
    SetStringA(SP, Frame.Encoding, S);
end;

{ Copied from DSDesign.pas }

function GenerateName(Controller: TJvID3Controller; FrameName: string;
  FrameClass: TJvID3FrameClass; Number: Integer): string;
var
  Fmt: string;

  procedure CrunchFrameName;
  var
    I: Integer;
  begin
    I := 1;
    while I <= Length(FrameName) do
    begin
      if FrameName[I] in IdentifierSymbols then
        Inc(I)
      else
      if FrameName[I] in LeadBytes then
        Delete(FrameName, I, 2)
      else
        Delete(FrameName, I, 1);
    end;
  end;

begin
  CrunchFrameName;
  if (FrameName = '') or (FrameName[1] in DigitSymbols) then
  begin
    if FrameClass <> nil then
      FrameName := FrameClass.ClassName + FrameName
    else
      FrameName := 'Frame' + FrameName;
    if FrameName[1] = 'T' then
      Delete(FrameName, 1, 1);
    CrunchFrameName;
  end;
  Fmt := '%s%s%d';
  if Number < 2 then
    Fmt := '%s%s';
  Result := Format(Fmt, [Controller.Name, FrameName, Number]);
end;

procedure SyncSafe(Source: Cardinal; var Dest; const DestSize: Integer); overload;
type
  TBytes = array [0..MaxInt - 1] of Byte;
var
  I: Byte;
begin
  { Test : Source = 255 -> Dest = $01 $80
           Source = 256 -> Dest = $02 $00
           Source = 257 -> Dest = $02 $01 etc.
  }

  for I := DestSize - 1 downto 0 do
  begin
    TBytes(Dest)[I] := Source and $7F; // $7F = %01111111
    Source := Source shr 7;
  end;
end;

procedure SyncSafe(Source: Int64; var Dest; const DestSize: Integer); overload;
type
  TBytes = array [0..MaxInt - 1] of Byte;
var
  I: Byte;
begin
  { Test : Source = 255 -> Dest = $01 $80
           Source = 256 -> Dest = $02 $00
           Source = 257 -> Dest = $02 $01 etc.
  }
  for I := DestSize - 1 downto 0 do
  begin
    TBytes(Dest)[I] := Source and $7F; // $7F = %01111111
    Source := Source shr 7;
  end;
end;

procedure TranslatePairString(var S: TJvID3StringPair; const SourceEnc, DestEnc: TJvID3Encoding);
begin
  case SourceEnc of
    ienISO_8859_1:
      if DestEnc <> ienISO_8859_1 then
        S.SW := StringToWideStringEx(S.SA, CP_ACP);
    ienUTF_16, ienUTF_16BE, ienUTF_8:
      if DestEnc = ienISO_8859_1 then
        S.SA := WideStringToStringEx(S.SW, CP_ACP);
  else
    ID3Error(RsEID3UnknownEncoding);
  end;
end;

procedure UnSyncSafe(var Source; const SourceSize: Integer; var Dest: Cardinal); overload;
type
  TBytes = array [0..MaxInt - 1] of Byte;
var
  I: Byte;
begin
  { Test : Source = $01 $80 -> Dest = 255
           Source = $02 $00 -> Dest = 256
           Source = $02 $01 -> Dest = 257 etc.
  }

  Dest := 0;
  for I := 0 to SourceSize - 1 do
  begin
    Dest := Dest shl 7;
    Dest := Dest or (TBytes(Source)[I] and $7F); // $7F = %01111111
  end;
end;

procedure UnSyncSafe(var Source; const SourceSize: Integer; var Dest: Int64); overload;
type
  TBytes = array [0..MaxInt - 1] of Byte;
var
  I: Byte;
begin
  { Test : Source = $01 $80 -> Dest = 255
           Source = $02 $00 -> Dest = 256
           Source = $02 $01 -> Dest = 257 etc.
  }

  Dest := 0;
  for I := 0 to SourceSize - 1 do
  begin
    Dest := Dest shl 7;
    Dest := Dest or (TBytes(Source)[I] and $7F); // $7F = %01111111
  end;
end;

procedure ExtractFixedStringsA(Content: PChar; const ALength: Integer; Strings: TStrings);
var
  P: PChar;
  S: string;
begin
  if (Content = nil) or (Content^ = #0) or (Strings = nil) or (ALength < 1) then
    Exit;

  Strings.BeginUpdate;
  try
    SetLength(S, ALength);

    while True do
    begin
      P := Content;

      while (P^ <> #0) and (P - Content < ALength) do
        Inc(P);

      if P - Content = ALength then
      begin
        Move(Content[0], S[1], ALength);
        Strings.Add(S);
      end;

      if P^ = #0 then
        Break;

      Inc(Content, ALength);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure ExtractFixedStringsW(Content: PWideChar; const ALength: Integer; Strings: TWideStrings);
var
  P: PWideChar;
  S: WideString;
begin
  if (Content = nil) or (Content^ = WideNull) or (Strings = nil) or (ALength < 1) then
    Exit;

  Strings.BeginUpdate;
  try
    SetLength(S, ALength);

    while True do
    begin
      P := Content;

      while (P^ <> #0) and (P - Content < ALength) do
        Inc(P);

      if P - Content = ALength then
      begin
        Move(Content[0], S[1], 2 * ALength);
        Strings.Add(S);
      end;

      if P^ = #0 then
        Break;

      Inc(Content, ALength);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure ExtractStringsA(Separator: Char; Content: PChar; Strings: TStrings);
var
  Tail: PChar;
  S: string;
  EOS: Boolean;
begin
  if (Content = nil) or (Content^ = #0) or (Strings = nil) then
    Exit;

  Strings.BeginUpdate;
  try
    Tail := Content;
    repeat
      while (Tail^ <> Separator) and (Tail^ <> #0) do
        Inc(Tail);

      EOS := Tail^ = #0;

      SetString(S, Content, Tail - Content);
      Strings.Add(S);

      Inc(Tail);
      Content := Tail;
    until EOS;
  finally
    Strings.EndUpdate;
  end;
end;

procedure ExtractStringsW(Separator: WideChar; Content: PWideChar; Strings: TWideStrings);
var
  Tail: PWideChar;
  S: WideString;
  EOS: Boolean;
begin
  if (Content = nil) or (Content^ = WideNull) or (Strings = nil) then
    Exit;

  Strings.BeginUpdate;
  try
    Tail := Content;

    repeat
      while (Tail^ <> Separator) and (Tail^ <> WideNull) do
        Inc(Tail);

      EOS := Tail^ = WideNull;

      SetLength(S, Tail - Content);
      Move(Content[0], S[1], 2 * (Tail - Content));
      Strings.Add(S);

      Inc(Tail);
      Content := Tail;
    until EOS;
  finally
    Strings.EndUpdate;
  end;
end;

function GetTagSizeInclHeader(Stream: TStream): Cardinal;
var
  Header: TID3v2HeaderRec;
begin
  if (Stream.Read(Header, SizeOf(Header)) = SizeOf(Header)) and
    (Header.Identifier = cID3HeaderId) then
  begin
    UnSyncSafe(Header.Size, 4, Result);
    Inc(Result, 10);
  end
  else
    Result := 0;
end;

procedure ChangeTagSize(const SourceFileName: string;
  const DestTagSizeInclHeader: Cardinal);
var
  DestFileName: string;
  Source, Dest: TFileStream;
  SourceFileSize: Int64;
  SourceTagSizeInclHeader: Cardinal; { size of tag + header size (=10) }
begin
  { (rb) Maybe we should copy the file attributes of the source file to
    the dest file? }

  Source := TFileStream.Create(SourceFileName, fmOpenRead or fmShareExclusive);
  try
    SourceTagSizeInclHeader := GetTagSizeInclHeader(Source);

    if SourceTagSizeInclHeader = DestTagSizeInclHeader then
      Exit;

    DestFileName := JclFileUtils.FileGetTempName(cChangeTagSizeFileNameTemplate);
    Dest := TFileStream.Create(DestFileName, fmCreate);
    try
      SourceFileSize := Source.Size;
      Dest.Size := SourceFileSize + DestTagSizeInclHeader - SourceTagSizeInclHeader;

      Source.Seek(SourceTagSizeInclHeader, soFromBeginning);
      Dest.Seek(DestTagSizeInclHeader, soFromBeginning);
      if SourceFileSize > SourceTagSizeInclHeader then
        Dest.CopyFrom(Source, SourceFileSize - SourceTagSizeInclHeader);
    finally
      Dest.Free;
    end;
  finally
    Source.Free;
  end;

  { If all went alright, then we now try to copy the dest file to
    the source file }
  if not DeleteFile(PChar(SourceFileName)) then
    RaiseLastOSError;

  if not RenameFile(DestFileName, SourceFileName) then
    RaiseLastOSError;
end;

function SearchSync(AStream: TStream;
  const BeginOffset: Integer; var Buffer; const BufferSize: Integer): Int64;
const
  CBufferSize = $0F00;
var
  LBuffer: PChar;
  I: Integer;
  LastWasFF: Boolean;
  BytesRead: Longint;
begin
  { Seek sync point 11111111 111 }
  LastWasFF := False;
  Result := AStream.Seek(BeginOffset, soFromBeginning);

  GetMem(LBuffer, CBufferSize);
  try
    while True do
    begin
      BytesRead := AStream.Read(LBuffer^, CBufferSize);
      if BytesRead = 0 then
      begin
        Result := -1;
        Break;
      end;

      for I := 0 to BytesRead - 1 do
      begin
        if LastWasFF and (Byte(LBuffer[I]) and $E0 = $E0) then
        begin
          Inc(Result, I - 1);
          if (I + BufferSize - 1 >= BytesRead) or (I = 0) then
          begin
            AStream.Seek(Result, soFromBeginning);
            if not AStream.Read(Buffer, BufferSize) = BufferSize then
              Result := -1;
          end
          else
            Move((LBuffer + I - 1)^, Buffer, BufferSize);

          Exit;
        end;

        LastWasFF := LBuffer[I] = #$FF;
      end;
      Inc(Result, BytesRead);
    end;
  finally
    FreeMem(LBuffer);
  end;
end;

function GetFrameIDLength(const Version: TJvID3Version): Byte;
begin
  case Version of
    ive2_2:
      Result := 3;
    ive2_3, ive2_4:
      Result := 4;
  else
    Result := 0;
    ID3Error(RsEID3UnknownVersion);
  end;
end;

function MajorVersionToVersion(const MajorVersion: Byte): TJvID3Version;
begin
  if MajorVersion < 2 then
    Result := iveLowerThan2_2
  else
  if MajorVersion = 2 then
    Result := ive2_2
  else
  if MajorVersion = 3 then
    Result := ive2_3
  else
  if MajorVersion = 4 then
    Result := ive2_4
  else
    Result := iveHigherThan2_4
end;

procedure RemoveUnsynchronisationScheme(Source, Dest: TStream; BytesToRead: Integer);
const
  MaxBufSize = $F000;
var
  LastWasFF: Boolean;
  BytesRead: Integer;
  SourcePtr, DestPtr: Integer;
  SourceBuf, DestBuf: PChar;
begin
  { Replace $FF 00 with $FF }

  GetMem(SourceBuf, Min(MaxBufSize, BytesToRead));
  GetMem(DestBuf, Min(MaxBufSize, BytesToRead));
  try
    LastWasFF := False;
    while BytesToRead > 0 do
    begin
      { Read at max CBufferSize bytes from the stream }
      BytesRead := Source.Read(SourceBuf^, Min(MaxBufSize, BytesToRead));
      if BytesRead = 0 then
        Id3Error(RsECouldNotReadData);

      Dec(BytesToRead, BytesRead);

      DestPtr := 0;
      SourcePtr := 0;

      while SourcePtr < BytesRead do
      begin
        { If previous was $FF and current is $00 then skip.. }
        if not LastWasFF or (SourceBuf[SourcePtr] <> #$00) then
        begin
          { ..otherwise copy }
          DestBuf[DestPtr] := SourceBuf[SourcePtr];
          Inc(DestPtr);
        end;

        LastWasFF := SourceBuf[SourcePtr] = #$FF;
        Inc(SourcePtr);
      end;
      Dest.Write(DestBuf^, DestPtr);
    end;
  finally
    FreeMem(DestBuf);
    FreeMem(SourceBuf);
  end;
end;

procedure ApplyUnsynchronisationScheme(Source, Dest: TStream; BytesToRead: Integer);
const
  MaxBufSize = $F000;
var
  LastWasFF: Boolean;
  BytesRead: Integer;
  SourcePtr, DestPtr: Integer;
  SourceBuf, DestBuf: PChar;
begin
  { Replace $FF 00         with  $FF 00 00
    Replace $FF %111xxxxx  with  $FF 00 %111xxxxx (%11100000 = $E0 = 224 }

  GetMem(SourceBuf, Min(MaxBufSize div 2, BytesToRead));
  GetMem(DestBuf, 2 * Min(MaxBufSize div 2, BytesToRead));
  try
    LastWasFF := False;
    while BytesToRead > 0 do
    begin
      { Read at max CBufferSize div 2 bytes from the stream }
      BytesRead := Source.Read(SourceBuf^, Min(MaxBufSize div 2, BytesToRead));
      if BytesRead = 0 then
        Id3Error(RsECouldNotReadData);

      Dec(BytesToRead, BytesRead);

      DestPtr := 0;
      SourcePtr := 0;

      while SourcePtr < BytesRead do
      begin
        { If previous was $FF and current is $00 or >=$E0 then add space.. }
        if LastWasFF and
          ((SourceBuf[SourcePtr] = #$00) or (Byte(SourceBuf[SourcePtr]) and $E0 > 0)) then
        begin
          DestBuf[DestPtr] := #$00;
          Inc(DestPtr);
        end;

        { Copy }
        DestBuf[DestPtr] := SourceBuf[SourcePtr];
        Inc(DestPtr);

        LastWasFF := SourceBuf[SourcePtr] = #$FF;
        Inc(SourcePtr);
      end;
      Dest.Write(DestBuf^, DestPtr);
    end;
  finally
    FreeMem(SourceBuf);
    FreeMem(DestBuf);
  end;
end;

//=== Global procedures ======================================================

{ Copied from DSDesign.pas }

function CreateUniqueName(AController: TJvID3Controller; const FrameName: string;
  FrameClass: TJvID3FrameClass; Component: TComponent): string;
var
  I: Integer;

  function IsUnique(const AName: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    with AController do
      for I := 0 to ComponentCount - 1 do
        if (Component <> Components[I]) and AnsiSameStr(AName, Components[I].Name) then
          Exit;
    Result := True;
  end;

begin
  for I := 1 to MaxInt do
  begin
    Result := GenerateName(AController, FrameName, FrameClass, I);
    if IsUnique(Result) then
      Break;
  end;
end;

function ExtToMIMEType(const Ext: string): string;
begin
  { Not a very reliable method }
  if SameText(Ext, '.jpeg') then
    Result := 'image/jpeg'
  else
  if SameText(Ext, '.tiff') then
    Result := 'image/tif'
  else
  if SameText(Ext, '.bmp') then
    Result := 'image/bitmap'
  else
  if Ext = '' then
    Result := 'image/'
  else
    { .png, .gif, .jpg etc. }
    Result := 'image/' + Copy(Ext, 2, MaxInt);
end;

{ References to the ID3v1 genres can be made by, as first byte, enter "("
  followed by a number from the genres list (appendix A) and ended with a ")"
  character. This is optionally followed by a refinement, e.g. "(21)" or
  "(4)Eurodisco". Several references can be made in the same frame, e.g.
  "(51)(39)". If the refinement should begin with a "(" character it should
  be replaced with "((", e.g. "((I can figure out any genre)" or
  "(55)((I think...)".

  The following new content types is defined in ID3v2 and is implemented in
  the same way as the numerig content types, e.g. "(RX)".

    RX        Remix
    CR        Cover
}
function GenreToNiceGenre(const AGenre: string): string;
var
  State: Integer;
  Start: Integer;
  I: Integer;

  procedure GoState0;
  begin
    State := 0;
    Start := I + 1;
  end;

  procedure AddString(const S: string);
  begin
    if Result > '' then
    begin
      if (S = '') or (S[1] = ' ') then
        Result := Result + S
      else
        Result := Result + ' ' + S;
    end
    else
      Result := S;
    GoState0;
  end;

  procedure AddReference(const AReference: string);
  var
    iReference: Integer;
    Genre: string;
  begin
    iReference := StrToIntDef(AReference, -1);
    if iReference < 0 then
    begin
      State := -1;
      Exit;
    end;

    Genre := ID3_IDToGenre(iReference);
    if Genre = '' then
    begin
      State := -1;
      Exit;
    end;

    AddString(ID3_IDToGenre(iReference));
    GoState0;
  end;

var
  P: PChar;
begin
  Result := '';
  State := 0;
  I := 1;
  Start := I;

  while (State >= 0) and (I <= Length(AGenre)) do
  begin
    case State of
      0:
        if AGenre[I] = '(' then
          State := 1
        else
          State := -1;
      1:
        case AGenre[I] of
          '(':
            begin
              Start := I;
              State := -1;
            end;
          '0'..'9':
            State := 2;
          'R':
            State := 3; // expect 'RX' = 'Remix'
          'C':
            State := 5; // expect 'CR' = 'Cover'
          ')':
            GoState0;
        else
          State := -1;
        end;
      2:
        case AGenre[I] of
          '0'..'9':
            ;
          ')':
            AddReference(Copy(AGenre, Start + 1, I - Start - 1));
        else
          State := -1;
        end;
      3:
        if AGenre[I] = 'X' then
          State := 4
        else
          State := -1;
      4:
        if AGenre[I] = ')' then
          AddString('Remix')
        else
          State := -1;
      5:
        if AGenre[I] = 'R' then
          State := 6
        else
          State := -1;
      6:
        if AGenre[I] = ')' then
          AddString('Cover')
        else
          State := -1;
    end;
    Inc(I);
  end;

  if Start <= Length(AGenre) then
  begin
    { Workaround for a bug in some taggers }
    P := PChar(AGenre) + Start - 1;
    while P^ = ' ' do Inc(P);
    if StrIComp(P, PChar(Result)) <> 0 then
      AddString(Copy(AGenre, Start, MaxInt));
  end;
end;

procedure GetID3v2Version(const AFileName: string; var HasTag: Boolean;
  var Version: TJvID3Version);
var
  Header: TID3v2HeaderRec;
begin
  with TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite) do
  try
    HasTag := (Read(Header, SizeOf(Header)) = SizeOf(Header)) and
      (Header.Identifier = cID3HeaderId);
    if not HasTag then
      Exit;

    Version := MajorVersionToVersion(Header.MajorVersion);
  finally
    Free;
  end;
end;

procedure ID3Error(const Msg: string; Component: TComponent = nil);
begin
  if Assigned(Component) and (Component.Name <> '') then
    raise EJvID3Error.CreateResFmt(@RsENameMsgFormat, [Component.Name, Msg])
  else
    raise EJvID3Error.Create(Msg);
end;

procedure ID3ErrorFmt(const Msg: string; const Args: array of const;
  Component: TComponent = nil);
begin
  ID3Error(Format(Msg, Args), Component);
end;

function MIMETypeToExt(const MIMEType: string): string;
begin
  { Not a very reliable method; maybe use Indy's TIdMimeTable
    in IdGlobal.pas

    See: ftp://ftp.isi.edu/in-notes/iana/assignments/media-types/media-types

    image/jpeg   .jpg    preferred     supported
    image/png    .png    preferred
    image/gif    .gif
    image/tiff   .tif
    image/x-pict .pic
    image/bitmap .bmp                  supported
  }
  Result := Copy(MIMEType, Pos('/', MIMEType) + 1, MaxInt);

  Result := AnsiLowerCase(Result);
  if Result = 'jpeg' then
    Result := '.jpg'
  else
  if Result = 'x-png' then
    Result := '.png'
  else
  if (Result = 'bitmap') or (Result = 'x-ms-bmp') then
    Result := '.bmp'
  else
  if Result = 'tiff' then
    Result := '.tif'
  else
  if Result = 'x-pict' then
    Result := '.pic'
  else
    Result := '.' + Result;
end;

function NiceGenreToGenre(const ANiceGenre: string): string;
var
  S: string;

  function IsPrefix(const APrefix: string): Boolean;
  var
    C: Integer;
  begin
    C := Length(APrefix);
    Result := ((C = Length(S)) or ((C < Length(S)) and (S[C + 1] = ' '))) and
      (AnsiStrLIComp(PChar(S), PChar(APrefix), C) = 0);
  end;

  procedure AddAndDelete(const Add: string; const DelCount: Integer);
  begin
    Result := Result + Add;
    Delete(S, 1, DelCount);
    while (S > '') and (S[1] = ' ') do
      Delete(S, 1, 1);
  end;

var
  GenreID: Integer;
begin
  Result := '';
  S := ANiceGenre;
  while S > '' do
  begin
    GenreID := ID3_LongGenreToID(S);
    if GenreID <> 255 then
      AddAndDelete(Format('(%d)', [GenreID]), Length(ID3_IDToGenre(GenreID)))
    else
    { Specials }
    if IsPrefix('remix') then
      AddAndDelete('(RX)', 5)
    else
    if IsPrefix('cover') then
      AddAndDelete('(CR)', 5)
    else
      Break;
  end;

  if S > '' then
  begin
    if S[1] = '(' then
      Result := Result + '(' + S
    else
      Result := Result + S;
  end;
end;

//=== TJvID3AudioEncryptionFrame =============================================

procedure TJvID3AudioEncryptionFrame.Assign(Source: TPersistent);
begin
  if Source is TJvID3AudioEncryptionFrame then
  begin
    FOwnerID := TJvID3AudioEncryptionFrame(Source).FOwnerID;
    FPreviewStart := TJvID3AudioEncryptionFrame(Source).FPreviewStart;
    FPreviewLength := TJvID3AudioEncryptionFrame(Source).FPreviewLength;
  end;

  inherited Assign(Source);
end;

class function TJvID3AudioEncryptionFrame.CanAddFrame(
  AController: TJvID3Controller; AFrameID: TJvID3FrameID): Boolean;
begin
  { There may be more than one "AENC" frames in a tag, but only one with the
    same 'Owner identifier' }
  Result := (AFrameID = fiAudioCrypto) or
    inherited CanAddFrame(AController, AFrameID);
end;

function TJvID3AudioEncryptionFrame.CheckFrame(
  const HandleError: TJvID3HandleError): Boolean;
begin
  Result := CheckIsURL(Self, FOwnerID, HandleError);

  { If something has changed update the framesize }
  if not Result and (HandleError = heAutoCorrect) then
  begin
    UpdateFrameSize;
    Result := True;
  end;
end;

procedure TJvID3AudioEncryptionFrame.Clear;
begin
  FOwnerID := '';
  FPreviewStart := 0;
  FPreviewLength := 0;
  inherited Clear;
end;

class function TJvID3AudioEncryptionFrame.Find(AController: TJvID3Controller;
  const AOwnerID: string): TJvID3AudioEncryptionFrame;
var
  Frame: TJvID3Frame;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  if not AController.FindFirstFrame(fiAudioCrypto, Frame) then
    Exit;

  while (Frame is TJvID3AudioEncryptionFrame) and
    (TJvID3AudioEncryptionFrame(Frame).OwnerID <> AOwnerID) do
    AController.FindNextFrame(fiAudioCrypto, Frame);

  if Frame is TJvID3AudioEncryptionFrame then
    Result := TJvID3AudioEncryptionFrame(Frame)
end;

class function TJvID3AudioEncryptionFrame.FindOrCreate(AController: TJvID3Controller;
  const AOwnerID: string): TJvID3AudioEncryptionFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController, AOwnerID);
  if not Assigned(Result) then
  begin
    Result := TJvID3AudioEncryptionFrame(AController.AddFrame(fiAudioCrypto));
    Result.OwnerID := AOwnerID;
  end;
end;

function TJvID3AudioEncryptionFrame.GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal;
begin
  { Owner identifier            <text string> $00
    Preview start               $xx xx
    Preview length              $xx xx
    Encryption info             <binary data>
  }
  Result := Cardinal(Length(FOwnerID)) + 1 + 2 + 2 + DataSize;
end;

function TJvID3AudioEncryptionFrame.GetIsEmpty: Boolean;
begin
  Result := inherited GetIsEmpty and (Length(FOwnerID) = 0) and
    (FPreviewStart = 0) and (FPreviewLength = 0);
end;

procedure TJvID3AudioEncryptionFrame.ReadFrame;
begin
  { Owner identifier            <text string> $00
    Preview start               $xx xx
    Preview length              $xx xx
    Encryption info             <binary data>
  }
  with Stream do
  begin
    ReadStringA(FOwnerID);

    if not CanRead(4) then
      Exit;

    Read(FPreviewStart, 2);
    FPreviewStart := ReverseBytes(FPreviewStart);

    Read(FPreviewLength, 2);
    FPreviewLength := ReverseBytes(FPreviewLength);

    ReadData(BytesTillEndOfFrame);
  end;
end;

function TJvID3AudioEncryptionFrame.SameUniqueIDAs(
  const Frame: TJvID3Frame): Boolean;
begin
  { There may be more than one "AENC" frames in a tag, but only one with the
    same 'Owner identifier' }
  Result := (Frame is TJvID3AudioEncryptionFrame) and
    (Frame.FrameID = FrameID) and (FrameID = fiAudioCrypto);

  if Result then
    Result := AnsiSameStr(TJvID3AudioEncryptionFrame(Frame).OwnerID, OwnerID)
  else
    Result := inherited SameUniqueIDAs(Frame);
end;

procedure TJvID3AudioEncryptionFrame.SetOwnerID(const Value: string);
begin
  if FOwnerID <> Value then
  begin
    FOwnerID := Value;
    Changed;
  end;
end;

procedure TJvID3AudioEncryptionFrame.SetPreviewLength(const Value: Word);
begin
  if FPreviewLength <> Value then
  begin
    FPreviewLength := Value;
    Changed;
  end;
end;

procedure TJvID3AudioEncryptionFrame.SetPreviewStart(const Value: Word);
begin
  if FPreviewStart <> Value then
  begin
    FPreviewStart := Value;
    Changed;
  end;
end;

procedure TJvID3AudioEncryptionFrame.WriteFrame;
var
  TempWord: Word;
begin
  { Owner identifier            <text string> $00
    Preview start               $xx xx
    Preview length              $xx xx
    Encryption info             <binary data>
  }
  with Stream do
  begin
    WriteStringA(OwnerID);
    WriteTerminatorA;

    TempWord := ReverseBytes(PreviewStart);
    Write(TempWord, 2);

    TempWord := ReverseBytes(PreviewLength);
    Write(TempWord, 2);

    WriteData;
  end;
end;

//=== TJvID3Base =============================================================

constructor TJvID3Base.Create(AController: TJvID3Controller);
begin
  inherited Create;
  FController := AController;
end;

procedure TJvID3Base.AfterConstruction;
begin
  inherited AfterConstruction;
  Reset;
end;

procedure TJvID3Base.Assign(Source: TPersistent);
begin
  if not Assigned(Source) then
    Reset
  else
    inherited Assign(Source);
end;

function TJvID3Base.GetStream: TJvID3Stream;
begin
  if not Assigned(FController) then
    ID3Error(RsEID3NoController);

  if icsUsingTempStream in FController.FState then
    Result := FController.FTempStream
  else
    Result := FController.FStream;
end;

//=== TJvID3BinaryFrame ======================================================

procedure TJvID3BinaryFrame.AfterConstruction;
begin
  inherited AfterConstruction;
  FData := nil;
  FDataSize := 0;
end;

procedure TJvID3BinaryFrame.Assign(Source: TPersistent);
begin
  if Source is TJvID3BinaryFrame then
    SetData(TJvID3BinaryFrame(Source).FData, TJvID3BinaryFrame(Source).DataSize);

  inherited Assign(Source);
end;

procedure TJvID3BinaryFrame.BeforeDestruction;
begin
  inherited BeforeDestruction;
  ReallocMem(FData, 0);
end;

class function TJvID3BinaryFrame.CanAddFrame(AController: TJvID3Controller;
  AFrameID: TJvID3FrameID): Boolean;
begin
  { There may only be one 'MCDI' frame in each tag. }
  Result := ((AFrameID = fiCDID) and not AController.HasFrame(fiCDID)) or
    inherited CanAddFrame(AController, AFrameID);
end;

function TJvID3BinaryFrame.CheckFrame(const HandleError: TJvID3HandleError): Boolean;
begin
  Result := True;
end;

procedure TJvID3BinaryFrame.Clear;
begin
  SetData(nil, 0);
  inherited Clear;
end;

class function TJvID3BinaryFrame.Find(AController: TJvID3Controller;
  const AFrameID: TJvID3FrameID): TJvID3BinaryFrame;
var
  Frame: TJvID3Frame;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  Frame := AController.Frames.FindFrame(AFrameID);
  if Frame is TJvID3BinaryFrame then
    Result := TJvID3BinaryFrame(Frame);
end;

class function TJvID3BinaryFrame.FindOrCreate(AController: TJvID3Controller;
  const AFrameID: TJvID3FrameID): TJvID3BinaryFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController, AFrameID);
  if not Assigned(Result) then
  begin
    AController.CheckFrameClass(TJvID3BinaryFrame, AFrameID);
    Result := TJvID3BinaryFrame(AController.AddFrame(AFrameID));
  end;
end;

function TJvID3BinaryFrame.GetData(P: Pointer; const Size: Cardinal): Boolean;
var
  CopySize: Cardinal;
begin
  Result := Assigned(P);
  if not Result then
    Exit;

  CopySize := Min(Size, DataSize);
  if (CopySize > 0) and Assigned(FData) then
    Move(FData^, P^, CopySize);
end;

function TJvID3BinaryFrame.GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal;
begin
  Result := FDataSize;
end;

function TJvID3BinaryFrame.GetIsEmpty: Boolean;
begin
  Result := DataSize = 0;
end;

procedure TJvID3BinaryFrame.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvID3BinaryFrame.LoadFromStream(Stream: TStream);
begin
  Stream.Position := 0;
  FDataSize := Stream.Size;
  ReallocMem(FData, FDataSize);
  if Assigned(FData) then
    Stream.Read(FData^, FDataSize);
  Changed;
end;

procedure TJvID3BinaryFrame.ReadData(ASize: Cardinal);
begin
  {if ASize < 0 then
    ASize := 0;}

  FDataSize := ASize;
  ReallocMem(FData, FDataSize);

  if Assigned(FData) and (FDataSize > 0) then
    with Stream do
      Read(FData^, FDataSize);
end;

procedure TJvID3BinaryFrame.ReadFrame;
begin
  ReadData(FFrameSize);
end;

function TJvID3BinaryFrame.SameUniqueIDAs(const Frame: TJvID3Frame): Boolean;
begin
  { There may only be one 'MCDI' frame in each tag. }
  Result := (Assigned(Frame) and (Frame.FrameID = FrameID) and
    (FrameID = fiCDID)) or inherited SameUniqueIDAs(Frame);
end;

procedure TJvID3BinaryFrame.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvID3BinaryFrame.SaveToStream(Stream: TStream);
begin
  if (DataSize > 0) and Assigned(FData) then
    Stream.Write(FData^, DataSize)
end;

function TJvID3BinaryFrame.SetData(P: Pointer; const Size: Cardinal): Boolean;
begin
  Result := Assigned(P) or (Size = 0);
  if not Result then
    Exit;

  ReallocMem(FData, Size);
  FDataSize := Size;
  if Assigned(FData) and Assigned(P) then
    Move(P^, FData^, FDataSize);
  Changed;
end;

procedure TJvID3BinaryFrame.WriteData;
begin
  if Assigned(FData) then
    with Stream do
      Write(FData^, DataSize);
end;

procedure TJvID3BinaryFrame.WriteFrame;
begin
  WriteData;
end;

//=== TJvID3ContentFrame =====================================================

procedure TJvID3ContentFrame.Assign(Source: TPersistent);
begin
  if Source is TJvID3ContentFrame then
  begin
    FLanguage := TJvID3ContentFrame(Source).FLanguage;
    CopyStringPair(TJvID3ContentFrame(Source).FText, FText);
    CopyStringPair(TJvID3ContentFrame(Source).FDescription, FDescription);
  end;

  inherited Assign(Source);
end;

class function TJvID3ContentFrame.CanAddFrame(AController: TJvID3Controller;
  AFrameID: TJvID3FrameID): Boolean;
begin
  { There may be more than one comment frame in each tag, but only one with
    the same language and content descriptor.
    There may be more than one 'Unsynchronised lyrics/text transcription' frame
    in each tag, but only one with the same language and content descriptor.
  }
  Result := (AFrameID in [fiComment, fiUnsyncedLyrics]) or
    inherited CanAddFrame(AController, AFrameID);
end;

function TJvID3ContentFrame.CheckFrame(const HandleError: TJvID3HandleError): Boolean;
begin
  Result := CheckIsLanguageA(Self, FLanguage, HandleError);

  { If something has changed update the framesize }
  if not Result and (HandleError = heAutoCorrect) then
  begin
    UpdateFrameSize;
    Result := True;
  end;
end;

procedure TJvID3ContentFrame.Clear;
begin
  FLanguage := '';
  ClearStringPair(FText);
  ClearStringPair(FDescription);

  inherited Clear;
end;

class function TJvID3ContentFrame.Find(AController: TJvID3Controller;
  const AFrameID: TJvID3FrameID): TJvID3ContentFrame;
var
  Frame: TJvID3Frame;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  Frame := AController.Frames.FindFrame(AFrameID);
  if Frame is TJvID3ContentFrame then
    Result := TJvID3ContentFrame(Frame);
end;

class function TJvID3ContentFrame.FindOrCreate(AController: TJvID3Controller;
  const AFrameID: TJvID3FrameID): TJvID3ContentFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController, AFrameID);
  if not Assigned(Result) then
  begin
    AController.CheckFrameClass(TJvID3ContentFrame, AFrameID);
    Result := TJvID3ContentFrame(AController.AddFrame(AFrameID));
  end;
end;

function TJvID3ContentFrame.GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal;
begin
  { Text encoding            $xx
    Language                 $xx xx xx
    Short content descrip.   <text string according to encoding> $00 (00)
    The actual text          <full text string according to encoding>
  }
  Result := 1 + 3 +
    LengthEnc(FDescription, Encoding, ToEncoding) +
    LengthTerminatorEnc(ToEncoding) +
    LengthEnc(FText, Encoding, ToEncoding);
end;

function TJvID3ContentFrame.GetIsEmpty: Boolean;
begin
  Result := ((Length(FLanguage) = 0) or (FLanguage = cUnknownLanguage)) and
    CheckIsEmpty(FText, Encoding) and CheckIsEmpty(FDescription, Encoding);
end;

procedure TJvID3ContentFrame.ReadFrame;
begin
  { Text encoding            $xx
    Language                 $xx xx xx
    Short content descrip.   <text string according to encoding> $00 (00)
    The actual text          <full text string according to encoding>
  }

  with Stream do
  begin
    ReadEncoding;
    ReadLanguage(FLanguage);
    ReadStringEnc(FDescription);
    ReadStringEnc(FText);
  end;
end;

function TJvID3ContentFrame.SameUniqueIDAs(const Frame: TJvID3Frame): Boolean;
begin
  { There may be more than one comment frame in each tag, but only one with
    the same language and content descriptor.
    There may be more than one 'Unsynchronised lyrics/text transcription' frame
    in each tag, but only one with the same language and content descriptor.
  }
  Result := (Frame is TJvID3ContentFrame) and
    (Frame.FrameID = FrameID) and (FrameID in [fiComment, fiUnsyncedLyrics]);

  if Result then
    Result :=
      AnsiSameStr(TJvID3ContentFrame(Frame).Language, Language) and
      SameStringPair(TJvID3ContentFrame(Frame).FDescription, FDescription,
      TJvID3ContentFrame(Frame).Encoding, Encoding)
  else
    Result := inherited SameUniqueIDAs(Frame);
end;

procedure TJvID3ContentFrame.SetDescription(const Value: string);
begin
  if FDescription.SA <> Value then
  begin
    FDescription.SA := Value;
    Changed;
  end;
end;

procedure TJvID3ContentFrame.SetDescriptionW(const Value: WideString);
begin
  if FDescription.SW <> Value then
  begin
    FDescription.SW := Value;
    Changed;
  end;
end;

procedure TJvID3ContentFrame.SetLanguage(const Value: string);
begin
  if FLanguage <> Value then
  begin
    FLanguage := Value;
    Changed;
  end;
end;

procedure TJvID3ContentFrame.SetText(const Value: string);
begin
  if FText.SA <> Value then
  begin
    FText.SA := Value;
    Changed;
  end;
end;

procedure TJvID3ContentFrame.SetTextW(const Value: WideString);
begin
  if FText.SW <> Value then
  begin
    FText.SW := Value;
    Changed;
  end;
end;

procedure TJvID3ContentFrame.WriteFrame;
begin
  { Text encoding            $xx
    Language                 $xx xx xx
    Short content descrip.   <text string according to encoding> $00 (00)
    The actual text          <full text string according to encoding>
  }

  with Stream do
  begin
    WriteEncoding;
    WriteLanguage(Language);
    WriteStringEnc(FDescription);
    WriteTerminatorEnc;
    WriteStringEnc(FText);
  end;
end;

//=== TJvID3Controller =======================================================

constructor TJvID3Controller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFrames := TJvID3Frames.Create(Self);
  FHeader := TJvID3Header.Create(Self);
  FExtendedHeader := TJvID3ExtendedHeader.Create(Self);
  FFileInfo := TJvID3FileInfo.Create;

  FActivateEvents := TList.Create;
  FClients := TList.Create;
  FState := [];

  { Defaults }
  FReadEncodingAs := ifeDontCare;
  FWriteEncodingAs := ifeDontCare;
  FReadVersionAs := ifvDontCare;
  FWriteVersionAs := ifvDontCare;
  FOptions := [coAutoCorrect, coRemoveEmptyFrames];
end;

destructor TJvID3Controller.Destroy;
begin
  SetActive(False);

  inherited Destroy;

  FreeAndNil(FActivateEvents);
  FreeAndNil(FClients);

  FDesigner.Free;
  FDesigner := nil;

  FreeAndNil(FFrames);
  FHeader.Free;
  FExtendedHeader.Free;
  FFileInfo.Free;
  FStream.Free;
end;

function TJvID3Controller.AddFrame(const AFrameID: TJvID3FrameID): TJvID3Frame;
var
  FrameClass: TJvID3FrameClass;
begin
  if not Active and not (icsReading in FState) then
    ID3Error(RsEID3ControllerNotActive, Self);

  FrameClass := GetFrameClass(AFrameID);

  Result := FrameClass.Create(Self, AFrameID);
  try
    Result.Name := CreateUniqueName(Self, Result.FrameName, FrameClass, Result);
    Result.Controller := Self;
  except
    Result.Free;
    { Suppress errors while reading }
    if not (icsReading in FState) then
      raise;
  end;
end;

procedure TJvID3Controller.ApplyUnsynchronisationSchemeOnCurrentStream;
var
  TmpStream: TMemoryStream;
  LTempStreamSize: Cardinal;
begin
  TmpStream := TMemoryStream.Create;
  try
    if icsUsingTempStream in FState then
    begin
      if not Assigned(FTempStream) then
        Id3Error(RsENoTempStream, Self);

      LTempStreamSize := GetTempStreamSize;
      FTempStream.Seek(0, soFromBeginning);
      ApplyUnsynchronisationScheme(FTempStream, TmpStream, LTempStreamSize);
      TmpStream.Seek(0, soFromBeginning);
      FTempStream.Seek(0, soFromBeginning);
      FTempStream.CopyFrom(TmpStream, TmpStream.Size);
    end
    else
    begin
      { Exclude header (size=10) from the unsynchronisation }
      FStream.Seek(10, soFromBeginning);
      ApplyUnsynchronisationScheme(FStream, TmpStream, FStream.Size - 10);
      TmpStream.Seek(0, soFromBeginning);
      FStream.Seek(10, soFromBeginning);
      FStream.CopyFrom(TmpStream, TmpStream.Size);
    end;
  finally
    TmpStream.Free;
  end;
end;

procedure TJvID3Controller.BeginReading;
begin
  if FState <> [] then
    Id3Error(RsEAlreadyReadingWriting, Self);

  Include(FState, icsReading);
  FStream := TJvID3Stream.Create;

  BeginUpdate;
end;

procedure TJvID3Controller.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TJvID3Controller.BeginUseTempStream;
begin
  if icsUsingTempStream in FState then
    Id3Error(RsEAlreadyUsingTempStream, Self);

  Include(FState, icsUsingTempStream);
  if not Assigned(FTempStream) then
    FTempStream := TJvID3Stream.Create;

  FTempStream.Seek(0, soFromBeginning);

  { Init FTempStream as FStream }
  FTempStream.FSourceEncoding := FStream.FSourceEncoding;
  FTempStream.FDestEncoding := FStream.FDestEncoding;
  FTempStream.FAllowedEncodings := FStream.FAllowedEncodings;
end;

procedure TJvID3Controller.BeginWriting;
begin
  if FState <> [] then
    Id3Error(RsEAlreadyReadingWriting, Self);

  Include(FState, icsWriting);
  FStream := TJvID3Stream.Create;

  BeginUpdate;
end;

function TJvID3Controller.CanAddFrame(const AFrameID: TJvID3FrameID): Boolean;
var
  FrameClass: TJvID3FrameClass;
begin
  { While reading we can always add all kinds of frames, ie we accept that the
    stream may contain errors }
  if icsReading in FState then
  begin
    Result := True;
    Exit;
  end;

  FrameClass := GetFrameClass(AFrameID);
  if Assigned(FrameClass) then
    Result := FrameClass.CanAddFrame(Self, AFrameID)
  else
    Result := False;
end;

procedure TJvID3Controller.ChangeToVersion(const ANewVersion: TJvID3Version);
begin
  Frames.ChangeToVersion(ANewVersion);
  Header.ChangeToVersion(ANewVersion);
  ExtendedHeader.ChangeToVersion(ANewVersion);
end;

procedure TJvID3Controller.CheckFrameClass(FrameClass: TJvID3FrameClass;
  const AFrameID: TJvID3FrameID);
var
  LFrameClass: string;
begin
  if FrameClass <> GetFrameClass(AFrameID) then
  begin
    if Assigned(FrameClass) then
      LFrameClass := FrameClass.ClassName
    else
      LFrameClass := '';
    ID3ErrorFmt(RsEID3InvalidFrameClass, [LFrameClass, ID3_FrameIDToString(AFrameID)], Self);
  end;
end;

procedure TJvID3Controller.Close;
begin
  SetActive(False);
end;

procedure TJvID3Controller.Commit;
const
  CHandleError: array [Boolean] of TJvID3HandleError = (heRaise, heAutoCorrect);
begin
  if not Active then
    ID3Error(RsEID3ControllerNotActive);

  try
    if coRemoveEmptyFrames in Options then
      FFrames.RemoveEmptyFrames;
    FFrames.CheckFrames(CHandleError[coAutoCorrect in Options]);

    SaveToFile(FFileName);
    SetModified(False);
  except
    {$IFDEF COMPILER6_UP}
    if csDesigning in ComponentState then
      if Assigned(Classes.ApplicationHandleException) then
        Classes.ApplicationHandleException(ExceptObject)
      else
        ShowException(ExceptObject, ExceptAddr)
    else
      raise;
    {$ELSE}
    Application.HandleException(ExceptObject);
    {$ENDIF COMPILER6_UP}
  end;
end;

function TJvID3Controller.CopyFromID3v1(const DoOverwrite: Boolean): Boolean;
var
  ID3v1Ctrl: TJvID3v1;
begin
  if not Active then
    ID3Error(RsEID3ControllerNotActive, Self);

  ID3v1Ctrl := TJvID3v1.Create(nil);
  try
    ID3v1Ctrl.FileName := FileName;
    ID3v1Ctrl.Open;
    Result := ID3v1Ctrl.HasTag;
    if Result then
      CopyFromID3v1Ctrl(ID3v1Ctrl, DoOverwrite);
  finally
    ID3v1Ctrl.Free;
  end;
end;

procedure TJvID3Controller.CopyFromID3v1Ctrl(AID3v1: TJvID3v1;
  const DoOverwrite: Boolean);
var
  Frame: TJvID3Frame;
  SP: TJvID3StringPair;
  Year: Word;

  function GetFrame(AFrameID: TJvID3FrameID): TJvID3Frame;
  begin
    Result := FFrames.FindFrame(AFrameID);
    if Assigned(Result) and not DoOverwrite then
      { If the frame already exists, and we don't want to overwrite, return nil }
      Result := nil
    else
    if not Assigned(Result) then
      { If the frame does not exists, create one }
      Result := AddFrame(AFrameID);
  end;

begin
  { There is a lot of extra code, because it may be possible that some frame
    is not encoded in ISO-8859-1 }

  if not Assigned(AID3v1) then
    Exit;

  // Songname
  Frame := GetFrame(fiTitle);
  if Assigned(Frame) then
    SetStringA(TJvID3TextFrame(Frame).FText, Frame.Encoding, AID3v1.SongName);

  // Artist
  Frame := GetFrame(fiLeadArtist);
  if Assigned(Frame) then
  begin
    SetStringA(SP, Frame.Encoding, AID3v1.Artist);
    TJvID3CustomTextFrame(Frame).NewText(SP);
  end;

  // Album
  Frame := GetFrame(fiAlbum);
  if Assigned(Frame) then
    SetStringA(TJvID3TextFrame(Frame).FText, Frame.Encoding, AID3v1.Album);

  // Year
  Year := StrToIntDef(AID3v1.Year, 0);
  if Year > 0 then
  begin
    if Version = ive2_4 then
    begin
      Frame := GetFrame(fiRecordingTime);
      if Assigned(Frame) then
        TJvID3TimestampFrame(Frame).FValue := EncodeDate(Year, 1, 1);
    end
    else
    begin
      Frame := GetFrame(fiYear);
      if Assigned(Frame) then
        TJvID3NumberFrame(Frame).FValue := Year;
    end;
  end;

  // Comment
  Frame := GetFrame(fiComment);
  if Assigned(Frame) then
    SetStringA(TJvID3ContentFrame(Frame).FText, Frame.Encoding, AID3v1.Comment);

  // Genre
  Frame := GetFrame(fiContentType);
  if Assigned(Frame) then
  begin
    if AID3v1.Genre = 255 then
      SetStringA(TJvID3TextFrame(Frame).FText, Frame.Encoding, '')
    else
      SetStringA(TJvID3TextFrame(Frame).FText, Frame.Encoding,
        Format('(%d)', [AID3v1.Genre]));
  end;

  // AlbumTrack
  if AID3v1.AlbumTrack > 0 then
  begin
    Frame := GetFrame(fiTrackNum);
    if Assigned(Frame) then
      SetStringA(TJvID3TextFrame(Frame).FText, Frame.Encoding, IntToStr(AID3v1.AlbumTrack));
  end;
end;

function TJvID3Controller.CopyToID3v1(const DoOverwrite: Boolean): Boolean;
var
  ID3v1Ctrl: TJvID3v1;
begin
  if not Active then
    ID3Error(RsEID3ControllerNotActive, Self);

  ID3v1Ctrl := TJvID3v1.Create(nil);
  try
    ID3v1Ctrl.FileName := FileName;
    ID3v1Ctrl.Open;
    CopyToID3v1Ctrl(ID3v1Ctrl, DoOverwrite);
    Result := ID3v1Ctrl.Commit;
  finally
    ID3v1Ctrl.Free;
  end;
end;

procedure TJvID3Controller.CopyToID3v1Ctrl(AID3v1: TJvID3v1;
  const DoOverwrite: Boolean);
var
  S: string;
  SP: TJvID3StringPair;
  Frame: TJvID3Frame;
  Track, P: Integer;
  I: Integer;
  YearSet, CommentSet: Boolean;
begin
  { There is a lot of extra code, because it may be possible that some frame
    is not encoded in ISO-8859-1 }

  if not Assigned(AID3v1) then
    Exit;

  YearSet := False;
  CommentSet := False;

  for I := 0 to FrameCount - 1 do
  begin
    Frame := FFrames[I];

    with AID3v1 do
      case Frame.FrameID of
        fiTitle:
          if DoOverwrite or (SongName = '') then
            SongName := Copy(GetStringA(TJvID3TextFrame(Frame).FText, Frame.Encoding), 1, 30);
        fiLeadArtist:
          if DoOverwrite or (Artist = '') then
          begin
            { Note: fiLeadArtist has multiple lines }
            TJvID3CustomTextFrame(Frame).GetText(SP);
            S := GetStringA(SP, Frame.Encoding);
            Artist := Copy(S, 1, 30);
          end;
        fiAlbum:
          if DoOverwrite or (Album = '') then
            Album := Copy(GetStringA(TJvID3TextFrame(Frame).FText, Frame.Encoding), 1, 30);
        fiYear:
          if not YearSet and (DoOverwrite or (Year = '')) then
          begin
            Year := Format('%.4d', [TJvID3NumberFrame(Frame).Value]);
            YearSet := True;
          end;
        fiRecordingTime:
          if not YearSet and (DoOverwrite or (Year = '')) then
          begin
            Year := Format('%.4d', [YearOfDate(TJvID3TimeStampFrame(Frame).Value)]);
            YearSet := True;
          end;
        fiComment:
          { Note : there may be more than 1 fiComment frame in the tag, just
                   pick the first we encounter }
          if not CommentSet and (DoOverwrite or (SongName = '')) then
          begin
            Comment := Copy(GetStringA(TJvID3ContentFrame(Frame).FText, Frame.Encoding), 1, 30);
            CommentSet := True;
          end;
        fiContentType:
          if DoOverwrite or (Genre = 255) then
            Genre := ID3_LongGenreToID(
              GetStringA(TJvID3TextFrame(Frame).FText, Frame.Encoding));
        fiTrackNum:
          if DoOverwrite or (AlbumTrack = 0) then
          begin
            S := GetStringA(TJvID3TextFrame(Frame).FText, Frame.Encoding);
            P := Pos('/', S);
            if P > 0 then
              Track := StrToIntDef(Copy(S, 1, P - 1), 0)
            else
              Track := StrToIntDef(S, 0);
            if (Track < 0) or (Track > 255) then
              Track := 0;

            AlbumTrack := Byte(Track);
          end;
      end;
  end;
end;

procedure TJvID3Controller.DoClose;
begin
  { Note: this will set Modified to True.. }
  Frames.Clear;
  FFileInfo.Reset;
  FActive := False;

  { ..thus we set it now back to false }
  SetModified(False);
end;

procedure TJvID3Controller.DoOpen;
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(FileStream);
    FActive := True;

    if ReadVersionAs <> ifvDontCare then
      FFrames.ChangeToVersion(CForceVersionToVersion[ReadVersionAs]);
  finally
    FileStream.Free;
  end;
end;

procedure TJvID3Controller.EndReading;
begin
  if not (icsReading in FState) then
    Id3Error(RsENotReading, Self);

  Exclude(FState, icsReading);
  FreeAndNil(FStream);
  FreeAndNil(FTempStream);

  EndUpdate;
end;

procedure TJvID3Controller.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    ID3Event(ideID3Change, 0);
end;

procedure TJvID3Controller.EndUseTempStream;
begin
  if not (icsUsingTempStream in FState) then
    Id3Error(RsENotUsingTempStream, Self);

  Exclude(FState, icsUsingTempStream);
  { Do not free the temp stream }
end;

procedure TJvID3Controller.EndWriting;
begin
  if not (icsWriting in FState) then
    Id3Error(RsENotWriting, Self);

  Exclude(FState, icsWriting);
  FreeAndNil(FStream);
  FreeAndNil(FTempStream);

  EndUpdate;
end;

procedure TJvID3Controller.EnsureExists(const FrameIDs: TJvID3FrameIDs);
var
  FrameID: TJvID3FrameID;
  IDs: TJvID3FrameIDs;
begin
  if not Active then
    ID3Error(RsEID3ControllerNotActive, Self);

  IDs := FrameIDs - FFrames.GetFrameIDs;
  { IDs represents a set of frames we have to construct }

  if IDs <> [] then
    for FrameID := Low(TJvID3FrameID) to High(TJvID3FrameID) do
      if (FrameID in IDs) and not (GetFrameClass(FrameID) = TJvID3SkipFrame) then
        AddFrame(FrameID);
end;

procedure TJvID3Controller.Erase;
var
  SavedActive: Boolean;
begin
  SavedActive := Active;
  Close;
  ChangeTagSize(FileName, 0);

  if SavedActive then
  begin
    Open;
    { Force Modified to be True }
    SetModified(True);
  end;
end;

function TJvID3Controller.FindFirstFrame(const AFrameID: TJvID3FrameID;
  var Frame: TJvID3Frame): Boolean;
begin
  Frame := nil;
  Result := FindNextFrame(AFrameID, Frame);
end;

function TJvID3Controller.FindNextFrame(const AFrameID: TJvID3FrameID;
  var From: TJvID3Frame): Boolean;
var
  I: Integer;
begin
  if From = nil then
  begin
    From := Frames.FindFrame(AFrameID);
    Result := Assigned(From);
  end
  else
  begin
    Result := True;
    I := From.Index + 1;
    while I < FrameCount do
    begin
      From := Frames[I];
      if From.FrameID = AFrameID then
        Exit;
      Inc(I);
    end;
    Result := False;
    From := nil;
  end;
end;

class function TJvID3Controller.GetFrameClass(const FrameID: TJvID3FrameID): TJvID3FrameClass;
begin
  Result := DefaultFrameClasses[FrameID];
  if not Assigned(Result) then
    { TJvID3SkipFrame is the default frame for non-implemented frames }
    Result := TJvID3SkipFrame;
end;

function TJvID3Controller.GetFrameCount: Integer;
begin
  Result := Frames.Count;
end;

function TJvID3Controller.GetFrameCountFor(const AFrameID: TJvID3FrameID): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FrameCount - 1 do
    if Frames[I].FrameID = AFrameID then
      Inc(Result);
end;

function TJvID3Controller.GetReadVersion: TJvID3Version;
begin
  { Returns the end-version (2.3 or 2.4) of a tag when reading. For example
    a tag can have version 2.3 (on disk) but when ReadVersionAs is set to ifv2_4
    it will be translated to a v2.4 tag, and ReadVersion will return ive2_4 in
    this case }

  case ReadVersionAs of
    ifvDontCare:
      begin
        Result := Version;
        if Result < ive2_2 then
          Result := ive2_2
        else
        if Result > ive2_4 then
          Result := ive2_4;
      end;
    ifv2_2:
      Result := ive2_2;
    ifv2_3:
      Result := ive2_3;
    ifv2_4:
      Result := ive2_4;
  else
    Result := ive2_3;
    ID3Error(RsEID3UnknownVersion, Self);
  end;
end;

function TJvID3Controller.GetTagSize: Cardinal;
begin
  if not Active then
    Result := 0
  else
    Result := Header.Size;
end;

function TJvID3Controller.GetTempStreamSize: Cardinal;
begin
  if not Assigned(FTempStream) then
    Id3Error(RsENoTempStream, Self);

  Result := FTempStream.Position;
end;

function TJvID3Controller.GetVersion: TJvID3Version;
begin
  Result := MajorVersionToVersion(FHeader.MajorVersion);
end;

function TJvID3Controller.GetWriteVersion: TJvID3Version;
begin
  { Returns the end-version (2.3 or 2.4) of a tag when writing. For example
    a tag can have version 2.3 but when WriteVersionAs is set to ifv2_4 it will
    be translated to a v2.4 tag, and WriteVersion will return ive2_4 in this
    case }

  case WriteVersionAs of
    ifvDontCare:
      begin
        Result := Version;
        { Default to v2.4; latest version }
        if (Result < ive2_2) or (Result > ive2_4) then
          Result := ive2_4;
      end;
    ifv2_2:
      Result := ive2_2;
    ifv2_3:
      Result := ive2_3;
    ifv2_4:
      Result := ive2_4;
  else
    Result := ive2_3;
    ID3Error(RsEID3UnknownVersion, Self);
  end;
end;

function TJvID3Controller.HasFrame(const AFrameID: TJvID3FrameID): Boolean;
begin
  Result := Assigned(Frames.FindFrame(AFrameID));
end;

procedure TJvID3Controller.ID3Event(Event: TJvID3Event; Info: Integer);
begin
  if (Event in [ideFrameChange, ideFrameListChange]) and
    (FState * [icsReading, icsWriting] = []) then
    SetModified(True);

  if (FUpdateCount = 0) and Assigned(FDesigner) then
    FDesigner.ID3Event(Event, Info);
end;

procedure TJvID3Controller.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive then
      SetActive(True);
  except
    {$IFDEF COMPILER6_UP}
    if csDesigning in ComponentState then
      if Assigned(Classes.ApplicationHandleException) then
        Classes.ApplicationHandleException(ExceptObject)
      else
        ShowException(ExceptObject, ExceptAddr)
    else
      raise;
    {$ELSE}
    Application.HandleException(ExceptObject);
    {$ENDIF COMPILER6_UP}
  end;
end;

procedure TJvID3Controller.LoadFromStream(AStream: TStream);
begin
  BeginReading;
  try
    { Clear }
    FHeader.Reset;
    FExtendedHeader.Reset;
    FFrames.Reset;

    { Read the header }
    if AStream.Size >= 10 then
      FStream.ReadFromStream(AStream, 10)
    else
      Exit;

    { Parse the header }
    FHeader.Read;

    if FHeader.HasTag and (Version in CSupportedVersions) then
    begin

      { Init encoding after the version is read }
      FStream.InitAllowedEncodings(ReadVersion, ReadEncodingAs);

      { Note that we will overwrite the header in FStream (first 10 bytes in FStream) }
      FStream.Position := 0;

      if hfUnsynchronisation in FHeader.Flags then
        { Unsynchronisation scheme is applied to the tag, we have to remove it,
          ie replace $FF $00 with $FF }
        RemoveUnsynchronisationScheme(AStream, FStream, FHeader.Size)
      else
        { If not, we just copy the stream to the memory stream }
        FStream.ReadFromStream(AStream, FHeader.Size);

      FStream.Position := 0;

      if hfExtendedHeader in FHeader.Flags then
        { Read extended header, note that it's read after the unsynchronisation
          scheme is removed }
        FExtendedHeader.Read;

      FFrames.Read;
    end;

    if Header.HasTag then
      FFileInfo.Read(AStream, 10 + Header.Size)
    else
      FFileInfo.Read(AStream, 0);
  finally
    EndReading;
  end;
end;

procedure TJvID3Controller.Open;
begin
  SetActive(True);
end;

procedure TJvID3Controller.RegisterClient(Client: TObject;
  Event: TJvID3ActivateChangeEvent);
begin
  { Based on TCustomConnection.RegisterClient }
  FClients.Add(Client);
  FActivateEvents.Add(TMethod(Event).Code);
end;

procedure TJvID3Controller.RemoveUnsynchronisationSchemeToTempStream(const ASize: Integer);
begin
  if icsUsingTempStream in FState then
    Id3Error(RsEAlreadyUsingTempStream, Self);

  if not Assigned(FTempStream) then
    FTempStream := TJvID3Stream.Create;

  FTempStream.Seek(0, soFromBeginning);
  RemoveUnsynchronisationScheme(FStream, FTempStream, ASize);
end;

procedure TJvID3Controller.SaveToFile(const AFileName: string);
var
  PaddingSize: Integer;
  OldTagSizeInclHeader: Cardinal;
  NewTagSizeInclHeader: Cardinal;
  FileStream: TFileStream;

  { Normally Tagsize is the size of the tag including padding excluding header, so
    we have vars

    xxxTagSizeInclHeader            = normal Tagsize + 10 (if tag exists)
                                    = 0                   (if tag doesn't exists)
    xxxTagSizeInclHeaderExclPadding = normal Tagsize + 10 - size of the padding (if tag exists)
                                    = 0                   (if tag doesn't exists)
  }
  function CalcNewPadding(const AOldTagSizeInclHeader: Cardinal;
    const ANewTagSizeInclHeaderExclPadding: Cardinal): Cardinal;
  const
    CMinPadding = $800; // = 2048
    CChunk = $800;
  var
    NewTagSizeInclHeader: Cardinal;
  begin
    Assert(AOldTagSizeInclHeader <= ANewTagSizeInclHeaderExclPadding);

    if AOldTagSizeInclHeader = 0 then
      Result := CMinPadding
    else
    begin
      NewTagSizeInclHeader := AOldTagSizeInclHeader;
      { ?? }
      while NewTagSizeInclHeader <= ANewTagSizeInclHeaderExclPadding do
        Inc(NewTagSizeInclHeader, 1 + NewTagSizeInclHeader div 2);

      Result := NewTagSizeInclHeader - ANewTagSizeInclHeaderExclPadding;
      if Result < CMinPadding then
        Result := CMinPadding;
    end;

    NewTagSizeInclHeader := ANewTagSizeInclHeaderExclPadding + Result;
    { Round to multiple of CChunk }
    NewTagSizeInclHeader := ((NewTagSizeInclHeader + CChunk - 1) div CChunk) * CChunk;
    Result := NewTagSizeInclHeader - ANewTagSizeInclHeaderExclPadding;
  end;

begin
  BeginWriting;
  try
    FStream.InitAllowedEncodings(WriteVersion, WriteEncodingAs);

    { Maybe only write header to the filestream? }
    Header.Write;

    if hfExtendedHeader in FHeader.Flags then
      { Write extended header, note that it's written before the unsynchronisation
        scheme is applied }
      FExtendedHeader.Write;

    FFrames.Write;

    { Compression }

    { Encryption }

    if hfUnsynchronisation in Header.Flags then
      ApplyUnsynchronisationSchemeOnCurrentStream;

    FileStream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareExclusive);
    try
      OldTagSizeInclHeader := GetTagSizeInclHeader(FileStream);

      { FStream.Size = size of new tag including header excluding padding }
      PaddingSize := OldTagSizeInclHeader - Cardinal(FStream.Size);

      { We always want to have padding (because of possible
        unsynchronisation possibly needs padding), thus if PaddingSize = 0, then
        also calculate new bigger padding size }
      if PaddingSize <= 0 then
        PaddingSize := CalcNewPadding(OldTagSizeInclHeader, FStream.Size);

      NewTagSizeInclHeader := FStream.Size + PaddingSize;
      if NewTagSizeInclHeader < OldTagSizeInclHeader then
        Inc(PaddingSize, OldTagSizeInclHeader - NewTagSizeInclHeader)
      else
      if NewTagSizeInclHeader > OldTagSizeInclHeader then
      begin
        { (rb) This is a bit clumbsy, we have to throw away the stream before
          resizing, then resize the file, and afterward construct the stream
          again.

          Couldn't come up with a cleaner way
        }

        FreeAndNil(FileStream);

        ChangeTagSize(FileName, NewTagSizeInclHeader);

        FileStream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareExclusive);
      end;

      { Write the padding }
      FStream.Seek(0, soFromEnd);
      FStream.WritePadding(PaddingSize);

      { Update header & write it again to the stream }
      Header.FSize := NewTagSizeInclHeader - 10;
      FStream.Seek(0, soFromBeginning);
      Header.Write;

      { Write the memory stream to the file }
      FStream.Seek(0, soFromBeginning);
      FileStream.Seek(0, soFromBeginning);
      FileStream.CopyFrom(FStream, FStream.Size);
    finally
      FileStream.Free;
    end;
  finally
    EndWriting;
  end;
end;

procedure TJvID3Controller.SendActivateEvent(Activated: Boolean);
var
  I: Integer;
  ActivateEvent: TJvID3ActivateChangeEvent;
begin
  { Based on TCustomConnection.SendConnectEvent }
  for I := 0 to FClients.Count - 1 do
  begin
    if FActivateEvents[I] <> nil then
    begin
      TMethod(ActivateEvent).Code := FActivateEvents[I];
      TMethod(ActivateEvent).Data := FClients[I];
      ActivateEvent(Self, Activated);
    end;
  end;
end;

procedure TJvID3Controller.SetActive(const Value: Boolean);
begin
  { Based on TCustomConnection.SetConnected }
  if (csReading in ComponentState) and Value then
    FStreamedActive := True
  else
  begin
    if Value = FActive then
      Exit;
    if Value then
    begin
      //if Assigned(BeforeConnect) then BeforeConnect(Self);
      DoOpen;
      SendActivateEvent(FActive);
      //if Assigned(AfterConnect) then AfterConnect(Self);
    end
    else
    begin
      //if Assigned(BeforeDisconnect) then BeforeDisconnect(Self);
      //SendConnectEvent(False);
      DoClose;
      SendActivateEvent(FActive);
      //if Assigned(AfterDisconnect) then AfterDisconnect(Self);
    end;
  end;
end;

procedure TJvID3Controller.SetExtendedHeader(const Value: TJvID3ExtendedHeader);
begin
  FExtendedHeader.Assign(Value);
end;

procedure TJvID3Controller.SetFileName(const Value: TFileName);
var
  SavedActive: Boolean;
begin
  if Value <> FFileName then
  begin
    SavedActive := Active;

    Close;
    FFileName := Value;

    if SavedActive then
      Open;
  end;
end;

procedure TJvID3Controller.SetHeader(const Value: TJvID3Header);
begin
  FHeader.Assign(Value);
end;

procedure TJvID3Controller.SetModified(Value: Boolean);
begin
  FModified := Value;
end;

procedure TJvID3Controller.SetReadEncodingAs(const Value: TJvID3ForceEncoding);
begin
  if (FReadVersionAs in [ifv2_2, ifv2_3]) and (Value in [ifeUTF_16BE, ifeUTF_8]) then
    ID3Error(RsEID3EncodingNotSupported, Self);

  FReadEncodingAs := Value;
end;

procedure TJvID3Controller.SetReadVersionAs(const Value: TJvID3ForceVersion);
begin
  FReadVersionAs := Value;
  if (FReadVersionAs in [ifv2_2, ifv2_3]) and (FReadEncodingAs in [ifeUTF_16BE, ifeUTF_8]) then
    FReadEncodingAs := ifeUTF_16;
end;

procedure TJvID3Controller.SetVersion(NewVersion: TJvID3Version);
begin
  if NewVersion = iveLowerThan2_2 then
    NewVersion := ive2_2
  else
  if NewVersion = iveHigherThan2_4 then
    NewVersion := ive2_4;

  if NewVersion = GetVersion then
    Exit;

  ChangeToVersion(NewVersion);
end;

procedure TJvID3Controller.SetWriteEncodingAs(const Value: TJvID3ForceEncoding);
begin
  if (FWriteVersionAs in [ifv2_2, ifv2_3]) and (Value in [ifeUTF_16BE, ifeUTF_8]) then
    ID3Error(RsEID3EncodingNotSupported, Self);

  FWriteEncodingAs := Value;
end;

procedure TJvID3Controller.SetWriteVersionAs(const Value: TJvID3ForceVersion);
begin
  FWriteVersionAs := Value;
  if (FWriteVersionAs in [ifv2_2, ifv2_3]) and (FWriteEncodingAs in [ifeUTF_16BE, ifeUTF_8]) then
    FWriteEncodingAs := ifeUTF_16;
end;

procedure TJvID3Controller.UnRegisterClient(Client: TObject);
var
  Index: Integer;
begin
  { Based on TCustomConnection.UnRegisterClient }
  Index := FClients.IndexOf(Client);
  if Index <> -1 then
  begin
    FClients.Delete(Index);
    FActivateEvents.Delete(Index);
  end;
end;

procedure TJvID3Controller.WriteTempStream;
var
  LTempStreamSize: Cardinal;
begin
  if not Assigned(FTempStream) then
    Id3Error(RsENoTempStream, Self);

  LTempStreamSize := GetTempStreamSize;
  FTempStream.Seek(0, soFromBeginning);
  FStream.CopyFrom(FTempStream, LTempStreamSize);
end;

//=== TJvID3ControllerDesigner ===============================================

constructor TJvID3ControllerDesigner.Create(Controller: TJvID3Controller);
begin
  inherited Create;
  FController := Controller;
  FController.FDesigner := Self;
end;

destructor TJvID3ControllerDesigner.Destroy;
begin
  FController.FDesigner := nil;
  inherited Destroy;
end;

procedure TJvID3ControllerDesigner.BeginDesign;
begin
  Controller.BeginUpdate;
end;

procedure TJvID3ControllerDesigner.EndDesign;
begin
  Controller.EndUpdate;
end;

procedure TJvID3ControllerDesigner.ID3Event(Event: TJvID3Event; Info: Integer);
begin
end;

//=== TJvID3CustomTextFrame ==================================================

procedure TJvID3CustomTextFrame.Assign(Source: TPersistent);
var
  SP: TJvID3StringPair;
begin
  if Source is TJvID3CustomTextFrame then
  begin
    TJvID3CustomTextFrame(Source).GetText(SP);
    NewText(SP);
  end;
  inherited Assign(Source);
end;

class function TJvID3CustomTextFrame.CanAddFrame(AController: TJvID3Controller;
  AFrameID: TJvID3FrameID): Boolean;
begin
  {  There may only be one text information frame of its kind in an tag }
  Result := not AController.HasFrame(AFrameID) or
    inherited CanAddFrame(AController, AFrameID);
end;

procedure TJvID3CustomTextFrame.Clear;
var
  SP: TJvID3StringPair;
begin
  ClearStringPair(SP);
  NewText(SP);
  inherited Clear;
end;

function TJvID3CustomTextFrame.GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal;
var
  SP: TJvID3StringPair;
begin
  GetText(SP);
  Result := 1 + LengthEnc(SP, Encoding, ToEncoding);
end;

function TJvID3CustomTextFrame.GetIsEmpty: Boolean;
begin
  { Framesize is always >=1, because we must write the Encoding byte }
  Result := GetFrameSize(Encoding) <= 1;
end;

procedure TJvID3CustomTextFrame.ReadFrame;
var
  SP: TJvID3StringPair;
begin
  with Stream do
  begin
    ReadEncoding;
    ReadStringEnc(SP);
    NewText(SP);
  end;
end;

function TJvID3CustomTextFrame.SameUniqueIDAs(const Frame: TJvID3Frame): Boolean;
begin
  {  There may only be one text information frame of its kind in an tag }
  Result := (Assigned(Frame) and (Frame.FrameID = FrameID)) or inherited SameUniqueIDAs(Frame);
end;

function TJvID3CustomTextFrame.SupportsVersion(const AVersion: TJvID3Version): Boolean;
begin
  case FrameID of
    { ** Not supported in 2.2 ** }

    fiFileOwner, fiEncoderSettings:
      Result := AVersion in [ive2_3, ive2_4];

    { ** Deprecated in 2.4 ** }

      { [TDAT] Replaced by the TDRC frame, 'Recording time' }
    fiDate,
      { [TIME] Replaced by the TDRC frame, 'Recording time' }
    fiTime,
      { [TORY] Replaced by the TDOR frame, 'Original release time' }
    fiOrigYear,
      { [TRDA] Replaced by the TDRC frame, 'Recording time' }
    fiRecordingDates,
      { [TSIZ] The information contained in this frame is in the general case
        either trivial to calculate for the player or impossible for the
        tagger to calculate. There is however no good use for such
        information. The frame is therefore completely deprecated. }
    fiSize,
      { [TYER] This frame is replaced by the TDRC frame, 'Recording time' }
    fiYear:
      Result := AVersion in [ive2_2, ive2_3];

    { ** New frames in 2.4 ** }

    fiEncodingTime, { [TDEN] Encoding time }
    fiOrigReleaseTime, { [TDOR] Original release time }
    fiRecordingTime, { [TDRC] Recording time }
    fiReleaseTime, { [TDRL] Release time }
    fiTaggingTime, { [TDTG] Tagging time }
    //fiInvolvedPeople2, { [TIPL] Involved people list }
    //fiMusicianCreditList, { [TMCL] Musician credits list }
    fiMood, { [TMOO] Mood }
    fiProducedNotice, { [TPRO] Produced notice }
    fiAlbumSortOrder, { [TSOA] Album sort order }
    fiPerformerSortOrder, { [TSOP] Performer sort order }
    fiTitleSortOrder, { [TSOT] Title sort order }
    fiSetSubTitle: { [TSST] Set subtitle }
      Result := AVersion = ive2_4;
  else
    Result := True;
  end;
end;

procedure TJvID3CustomTextFrame.WriteFrame;
var
  SP: TJvID3StringPair;
begin
  with Stream do
  begin
    WriteEncoding;
    GetText(SP);
    WriteStringEnc(SP);
  end;
end;

//=== TJvID3DoubleListFrame ==================================================

procedure TJvID3DoubleListFrame.AfterConstruction;
begin
  inherited AfterConstruction;

  FList := TStringList.Create;
  FListW := TWideStringList.Create;

  FList.OnChange := ListChanged;
  FListW.OnChange := ListChanged;
end;

procedure TJvID3DoubleListFrame.Assign(Source: TPersistent);
begin
  if Source is TJvID3DoubleListFrame then
  begin
    FList.Assign(TJvID3DoubleListFrame(Source).List);
    FListW.Assign(TJvID3DoubleListFrame(Source).ListW);
  end;

  inherited Assign(Source);
end;

procedure TJvID3DoubleListFrame.BeforeDestruction;
begin
  inherited BeforeDestruction;

  FList.Free;
  FListW.Free;
end;

class function TJvID3DoubleListFrame.CanAddFrame(AController: TJvID3Controller;
  AFrameID: TJvID3FrameID): Boolean;
begin
  { There may only be one "IPLS" frame in each tag. }
  Result :=
    ((AFrameID in [fiInvolvedPeople, fiInvolvedPeople2, fiMusicianCreditList]) and
    not AController.HasFrame(AFrameID)) or
    inherited CanAddFrame(AController, AFrameID);
end;

procedure TJvID3DoubleListFrame.ChangeToVersion(const ANewVersion: TJvID3Version);
var
  Frame: TJvID3DoubleListFrame;
begin
  if IsEmpty then
    Exit;

  case ANewVersion of
    ive2_2, ive2_3:
      if FrameID in [fiInvolvedPeople2, fiMusicianCreditList] then
      begin
        { Change fiInvolvedPeople2, fiMusicianCreditList to fiInvolvedPeople }
        Frame := TJvID3DoubleListFrame.FindOrCreate(FController, fiInvolvedPeople);
        CopyLists(List, ListW, Encoding, Frame.List, Frame.ListW, Frame.Encoding);
      end;
    ive2_4:
      if FrameID = fiInvolvedPeople then
      begin
        { Change fiInvolvedPeople to fiInvolvedPeople2 }
        Frame := TJvID3DoubleListFrame.FindOrCreate(FController, fiInvolvedPeople2);
        CopyLists(List, ListW, Encoding, Frame.List, Frame.ListW, Frame.Encoding);
      end;
  end;
end;

function TJvID3DoubleListFrame.CheckFrame(const HandleError: TJvID3HandleError): Boolean;
begin
  Result := True;
end;

procedure TJvID3DoubleListFrame.Clear;
begin
  List.Clear;
  ListW.Clear;
  inherited Clear;
end;

class function TJvID3DoubleListFrame.Find(AController: TJvID3Controller;
  const AFrameID: TJvID3FrameID): TJvID3DoubleListFrame;
var
  Frame: TJvID3Frame;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  Frame := AController.Frames.FindFrame(AFrameID);
  if Frame is TJvID3DoubleListFrame then
    Result := TJvID3DoubleListFrame(Frame)
end;

class function TJvID3DoubleListFrame.FindOrCreate(AController: TJvID3Controller;
  const AFrameID: TJvID3FrameID): TJvID3DoubleListFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController, AFrameID);
  if not Assigned(Result) then
  begin
    AController.CheckFrameClass(TJvID3DoubleListFrame, AFrameID);
    Result := TJvID3DoubleListFrame(AController.AddFrame(AFrameID));
  end;
end;

function TJvID3DoubleListFrame.GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal;
var
  I: Integer;
  Item: TJvID3StringPair;
  P: Integer;
  SW: WideString;
begin
  Item.SA := '';
  Item.SW := '';

  { 1 byte for encoding }
  Result := 1;

  case Encoding of
    ienISO_8859_1:
      for I := 0 to List.Count - 1 do
      begin
        Item.SA := Names[I];
        Inc(Result, LengthEnc(Item, Encoding, ToEncoding));
        Inc(Result, LengthTerminatorEnc(ToEncoding));
        Item.SA := Values[I];
        Inc(Result, LengthEnc(Item, Encoding, ToEncoding));
        Inc(Result, LengthTerminatorEnc(ToEncoding));
      end;
    ienUTF_16, ienUTF_16BE, ienUTF_8:
      for I := 0 to ListW.Count - 1 do
      begin
        SW := ListW[I];
        P := Pos('=', SW);
        if P > 0 then
        begin
          SetLength(Item.SW, P - 1);
          Move(SW[1], Item.SW[1], 2 * (P - 1));
        end
        else
          Item.SW := SW;

        Inc(Result, LengthEnc(Item, Encoding, ToEncoding));
        Inc(Result, LengthTerminatorEnc(ToEncoding));

        if (P > 0) and (P < Length(SW)) then
        begin
          SetLength(Item.SW, Length(SW) - P);
          Move(SW[P + 1], Item.SW[1], 2 * (Length(SW) - P));
        end
        else
          Item.SW := '';

        Inc(Result, LengthEnc(Item, Encoding, ToEncoding));
        Inc(Result, LengthTerminatorEnc(ToEncoding));
      end;
  else
    Error(RsEID3UnknownEncoding);
  end;
end;

function TJvID3DoubleListFrame.GetIsEmpty: Boolean;
begin
  Result := False;

  case Encoding of
    ienISO_8859_1:
      Result := (List.Count = 0) or ((List.Count = 1) and (List[0] = ''));
    ienUTF_16, ienUTF_16BE, ienUTF_8:
      Result := (ListW.Count = 0) or ((ListW.Count = 1) and (ListW[0] = ''));
  else
    Error(RsEID3UnknownEncoding);
  end;
end;

function TJvID3DoubleListFrame.GetNameA(Index: Integer): string;
begin
  Result := List.Names[Index];
end;

function TJvID3DoubleListFrame.GetNameW(Index: Integer): string;
begin
  Result := ListW.Names[Index];
end;

function TJvID3DoubleListFrame.GetValueA(Index: Integer): string;
begin
  if Index >= 0 then
    Result := Copy(List[Index], Length(Names[Index]) + 2, MaxInt)
  else
    Result := '';
end;

function TJvID3DoubleListFrame.GetValueW(Index: Integer): string;
begin
  if Index >= 0 then
    Result := Copy(ListW[Index], Length(NamesW[Index]) + 2, MaxInt)
  else
    Result := '';
end;

procedure TJvID3DoubleListFrame.ListChanged(Sender: TObject);
begin
  Changed;
end;

procedure TJvID3DoubleListFrame.ReadFrame;
const
  CMinBytes: array [TJvID3Encoding] of Byte = (2, 4, 4, 2);
var
  S1, S2: TJvID3StringPair;
begin
  with Stream do
  begin
    ReadEncoding;

    while BytesTillEndOfFrame > CMinBytes[Encoding] do
    begin
      ReadStringEnc(S1);
      ReadStringEnc(S2);

      case Encoding of
        ienISO_8859_1:
          List.Add(S1.SA + '=' + S2.SA);
        ienUTF_16, ienUTF_16BE, ienUTF_8:
          ListW.Add(S1.SW + '=' + S2.SW);
      else
        Error(RsEID3UnknownEncoding);
      end;
    end;
  end;
end;

function TJvID3DoubleListFrame.SameUniqueIDAs(const Frame: TJvID3Frame): Boolean;
begin
  Result := (Assigned(Frame) and (Frame.FrameID = FrameID)) or inherited SameUniqueIDAs(Frame);
end;

function TJvID3DoubleListFrame.GetList: TStrings;
begin
  Result := FList;
end;

procedure TJvID3DoubleListFrame.SetList(const Value: TStrings);
begin
  FList.Assign(Value);
  Changed;
end;

function TJvID3DoubleListFrame.GetListW: TWideStrings;
begin
  Result := FListW;
end;

procedure TJvID3DoubleListFrame.SetListW(const Value: TWideStrings);
begin
  FListW.Assign(Value);
  Changed;
end;

function TJvID3DoubleListFrame.SupportsVersion(const AVersion: TJvID3Version): Boolean;
begin
  case FrameID of
    { Deprecated in 2.4 }

    { [IPLS]  - Involved people list
      This frame is replaced by the two frames TMCL, 'Musician credits
      and TIPL, 'Involved people list' }

    fiInvolvedPeople:
      Result := AVersion in [ive2_2, ive2_3];

    { New frames in 2.4 }

    fiInvolvedPeople2, { [TIPL] Involved people list }
    fiMusicianCreditList: { [TMCL] Musician credits list }
      Result := AVersion = ive2_4;
  else
    Result := True;
  end;
end;

procedure TJvID3DoubleListFrame.WriteFrame;
var
  I: Integer;
  Item: TJvID3StringPair;
  SW: WideString;
  P: Integer;
begin
  Item.SA := '';
  Item.SW := '';

  with Stream do
  begin
    WriteEncoding;
    case Encoding of
      ienISO_8859_1:
        for I := 0 to List.Count - 1 do
        begin
          Item.SA := Names[I];
          WriteStringEnc(Item);
          WriteTerminatorEnc;
          Item.SA := Values[I];
          WriteStringEnc(Item);
          WriteTerminatorEnc;
        end;
      ienUTF_16, ienUTF_16BE, ienUTF_8:
        for I := 0 to ListW.Count - 1 do
        begin
          SW := ListW[I];
          P := Pos('=', SW);
          if P > 0 then
          begin
            SetLength(Item.SW, P - 1);
            Move(SW[1], Item.SW[1], 2 * (P - 1));
          end
          else
            Item.SW := SW;

          WriteStringEnc(Item);
          WriteTerminatorEnc;

          if (P > 0) and (P < Length(SW)) then
          begin
            SetLength(Item.SW, Length(SW) - P);
            Move(SW[P + 1], Item.SW[1], 2 * (Length(SW) - P));
          end
          else
            Item.SW := '';

          WriteStringEnc(Item);
          WriteTerminatorEnc;
        end;
    else
      Error(RsEID3UnknownEncoding);
    end;
  end;
end;

//=== TJvID3ExtendedHeader ===================================================

procedure TJvID3ExtendedHeader.Assign(Source: TPersistent);
begin
  if Source is TJvID3ExtendedHeader then
  begin
    FTotalFrameCRC := TJvID3ExtendedHeader(Source).FTotalFrameCRC;
    FSizeOfPadding := TJvID3ExtendedHeader(Source).FSizeOfPadding;
    FFlags := TJvID3ExtendedHeader(Source).FFlags;
  end
  else
    inherited Assign(Source);
end;

procedure TJvID3ExtendedHeader.ChangeToVersion(const ANewVersion: TJvID3Version);
begin
  case ANewVersion of
    ive2_2:
      FFlags := [];
    ive2_3:
      FFlags := FFlags - [hefTagIsAnUpdate, hefTagRestrictions];
    ive2_4:
      { Nothing }
  else
    ID3Error(RsEID3VersionNotSupported, Controller);
  end;
end;

function TJvID3ExtendedHeader.GetSize: Cardinal;
begin
  Result := GetSizeForVersion(Controller.Version);
end;

function TJvID3ExtendedHeader.GetSizeForVersion(const AVersion: TJvID3Version): Cardinal;
begin
  case AVersion of
    ive2_2:
      Result := 0;
    ive2_3:
      begin
        { The 'Extended header size', currently 6 or 10 bytes, excludes itself. }
        Result := 6;
        if hefCRCDataPresent in Flags then
          Inc(Result, 4);
      end;
    ive2_4:
      begin
        Result := 6;
        if hefTagIsAnUpdate in Flags then
          Inc(Result, 1);
        if hefCRCDataPresent in Flags then
          Inc(Result, 6);
        if hefTagRestrictions in Flags then
          Inc(Result, 2);
      end;
  else
    Result := 0;
    ID3Error(RsEID3UnknownVersion, Controller);
  end;
end;

procedure TJvID3ExtendedHeader.Read;
var
  LSize: Cardinal;
  LFlag: Byte;
  FlagDataLength: Byte;
begin
  Reset;

  { Controller.Version is the actual version of the stream; Controller.ReadVersion
    is the version it's transformed in _after_ reading the data from the stream }
  case Controller.Version of
    ive2_2:
      ; { Do nothing }
    ive2_3:
      with Stream do
      begin
        ReadFixedNumber(LSize);

        BeginReadFrame(LSize);
        try
          { Flags:

            %x0000000 00000000            x - CRC data present
          }
          Read(LFlag, 1);
          if LFlag and $80 > 0 then
            Include(FFlags, hefCRCDataPresent);

          { Not used: }
          Read(LFlag, 1);

          { Size of padding }
          ReadFixedNumber(FSizeOfPadding);

          if hefCRCDataPresent in FFlags then
            { Total frame CRC }
            ReadFixedNumber(FTotalFrameCRC);
        finally
          EndReadFrame;
        end;
      end;
    ive2_4:
      with Stream do
      begin
        ReadSyncSafeInteger(LSize);

        { LSize is the size of the whole extended header, thus including the
          just read 4 bytes. An extended header can never have a size of fewer
          than six bytes}

        if LSize < 6 then
          Exit;

        BeginReadFrame(LSize - 4);
        try
          { Nr of flag bytes; always 1 in v2.4 }
          Read(FlagDataLength, 1);

          { Flags:

            %0bcd0000            b - Tag is an update
                                 c - CRC data present
                                 d - Tag restrictions
          }
          Read(LFlag, 1);

          if LFlag and $40 > 0 then
            Include(FFlags, hefTagIsAnUpdate);
          if LFlag and $20 > 0 then
            Include(FFlags, hefCRCDataPresent);
          if LFlag and $10 > 0 then
            Include(FFlags, hefTagRestrictions);

          if hefTagIsAnUpdate in FFlags then
          begin
            Read(FlagDataLength, 1);
            { Expect FlagDataLength to be 0 }
          end;
          if hefCRCDataPresent in FFlags then
          begin
            Read(FlagDataLength, 1);
            { Expect FlagDataLength to be 5 }
            ReadSyncSafeInteger(FTotalFrameCRC, 5);
          end;
          if hefTagRestrictions in FFlags then
          begin
            Read(FlagDataLength, 1);
            { Expect FlagDataLength to be 1 }
            Read(LFlag, 1);
            { Flags:

              %ppqrrstt            p - Tag size restrictions
                                   q - Text encoding restrictions
                                   r - Text fields size restrictions
                                   s - Image encoding restrictions
                                   t - Image size restrictions
            }
            with FRestrictions do
            begin
              RTagSize := TJvID3TagSizeRestriction((LFlag shr 6) and 3);
              RTextEncoding := TJvID3TextEncodingRestriction((LFlag shr 5) and 1);
              RTextFieldsSize := TJvID3TextFieldsSizeRestriction((LFlag shr 3) and 3);
              RImageEncoding := TJvID3ImageEncodingRestriction((LFlag shr 2) and 1);
              RImageSize := TJvID3ImageSizeRestriction(LFlag and 3);
            end;
          end;
        finally
          EndReadFrame;
        end;
      end;
  end;
end;

procedure TJvID3ExtendedHeader.Reset;
begin
  FTotalFrameCRC := 0;
  FSizeOfPadding := 0;
  FFlags := [];
end;

procedure TJvID3ExtendedHeader.SetFlags(const Value: TJvID3HeaderExtendedFlags);
var
  ChangedFlags: TJvID3HeaderExtendedFlags;
begin
  if FFlags <> Value then
  begin
    ChangedFlags := FFlags + Value - (FFlags * Value);

    { hefCRCDataPresent is currently not supported }
    if (hefCRCDataPresent in ChangedFlags) and (hefCRCDataPresent in Value) then
      Id3Error(RsEControllerDoesNotSupportCRC, Controller);

    FFlags := Value;
  end;
end;

procedure TJvID3ExtendedHeader.Write;
var
  LFlag: Byte;
  FlagDataLength: Byte;
  LExtendedHeaderSize: Cardinal;
begin
  LExtendedHeaderSize := GetSizeForVersion(Controller.WriteVersion);

  case Controller.WriteVersion of
    ive2_2:
      ; { Do nothing }
    ive2_3:
      with Stream do
      begin
        WriteFixedNumber(LExtendedHeaderSize);

        BeginWriteFrame(LExtendedHeaderSize);
        try
          { Flags:

            %x0000000 00000000            x - CRC data present
          }
          LFlag := 0;
          if hefCRCDataPresent in Flags then
            Inc(LFlag, $80);
          Write(LFlag, 1);

          { Not used }
          LFlag := 0;
          Write(LFlag, 1);

          { Size of padding }
          WriteFixedNumber(FSizeOfPadding);

          if hefCRCDataPresent in FFlags then
            { Total frame CRC }
            WriteFixedNumber(FTotalFrameCRC);
        finally
          EndWriteFrame;
        end;
      end;
    ive2_4:
      with Stream do
      begin
        WriteSyncSafeInteger(LExtendedHeaderSize);
        { LExtendedHeaderSize is the size of the whole extended header, thus
          including the just read 4 bytes }

        BeginWriteFrame(LExtendedHeaderSize - 4);
        try
          { Nr of flag bytes; always 1 in v2.4 }
          FlagDataLength := 1;
          Write(FlagDataLength, 1);

          { Flags:

            %0bcd0000            b - Tag is an update
                                 c - CRC data present
                                 d - Tag restrictions
          }
          LFlag := 0;
          if hefTagIsAnUpdate in Flags then
            Inc(LFlag, $40);
          if hefCRCDataPresent in Flags then
            Inc(LFlag, $20);
          if hefTagRestrictions in Flags then
            Inc(LFlag, $10);
          Write(LFlag, 1);

          if hefTagIsAnUpdate in FFlags then
          begin
            { FlagDataLength is always 0 for hefTagIsAnUpdate }
            FlagDataLength := 0;
            Write(FlagDataLength, 1);
          end;
          if hefCRCDataPresent in FFlags then
          begin
            { FlagDataLength is always 5 for hefCRCDataPresent }
            FlagDataLength := 5;
            Write(FlagDataLength, 1);

            WriteSyncSafeInteger(FTotalFrameCRC, 5);
          end;
          if hefTagRestrictions in FFlags then
          begin
            { FlagDataLength is always 1 for hefTagIsAnUpdate }
            FlagDataLength := 1;
            Write(FlagDataLength, 1);

            { Flags:

              %ppqrrstt            p - Tag size restrictions
                                   q - Text encoding restrictions
                                   r - Text fields size restrictions
                                   s - Image encoding restrictions
                                   t - Image size restrictions
            }
            with FRestrictions do
              LFlag :=
                ((Byte(RTagSize) and 3) shl 7) +
                ((Byte(RTextEncoding) and 1) shl 5) +
                ((Byte(RTextFieldsSize) and 3) shl 3) +
                ((Byte(RImageEncoding) and 1) shl 2) +
                (Byte(RImageSize) and 3);
            Write(LFlag, 1);
          end;
        finally
          EndWriteFrame;
        end;
      end;
  end;
end;

//=== TJvID3FileInfo =========================================================

procedure TJvID3FileInfo.Calc;
const
  CID3v1Size: array [Boolean] of Integer = (0, 128);
var
  Tmp: Extended;
begin
  if FAudioSize = 0 then
    { No vbr tag found, so we calculate the audio size }
    FAudioSize := FFileSize - FHeaderFoundAt - CID3v1Size[FHasID3v1Tag];

  if (FAudioSize > 0) and (FFrameCount > 0) then
  begin
    { We've found a vbr tag (with enough info) }
    Tmp := FAudioSize / FFrameCount;
    FFrameLengthInBytes := Round(Tmp);

    { Determine average bitrate }
    Tmp := FSamplingRateFrequency * Tmp / CLayerArray[Layer];
    if Version in [mvVersion2, mvVersion25] then
      Tmp := Tmp / 2;

    FBitRate := Round(Tmp);
    FLengthInSec := Round((FAudioSize * 8) / (1000 * Tmp));
  end
  else
  if FBitrate > 0 then
    FLengthInSec := Round((FAudioSize * 8) / (1000 * FBitrate));

  if FFrameLengthInBytes = 0 then
  begin
    { Didn't calc the FFrameLengthInBytes yet }
    Tmp := 0;
    if (FBitrate <> CFreeBitrate) and (FSamplingRateFrequency > 0) then
    begin
      Tmp := CLayerArray[Layer] * FBitRate / FSamplingRateFrequency + FPaddingLength;
      if Version in [mvVersion2, mvVersion25] then
        Tmp := Tmp / 2;
    end;

    if Tmp > 0 then
    begin
      FFrameCount := Round(FAudioSize / Tmp);
      FFrameLengthInBytes := Round(Tmp);
    end;
  end;
end;

function TJvID3FileInfo.GetIsValid: Boolean;
begin
  Result := (FHeaderFoundAt >= 0) and (FLayer <> mlNotDefined) and (FVersion <> mvReserved);
end;

procedure TJvID3FileInfo.ParseMPEGTag(AMPEGTag: PChar);
var
  LHasPadding: Boolean;
  B: Byte;
begin
  { Most info from http://www.dv.co.yu/mpgscript/mpeghdr.htm }

  { AAAAAAAA AAABBCCD EEEEFFGH IIJJKLMM     -> bits

  A   11    (31-21)     Frame sync (all bits set)
  B    2    (20,19)     MPEG Audio version ID
                          00 - MPEG Version 2.5 (unofficial)
                          01 - reserved
                          10 - MPEG Version 2 (ISO/IEC 13818-3)
                          11 - MPEG Version 1 (ISO/IEC 11172-3)
  C    2    (18,17)     Layer description
                          00 - reserved
                          01 - Layer III
                          10 - Layer II
                          11 - Layer I
  D    1    (16)        Protection bit
                          0 - Protected by CRC (16bit crc follows header)
                          1 - Not protected
  E    4    (15,12)     Bitrate index

                        bits  V1,L1  V1,L2  V1,L3  V2,L1  V2, L2 & L3
                        0000  free   free   free   free     free
                        0001   32     32     32     32       8
                        0010   64     48     40     48       16
                        0011   96     56     48     56       24
                        0100   128    64     56     64       32
                        0101   160    80     64     80       40
                        0110   192    96     80     96       48
                        0111   224    112    96     112      56
                        1000   256    128    112    128      64
                        1001   288    160    128    144      80
                        1010   320    192    160    160      96
                        1011   352    224    192    176      112
                        1100   384    256    224    192      128
                        1101   416    320    256    224      144
                        1110   448    384    320    256      160
                        1111   bad    bad    bad    bad      bad

                        NOTES: All values are in kbps
                        V1 - MPEG Version 1
                        V2 - MPEG Version 2 and Version 2.5
                        L1 - Layer I
                        L2 - Layer II
                        L3 - Layer III
                        "free" means free format.
                        "bad" means that this is not an allowed value

  F    2    (11,10)     Sampling rate frequency index (values are in Hz) bits
                                MPEG1    MPEG2    MPEG2.5
                          00    44100    22050    11025
                          01    48000    24000    12000
                          10    32000    16000    8000
                          11    reserv.  reserv.  reserv.
  G    1    (9)         Padding bit
                          0 - frame is not padded
                          1 - frame is padded with one extra slot
  H    1    (8)         Private bit.
  I    2    (7,6)       Channel Mode
                          00 - Stereo
                          01 - Joint stereo (Stereo)
                          10 - Dual channel (2 mono channels)
                          11 - Single channel (Mono)
  J    2    (5,4)       Mode extension (Only if Joint stereo)
                                 Layer I and II               Layer III
                          value                  Intensity stereo  MS stereo
                          00     bands 4 to 31         off           off
                          01     bands 8 to 31         on            off
                          10     bands 12 to 31        off           on
                          11     bands 16 to 31        on            on
  K    1    (3)         Copyright
                          0 - Audio is not copyrighted
                          1 - Audio is copyrighted
  L    1    (2)         Original
                          0 - Copy of original media
                          1 - Original media
  M    2    (1,0)       Emphasis
                          00 - none
                          01 - 50/15 ms
                          10 - reserved
                          11 - CCIT J.17
  }

  { Note: we assume a Reset is done before Parse is called, so we can
          do quick exits }

  { D }
  B := PByte(AMPEGTag + 1)^;
  if B and $1 = 0 then
    Include(FBits, mbProtection);
  { C }
  B := B shr 1;
  FLayer := TJvMPEGLayer(B and $3);
  { B }
  B := B shr 2;
  FVersion := TJvMPEGVersion(B and $3);
  if (FLayer = mlNotDefined) or (FVersion = mvReserved) then
    Exit;

  B := PByte(AMPEGTag + 2)^;
  { H }
  if B and $1 > 0 then
    Include(FBits, mbPrivate);
  B := B shr 1;
  { G }
  LHasPadding := B and $1 > 0;
  B := B shr 1;
  { F }
  FSamplingRateFrequency := CSamplingFrequency[Version, B and $3];
  B := B shr 2;
  { E }
  FBitrate := CBitrate[CMapBitrate[Version in [mvVersion2, mvVersion25], Layer], B and $F];
  if FBitrate = CBadBitrate then
    Exit;

  B := PByte(AMPEGTag + 3)^;
  { M }
  FEmphasis := TJvMPEGEmphasis(B and $3);
  B := B shr 2;
  { L }
  if B and $1 > 0 then
    Include(FBits, mbOriginal);
  B := B shr 1;
  { K }
  if B and $1 > 0 then
    Include(FBits, mbCopyrighted);
  B := B shr 1;
  { J }
  FModeExtension := TJvMPEGModeExtension(B and $3);
  B := B shr 2;
  { I }
  FChannelMode := TJvMPEGChannelMode(B and $3);

  { Calculate some stuff }
  if LHasPadding then
  begin
    if Layer = mlLayerI then
      FPaddingLength := 4
    else
      FPaddingLength := 1;
  end
  else
    FPaddingLength := 0;
end;

procedure TJvID3FileInfo.ParseVbrTag(AMPEGTag: PChar);
const
  VBRTag_Xing: PChar = 'Xing'; { Do not change case }
  VBRTag_Info: PChar = 'Info'; { Do not change case }
  FRAMES_FLAG = $0001;
  BYTES_FLAG = $0002;
  TOC_FLAG = $0004;
var
  HeadFlags: Integer;
begin
  { Now try to find the Xing or Info tag }

  { maximum bytes needed is currently: 4 + 32 + 4 + 4 + 4 + 4 = 52 }
  if Version = mvVersion1 then
  begin
    if ChannelMode <> mcSingleChannel then
      Inc(AMPEGTag, 32 + 4)
    else
      Inc(AMPEGTag, 17 + 4)
  end
  else
  begin
    if ChannelMode <> mcSingleChannel then
      Inc(AMPegTag, 17 + 4)
    else
      Inc(AMPegTag, 9 + 4);
  end;

  if (PInteger(AMPEGTag)^ <> PInteger(VBRTag_Xing)^) and
    (PInteger(AMPEGTag)^ <> PInteger(VBRTag_Info)^) then
    Exit;
  Inc(AMPegTag, 4);

  { (rb) Now always true?? }
  FIsVBR := True;

  HeadFlags := ReverseBytes(PInteger(AMPegTag)^);
  Inc(AMPegTag, 4);

  if HeadFlags and FRAMES_FLAG > 0 then
  begin
    FFrameCount := ReverseBytes(PInteger(AMPegTag)^);
    Inc(AMPegTag, 4);
  end;

  if HeadFlags and BYTES_FLAG > 0 then
    FAudioSize := ReverseBytes(PInteger(AMPegTag)^);
end;

procedure TJvID3FileInfo.Read(AStream: TStream; const Offset: Int64);
const
  CID3v1Tag = 'TAG'; { do not change case }
  CTagSize = 128;
  CTagIDSize = 3;
  CMPEGTagSize = 52;
var
  TagID: array [0..CTagIDSize - 1] of Char;
  MPEGTag: array [0..CMPEGTagSize - 1] of Char;
begin
  Reset;

  FHeaderFoundAt := SearchSync(AStream, Offset, MPEGTag, CMPEGTagSize);
  if FHeaderFoundAt < 0 then
    Exit;

  ParseMPEGTag(MPEGTag);
  ParseVbrTag(MPEGTag);

  if FFileSize = 0 then
    FFileSize := AStream.Size;

  if (FAudioSize = 0) and (FFileSize >= 128) then
  begin
    { Need to determine if the file has an ID3v1 tag }
    AStream.Seek(-CTagSize, soFromEnd);
    FHasID3v1Tag := (AStream.Read(TagID, CTagIDSize) = CTagIDSize) and (TagID = CID3v1Tag);
  end;

  { We now know enough to calculate the rest }
  Calc;
end;

procedure TJvID3FileInfo.Reset;
begin
  FAudioSize := 0;
  FBitrate := 0;
  FBits := [];
  FChannelMode := Low(TJvMPEGChannelMode);
  FEmphasis := Low(TJvMPEGEmphasis);
  FFileSize := 0;
  FFrameCount := 0;
  FFrameLengthInBytes := 0;
  FHasID3v1Tag := False;
  FHeaderFoundAt := -1;
  FIsVBR := False;
  FLayer := Low(TJvMPEGLayer);
  FLengthInSec := 0;
  FModeExtension := Low(TJvMPEGModeExtension);
  FSamplingRateFrequency := 0;
  FVersion := Low(TJvMPEGVersion);
end;

//=== TJvID3Frame ============================================================

constructor TJvID3Frame.Create(AOwner: TComponent; const AFrameID: TJvID3FrameID;
  const AFrameIDStr: string);
begin
  inherited Create(AOwner);

  CheckFrameID(AFrameID);

  FFrameID := AFrameID;
  FrameName := AFrameIDStr;

  FEncoding := ienISO_8859_1;
end;

destructor TJvID3Frame.Destroy;
begin
  if FController <> nil then
  begin
    if FFrames <> nil then
      FFrames.Remove(Self);
  end;
  inherited Destroy;
end;

procedure TJvID3Frame.Assign(Source: TPersistent);
begin
  if Source = nil then
    Clear
  else
  if Source is TJvID3Frame then
  begin
    FFlags := TJvID3Frame(Source).FFlags;
    FEncryptionID := TJvID3Frame(Source).FEncryptionID;
    FGroupID := TJvID3Frame(Source).FGroupID;
    FDecompressedSize := TJvID3Frame(Source).FDecompressedSize;
    FEncoding := TJvID3Frame(Source).FEncoding;
    { v2.4 }
    FDataLengthIndicator := TJvID3Frame(Source).FDataLengthIndicator;

    Changed;
  end
  else
    inherited Assign(Source);
end;

class function TJvID3Frame.CanAddFrame(AController: TJvID3Controller;
  AFrameID: TJvID3FrameID): Boolean;
begin
  Result := False;
end;

procedure TJvID3Frame.Changed;
begin
  FFrameSize := GetFrameSize(Encoding);
  DataChanged;
end;

procedure TJvID3Frame.ChangeToVersion(const ANewVersion: TJvID3Version);
begin
  { Do nothing }
end;

function TJvID3Frame.CheckFrame(const HandleError: TJvID3HandleError): Boolean;
begin
  Result := False;
end;

procedure TJvID3Frame.CheckFrameID(const AFrameID: TJvID3FrameID);
begin
  if AFrameID in [fiErrorFrame, fiPaddingFrame] then
    ErrorFmt(RsEID3FrameIDNotSupported, [ID3_FrameIDToString(AFrameID)]);

  if TJvID3Controller.GetFrameClass(AFrameID) <> ClassType then
    ErrorFmt(RsEID3FrameIDNotSupported, [ID3_FrameIDToString(AFrameID)]);
end;

procedure TJvID3Frame.CheckFrameIDStr(const S: string);
var
  LFrameID: TJvID3FrameID;
begin
  LFrameID := ID3_StringToFrameID(S);
  if LFrameID in [fiErrorFrame, fiPaddingFrame] then
    ErrorFmt(RsEID3FrameIDStrNotSupported, [S]);

  if TJvID3Controller.GetFrameClass(LFrameID) <> ClassType then
    ErrorFmt(RsEID3FrameIDStrNotSupported, [S]);
end;

function TJvID3Frame.CheckIsUnique: Boolean;
begin
  Result := FFrames.CheckIsUnique(Self);
end;

procedure TJvID3Frame.Clear;
begin
  Changed;
end;

procedure TJvID3Frame.DataChanged;
begin
  if Assigned(FController) then
    FController.ID3Event(ideFrameChange, Longint(Self));
end;

procedure TJvID3Frame.Error(const Msg: string);
begin
  Id3ErrorFmt(RsEErrorInFrame, [FrameName, Name, Msg], Controller);
end;

procedure TJvID3Frame.ErrorFmt(const Msg: string;
  const Args: array of const);
begin
  Error(Format(Msg, Args));
end;

function TJvID3Frame.GetFrameIDStrForVersion(
  const Version: TJvID3Version): string;
begin
  if FFrameIDStr = '' then
    case Version of
      ive2_2:
        Result := ID3_FrameIDToString(FrameID, 3);
      ive2_3, ive2_4:
        Result := ID3_FrameIDToString(FrameID, 4);
    else
      Error(RsEID3UnknownVersion);
    end
  else
    Result := FFrameIDStr;
end;

function TJvID3Frame.GetFrameName: string;
begin
  Result := GetFrameIDStrForVersion(ive2_3);
end;

function TJvID3Frame.GetIndex: Integer;
begin
  if FFrames <> nil then
    Result := FFrames.IndexOf(Self)
  else
    Result := -1;
end;

function TJvID3Frame.GetIsEmpty: Boolean;
begin
  Result := True;
end;

function TJvID3Frame.GetStream: TJvID3Stream;
begin
  if not Assigned(FController) then
    Error(RsEID3NoController);

  if icsUsingTempStream in FController.FState then
    Result := FController.FTempStream
  else
    Result := FController.FStream;
end;

procedure TJvID3Frame.Read;
var
  LFrameSize: Integer;
begin
  { Note: don't use 'with Stream do' for the whole procedure, because calling
          BeginUseTempStream changes the value of property Stream
  }

  ReadFrameHeader;

  if not Stream.CanRead(FrameSize) then
  begin
    { Serious error, skip the rest of the stream }
    Stream.BeginReadFrame(Stream.BytesTillEndOfTag);
    Stream.EndReadFrame;
  end
  else
  if (Controller.Version = ive2_4) and (fhfUnsynchronisationApplied in FFlags) then
  begin
    { Stream is unsynchronised, remove the unsynchronisation scheme and
      read the frame }

    Stream.BeginReadFrame(FrameSize);
    try
      Controller.RemoveUnsynchronisationSchemeToTempStream(FrameSize);
    finally
      Stream.EndReadFrame;
    end;

    LFrameSize := Controller.GetTempStreamSize;

    Controller.BeginUseTempStream;
    try
      Stream.BeginReadFrame(LFrameSize);
      try
        //Self.Clear;
        ReadFrame;
      finally
        Stream.EndReadFrame;
      end;
    finally
      Controller.EndUseTempStream;
    end;
  end
  else
    with Stream do
    begin
      BeginReadFrame(FrameSize);
      try
        //Self.Clear;
        ReadFrame;
      finally
        EndReadFrame;
      end;
    end;
end;

procedure TJvID3Frame.ReadEncoding;
begin
  with Stream do
    ReadEnc(FEncoding);
end;

procedure TJvID3Frame.ReadFrameHeader;
var
  Flag0, Flag1: Byte;
begin
  case Controller.Version of
    ive2_2:
      with Stream do
      begin
        { Frame ID         $xx xx xx    (three characters)  // read in TJvID3Frames.Read
          Size             $xx xx xx
        }

        ReadFixedNumber3(FFrameSize);

        FFlags := [];
      end;
    ive2_3:
      with Stream do
      begin
        { Frame ID         $xx xx xx xx (four characters)  // read in TJvID3Frames.Read
          Size             $xx xx xx xx
          Flags            $xx xx
        }

        ReadFixedNumber(FFrameSize);

        { Flags:

          %abc00000 %ijk00000     a - Tag alter preservation    i - Compression
                                  b - File alter preservation   j - Encryption
                                  c - Read only                 k - Grouping identity
        }

        FFlags := [];

        Read(Flag0, 1);
        Read(Flag1, 1);

        if (Flag0 and $80) > 0 then
          Include(FFlags, fhfOnTagAlterDiscardFrame);
        if (Flag0 and $40) > 0 then
          Include(FFlags, fhfOnFileAlterDiscardFrame);
        if (Flag0 and $20) > 0 then
          Include(FFlags, fhfReadOnly);

        if (Flag1 and $80) > 0 then
          Include(FFlags, fhfIsCompressed);
        if (Flag1 and $40) > 0 then
          Include(FFlags, fhfIsEncrypted);
        if (Flag1 and $20) > 0 then
          Include(FFlags, fhfContainsGroupInformation);

        if fhfIsCompressed in Flags then
          ReadFixedNumber(FDecompressedSize);

        if fhfIsEncrypted in Flags then
          Read(FEncryptionID, 1);

        if fhfContainsGroupInformation in Flags then
          Read(FGroupID, 1);
      end;
    ive2_4:
      with Stream do
      begin
        { Frame ID      $xx xx xx xx  (four characters)   // read in TJvID3Frames.Read
          Size      4 * %0xxxxxxx
          Flags         $xx xx
        }
        ReadSyncSafeInteger(FFrameSize, 4);
        FFlags := [];

        { Flags:

          %0abc0000 %0h00kmnp    a - Tag alter preservation   k - Compression
                                 b - File alter preservation  m - Encryption
                                 c - Read only                n - Unsynchronisation
                                 h - Grouping identity        p - Data length indicator
        }

        Read(Flag0, 1);
        Read(Flag1, 1);

        if (Flag0 and $40) > 0 then
          Include(FFlags, fhfOnTagAlterDiscardFrame);
        if (Flag0 and $20) > 0 then
          Include(FFlags, fhfOnFileAlterDiscardFrame);
        if (Flag0 and $10) > 0 then
          Include(FFlags, fhfReadOnly);

        if (Flag1 and $40) > 0 then
          Include(FFlags, fhfContainsGroupInformation);
        if (Flag1 and $08) > 0 then
          Include(FFlags, fhfIsCompressed);
        if (Flag1 and $04) > 0 then
          Include(FFlags, fhfIsEncrypted);
        if (Flag1 and $02) > 0 then
          Include(FFlags, fhfUnsynchronisationApplied);
        if (Flag1 and $01) > 0 then
          Include(FFlags, fhfDataLengthIndicator);

        if fhfContainsGroupInformation in Flags then
          Read(FGroupID, 1);

        if fhfIsEncrypted in Flags then
          Read(FEncryptionID, 1);

        if fhfDataLengthIndicator in Flags then
          { TODO : why , 4? }
          ReadSyncSafeInteger(FDataLengthIndicator, 4);
      end;
  end;
end;

function TJvID3Frame.SameUniqueIDAs(const Frame: TJvID3Frame): Boolean;
begin
  Result := False;
end;

procedure TJvID3Frame.SetController(const AController: TJvID3Controller);
begin
  if AController <> FController then
  begin
    if Assigned(FController) then
      FController.FFrames.Remove(Self);

    FController := AController;

    if Assigned(FController) then
      FController.FFrames.Add(Self);
  end;
end;

procedure TJvID3Frame.SetEncoding(const Value: TJvID3Encoding);
begin
  if FEncoding <> Value then
  begin
    FEncoding := Value;
    Changed;
  end;
end;

procedure TJvID3Frame.SetFlags(const Value: TJvID3FrameHeaderFlags);
var
  ChangedFlags: TJvID3FrameHeaderFlags;
begin
  if FFlags <> Value then
  begin
    ChangedFlags := FFlags + Value - (FFlags * Value);

    { fhfIsCompressed is currently not supported }
    if (fhfIsCompressed in ChangedFlags) and (fhfIsCompressed in Value) then
      Id3Error(RsEControllerDoesNotSupportCompression, Controller);

    { fhfIsEncrypted is currently not supported }
    if (fhfIsEncrypted in ChangedFlags) and (fhfIsEncrypted in Value) then
      Id3Error(RsEControllerDoesNotSupportEncryption, Controller);

    FFlags := Value;
  end;
end;

procedure TJvID3Frame.SetFrameID(const Value: TJvID3FrameID);
begin
  { TODO : Refresh designer while changing }
  CheckFrameID(Value);

  FFrameID := Value;
  FFrameIDStr := '';
end;

procedure TJvID3Frame.SetFrameName(NewFrameName: string);
begin
  { TODO : Refresh designer while changing }
  if NewFrameName = '' then
    FFrameIDStr := ''
  else
  begin
    { Force uppercase }
    NewFrameName := AnsiUpperCase(NewFrameName);

    CheckFrameIDStr(NewFrameName);
    FFrameID := ID3_StringToFrameID(NewFrameName);
    if FFrameID = fiUnknownFrame then
      FFrameIDStr := NewFrameName
    else
      FFrameIDStr := '';
  end;
end;

procedure TJvID3Frame.SetIndex(const Value: Integer);
begin
  if FFrames <> nil then
    FFrames.SetFrameIndex(Self, Value)
end;

function TJvID3Frame.SupportsVersion(const AVersion: TJvID3Version): Boolean;
begin
  Result := AVersion in CSupportedVersions;
end;

procedure TJvID3Frame.UpdateFrameSize;
begin
  FFrameSize := GetFrameSize(Encoding);
end;

procedure TJvID3Frame.Write;
var
  LFrameSize: Cardinal;
begin
  { Note: don't use 'with Stream do' for the whole procedure, because calling
          BeginUseTempStream changes the value of property Stream
  }

  if not SupportsVersion(Controller.WriteVersion) then
    Exit;

  Stream.SourceEncoding := Self.Encoding;

  WriteID;

  { Get the frame size, with the encoding as the stream }
  LFrameSize := GetFrameSize(Stream.DestEncoding);

  if (Controller.WriteVersion = ive2_4) and
    (fhfUnsynchronisationApplied in FFlags) then
  begin
    { Write the frame to the temporary stream }
    Controller.BeginUseTempStream;
    try
      Stream.BeginWriteFrame(LFrameSize);
      try
        WriteFrame;
      finally
        Stream.EndWriteFrame;
      end;

      { Retrieve the frame size _before_ unsynchronisation }
      FDataLengthIndicator := Controller.GetTempStreamSize;

      Controller.ApplyUnsynchronisationSchemeOnCurrentStream;
    finally
      Controller.EndUseTempStream;
    end;

    { Retrieve the frame size _after_ unsynchronisation }
    LFrameSize := Controller.GetTempStreamSize;

    WriteFrameHeader(LFrameSize);

    Controller.WriteTempStream;
  end
  else
    with Stream do
    begin
      WriteFrameHeader(LFrameSize);

      BeginWriteFrame(LFrameSize);
      try
        WriteFrame;
      finally
        EndWriteFrame;
      end;
    end;
end;

procedure TJvID3Frame.WriteEncoding;
begin
  with Stream do
    WriteEnc;
end;

procedure TJvID3Frame.WriteFrameHeader(const AFrameSize: Cardinal);
var
  Flag0, Flag1: Byte;
begin
  { Note: A v2.3 or v2.3 frame size is written as 4 bytes, thus always fits
          exactly in a Cardinal. A v2.2 frame size is written as 3 bytes }
  case Controller.WriteVersion of
    ive2_2:
      if AFrameSize > $00FFFFFF then // = 16 MB
        Id3Error(RsEFrameSizeTooBig, Self)
      else
        with Stream do
        begin
          { Frame ID         $xx xx xx  (three characters)   // Written in TJvID3Frame.Write
            Size             $xx xx xx
          }

          WriteFixedNumber3(AFrameSize);
        end;
    ive2_3:
      with Stream do
      begin
        { Frame ID         $xx xx xx xx (four characters)   // Written in TJvID3Frame.Write
          Size             $xx xx xx xx
          Flags            $xx xx
        }

        WriteFixedNumber(AFrameSize);

        { Flags:

          %abc00000 %ijk00000     a - Tag alter preservation    i - Compression
                                  b - File alter preservation   j - Encryption
                                  c - Read only                 k - Grouping identity
        }

        Flag0 := 0;
        Flag1 := 0;

        if fhfOnTagAlterDiscardFrame in FFlags then
          Inc(Flag0, $80);
        if fhfOnFileAlterDiscardFrame in FFlags then
          Inc(Flag0, $40);
        if fhfReadOnly in FFlags then
          Inc(Flag0, $20);

        { Compression is not supported }
        //if fhfIsCompressed in FFlags then
        //  Inc(Flag1, $80);
        { Encryption is not supported }
        //if fhfIsEncrypted in FFlags then
        //  Inc(Flag1, $40);
        if fhfContainsGroupInformation in FFlags then
          Inc(Flag1, $20);

        Write(Flag0, 1);
        Write(Flag1, 1);

        { Compression is not supported }
        //if fhfIsCompressed in Flags then
        //  WriteFixedNumber(FDecompressedSize);

        { Encryption is not supported }
        //if fhfIsEncrypted in Flags then
        //  Write(FEncryptionID, 1);

        if fhfContainsGroupInformation in Flags then
          Write(FGroupID, 1);
      end;
    ive2_4:
      with Stream do
      begin
        { Frame ID      $xx xx xx xx  (four characters)   // read in TJvID3Frames.Read
          Size      4 * %0xxxxxxx
          Flags         $xx xx
        }
        WriteSyncSafeInteger(AFrameSize, 4);

        { Flags:

          %0abc0000 %0h00kmnp    a - Tag alter preservation   k - Compression
                                 b - File alter preservation  m - Encryption
                                 c - Read only                n - Unsynchronisation
                                 h - Grouping identity        p - Data length indicator
        }

        Flag0 := 0;
        Flag1 := 0;

        if fhfOnTagAlterDiscardFrame in FFlags then
          Inc(Flag0, $40);
        if fhfOnFileAlterDiscardFrame in FFlags then
          Inc(Flag0, $20);
        if fhfReadOnly in FFlags then
          Inc(Flag0, $10);

        if fhfContainsGroupInformation in FFlags then
          Inc(Flag1, $40);
        { Compression is not supported }
        //if fhfIsCompressed in FFlags then
        //  Inc(Flag1, $08);
        { Encryption is not supported }
        //if fhfIsEncrypted in FFlags then
        //  Inc(Flag1, $04);
        if fhfUnsynchronisationApplied in FFlags then
          Inc(Flag1, $02);
        if fhfDataLengthIndicator in FFlags then
          Inc(Flag1, $01);

        Write(Flag0, 1);
        Write(Flag1, 1);

        if fhfContainsGroupInformation in Flags then
          Write(FGroupID, 1);

        { Encryption is not supported }
        //if fhfIsEncrypted in Flags then
        //  Write(FEncryptionID, 1);

        if fhfDataLengthIndicator in Flags then
          WriteSyncSafeInteger(FDataLengthIndicator, 4);
      end;
  end;
end;

procedure TJvID3Frame.WriteID;
var
  LFrameIDStr: string;
  FrameIDLength: Byte;
begin
  LFrameIDStr := GetFrameIDStrForVersion(Controller.WriteVersion);
  FrameIDLength := GetFrameIDLength(Controller.WriteVersion);

  if Length(LFrameIDStr) <> FrameIDLength then
  begin
    SetLength(LFrameIDStr, FrameIDLength);
    FillChar(LFrameIDStr, FrameIDLength, #0);
  end;

  with Stream do
    Write(PChar(LFrameIDStr)^, FrameIDLength);
end;

//=== TJvID3Frames ===========================================================

procedure TJvID3Frames.Add(Frame: TJvID3Frame);
begin
  CheckCanAddFrame(Frame.FrameID);

  FList.Add(Frame);
  Frame.FFrames := Self;
  Frame.Controller := Controller;
  Changed;
end;

procedure TJvID3Frames.AfterConstruction;
begin
  FList := TList.Create;
  inherited AfterConstruction;
end;

procedure TJvID3Frames.Assign(Source: TPersistent);
var
  I: Integer;
  Frame: TJvID3Frame;
begin
  if Source is TJvID3Frames then
  begin
    Clear;
    for I := 0 to TJvID3Frames(Source).FList.Count - 1 do
    begin
      Frame := Controller.AddFrame(TJvID3Frames(Source).Frames[I].FrameID);
      Frame.FrameName := TJvID3Frames(Source).Frames[I].FrameName;
      Frame.Assign(TJvID3Frames(Source).Frames[I]);
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvID3Frames.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if FList <> nil then
    Clear;
  FList.Free;
end;

procedure TJvID3Frames.Changed;
begin
  if (FController <> nil) and not (csDestroying in FController.ComponentState) then
    FController.ID3Event(ideFrameListChange, 0);
  {if Assigned(OnChange) then OnChange(Self);}
end;

procedure TJvID3Frames.ChangeToVersion(const ANewVersion: TJvID3Version);
var
  I: Integer;
begin
  if not (ANewVersion in CSupportedVersions) then
    ID3Error(RsEID3VersionNotSupported, Controller);

  for I := Count - 1 downto 0 do
    Frames[I].ChangeToVersion(ANewVersion);

  for I := Count - 1 downto 0 do
    if not Frames[I].SupportsVersion(ANewVersion) then
      Frames[I].Free;
end;

procedure TJvID3Frames.CheckCanAddFrame(FrameID: TJvID3FrameID);
begin
  if not FController.CanAddFrame(FrameID) then
    ID3ErrorFmt(RsEID3AlreadyContainsFrame, [ID3_FrameIDToString(FrameID)]);
end;

function TJvID3Frames.CheckFrames(const HandleError: TJvID3HandleError): Boolean;
var
  I: Integer;
begin
  Result := False;
  { Check whether the frames have correct parameters }
  for I := 0 to Count - 1 do
    if not Frames[I].CheckFrame(HandleError) then
      Exit;

  { Check whether the frames are unique }
  for I := Count - 1 downto 0 do
    if not Frames[I].CheckIsUnique then
      case HandleError of
        heAutoCorrect:
          Frames[I].Free;
        heRaise:
          Frames[I].Error(RsEID3DuplicateFrame);
      else
        Exit;
      end;

  Result := True;
end;

function TJvID3Frames.CheckIsUnique(Frame: TJvID3Frame): Boolean;
var
  FoundFrame: TJvID3Frame;
begin
  Result := True;
  if not Assigned(Frame) then
    Exit;

  if not Controller.FindFirstFrame(Frame.FrameID, FoundFrame) then
    Exit;

  while Assigned(FoundFrame) and (FoundFrame.Index < Frame.Index) do
  begin
    if FoundFrame.SameUniqueIDAs(Frame) then
    begin
      Result := False;
      Break;
    end;

    if not Controller.FindNextFrame(Frame.FrameID, FoundFrame) then
      Break;
  end;
end;

procedure TJvID3Frames.Clear;
var
  F: TJvID3Frame;
begin
  if FList.Count <= 0 then
    Exit;

  while FList.Count > 0 do
  begin
    F := FList.Last;
    F.FController := nil;
    F.Free;
    FList.Delete(FList.Count - 1);
  end;
  Changed;
end;

function TJvID3Frames.FindFrame(const FrameID: TJvID3FrameID): TJvID3Frame;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Result := FList.Items[I];
    if Result.FrameID = FrameID then
      Exit
  end;
  Result := nil;
end;

function TJvID3Frames.FindFrame(const FrameName: string): TJvID3Frame;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Result := FList.Items[I];
    if SameText(Result.FrameName, FrameName) then
      Exit;
  end;
  Result := nil;
end;

function TJvID3Frames.FrameByID(const FrameID: TJvID3FrameID): TJvID3Frame;
begin
  Result := FindFrame(FrameID);
  if Result = nil then
    ID3ErrorFmt(RsEID3FrameNotFound, [ID3_FrameIDToString(FrameID)], Controller);
end;

function TJvID3Frames.FrameByName(const FrameName: string): TJvID3Frame;
begin
  Result := FindFrame(FrameName);
  if Result = nil then
    ID3ErrorFmt(RsEID3FrameNotFound, [FrameName], Controller);
end;

function TJvID3Frames.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJvID3Frames.GetFrame(Index: Integer): TJvID3Frame;
begin
  Result := FList[Index];
end;

function TJvID3Frames.GetFrameIDs: TJvID3FrameIDs;
begin
end;

procedure TJvID3Frames.GetFrameNames(List: TStrings);
var
  I: Integer;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := 0 to FList.Count - 1 do
      List.Add(TJvID3Frame(FList.Items[I]).FrameName)
  finally
    List.EndUpdate;
  end;
end;

function TJvID3Frames.IndexOf(Frame: TJvID3Frame): Integer;
begin
  Result := FList.IndexOf(Frame);
end;

procedure TJvID3Frames.Read;
const
  { v2.2        : Frame header is 6 bytes
    v2.3 and up : Frame header is minimal 10 bytes }
  CMinimalHeaderSize: array [Boolean] of Byte = (6, 10);
var
  Frame: TJvID3Frame;
  FrameIDStr: string;
  FrameID: TJvID3FrameID;

  LFrameIDLength: Byte;
  LMinimalHeaderSize: Byte;
begin
  LFrameIDLength := GetFrameIDLength(Controller.Version);
  LMinimalHeaderSize := CMinimalHeaderSize[Controller.Version > ive2_2];
  SetLength(FrameIDStr, LFrameIDLength);

  with Stream do
    while BytesTillEndOfTag >= LMinimalHeaderSize do
    begin
      if Read(PChar(FrameIDStr)^, LFrameIDLength) <> LFrameIDLength then
        Exit;

      FrameID := ID3_StringToFrameID(FrameIDStr);

      if FrameID in [fiPaddingFrame, fiErrorFrame] then
        Exit;

      Frame := Controller.AddFrame(FrameID);
      if Assigned(Frame) then
      begin
        Frame.FrameName := FrameIDStr;
        Frame.Read;
      end;
    end;
end;

procedure TJvID3Frames.Remove(Frame: TJvID3Frame);
begin
  FList.Remove(Frame);
  Frame.FFrames := nil;
  Changed;
end;

procedure TJvID3Frames.RemoveEmptyFrames;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Frames[I].IsEmpty then
      Frames[I].Free;
end;

procedure TJvID3Frames.Reset;
begin
  Clear;
end;

procedure TJvID3Frames.SetFrame(Index: Integer; Value: TJvID3Frame);
begin
  Frames[Index].Assign(Value);
end;

procedure TJvID3Frames.SetFrameIndex(Frame: TJvID3Frame; Value: Integer);
var
  CurIndex, Count: Integer;
begin
  CurIndex := FList.IndexOf(Frame);
  if CurIndex >= 0 then
  begin
    Count := FList.Count;
    if Value < 0 then
      Value := 0;
    if Value >= Count then
      Value := Count - 1;
    if Value <> CurIndex then
    begin
      FList.Delete(CurIndex);
      FList.Insert(Value, Frame);
      Changed;
    end;
  end;
end;

procedure TJvID3Frames.Write;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    Frames[I].Write;
end;

//=== TJvID3GeneralObjFrame ==================================================

procedure TJvID3GeneralObjFrame.Assign(Source: TPersistent);
begin
  if Source is TJvID3GeneralObjFrame then
  begin
    CopyStringPair(TJvID3GeneralObjFrame(Source).FContentDescription, FContentDescription);
    FMIMEType := TJvID3GeneralObjFrame(Source).FMIMEType;
    CopyStringPair(TJvID3GeneralObjFrame(Source).FFileName, FFileName);
  end;
  inherited Assign(Source);
end;

class function TJvID3GeneralObjFrame.CanAddFrame(AController: TJvID3Controller;
  AFrameID: TJvID3FrameID): Boolean;
begin
  { There may be more than one "GEOB" frame in each tag, but only one with the
    same content descriptor. }
  Result := (AFrameID = fiGeneralObject) or
    inherited CanAddFrame(AController, AFrameID);
end;

function TJvID3GeneralObjFrame.CheckFrame(const HandleError: TJvID3HandleError): Boolean;
begin
  Result := True;
end;

procedure TJvID3GeneralObjFrame.Clear;
begin
  ClearStringPair(FContentDescription);
  FMIMEType := '';
  ClearStringPair(FFileName);

  inherited Clear;
end;

class function TJvID3GeneralObjFrame.Find(AController: TJvID3Controller;
  const AContentDescription: string): TJvID3GeneralObjFrame;
var
  Frame: TJvID3Frame;
  SP: TJvID3StringPair;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  if not AController.FindFirstFrame(fiGeneralObject, Frame) then
    Exit;

  SP.SA := AContentDescription;

  while (Frame is TJvID3GeneralObjFrame) and
    not SameStringPair(SP, TJvID3GeneralObjFrame(Frame).FContentDescription,
      ienISO_8859_1, Frame.Encoding) do

    AController.FindNextFrame(fiGeneralObject, Frame);

  if Frame is TJvID3GeneralObjFrame then
    Result := TJvID3GeneralObjFrame(Frame);
end;

class function TJvID3GeneralObjFrame.Find(AController: TJvID3Controller): TJvID3GeneralObjFrame;
var
  Frame: TJvID3Frame;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  Frame := AController.Frames.FindFrame(fiGeneralObject);
  if Frame is TJvID3GeneralObjFrame then
    Result := TJvID3GeneralObjFrame(Frame);
end;

class function TJvID3GeneralObjFrame.FindOrCreate(AController: TJvID3Controller;
  const AContentDescription: string): TJvID3GeneralObjFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController, AContentDescription);
  if not Assigned(Result) then
  begin
    Result := TJvID3GeneralObjFrame(AController.AddFrame(fiGeneralObject));
    Result.ContentDescription := AContentDescription;
  end;
end;

class function TJvID3GeneralObjFrame.FindOrCreate(AController: TJvID3Controller): TJvID3GeneralObjFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController);
  if not Assigned(Result) then
    Result := TJvID3GeneralObjFrame(AController.AddFrame(fiGeneralObject));
end;

class function TJvID3GeneralObjFrame.FindOrCreateW(AController: TJvID3Controller;
  const AContentDescription: WideString): TJvID3GeneralObjFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController, AContentDescription);
  if not Assigned(Result) then
  begin
    Result := TJvID3GeneralObjFrame(AController.AddFrame(fiGeneralObject));
    Result.ContentDescriptionW := AContentDescription;
  end;
end;

class function TJvID3GeneralObjFrame.FindW(AController: TJvID3Controller;
  const AContentDescription: WideString): TJvID3GeneralObjFrame;
var
  Frame: TJvID3Frame;
  SP: TJvID3StringPair;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  if not AController.FindFirstFrame(fiGeneralObject, Frame) then
    Exit;

  SP.SW := AContentDescription;

  while (Frame is TJvID3GeneralObjFrame) and
    not SameStringPair(SP, TJvID3GeneralObjFrame(Frame).FContentDescription,
      ienUTF_16, Frame.Encoding) do

    AController.FindNextFrame(fiGeneralObject, Frame);

  if Frame is TJvID3GeneralObjFrame then
    Result := TJvID3GeneralObjFrame(Frame);
end;

function TJvID3GeneralObjFrame.GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal;
begin
  { Text encoding              $xx
    MIME type                  <text string> $00
    FileName                   <text string according to encoding> $00 (00)
    Content description        <text string according to encoding> $00 (00)
    Encapsulated object        <binary data>
  }
  Result := 1 + Cardinal(Length(MIMEType)) + 1 +
    LengthEnc(FFileName, Encoding, ToEncoding) +
    LengthTerminatorEnc(ToEncoding) +
    LengthEnc(FContentDescription, Encoding, ToEncoding) +
    LengthTerminatorEnc(ToEncoding) +
    DataSize;
end;

function TJvID3GeneralObjFrame.GetIsEmpty: Boolean;
begin
  Result := inherited GetIsEmpty and (Length(FMIMEType) = 0) and
    CheckIsEmpty(FContentDescription, Encoding) and
    CheckIsEmpty(FFileName, Encoding);
end;

procedure TJvID3GeneralObjFrame.ReadFrame;
begin
  { Text encoding              $xx
    MIME type                  <text string> $00
    FileName                   <text string according to encoding> $00 (00)
    Content description        <text string according to encoding> $00 (00)
    Encapsulated object        <binary data>
  }
  with Stream do
  begin
    ReadEncoding;
    ReadStringA(FMimeType);
    ReadStringEnc(FFileName);
    ReadStringEnc(FContentDescription);
    ReadData(BytesTillEndOfFrame);
  end;
end;

function TJvID3GeneralObjFrame.SameUniqueIDAs(const Frame: TJvID3Frame): Boolean;
begin
  { There may be more than one "GEOB" frame in each tag, but only one with the
    same content descriptor. }
  Result := (Frame is TJvID3GeneralObjFrame) and
    (Frame.FrameID = FrameID) and (FrameID = fiGeneralObject);

  if Result then
    Result :=
      SameStringPair(TJvID3GeneralObjFrame(Frame).FContentDescription, FContentDescription,
        TJvID3GeneralObjFrame(Frame).Encoding, Encoding)
  else
    Result := inherited SameUniqueIDAs(Frame);
end;

procedure TJvID3GeneralObjFrame.SetContentDescription(const Value: string);
begin
  if FContentDescription.SA <> Value then
  begin
    FContentDescription.SA := Value;
    Changed;
  end;
end;

procedure TJvID3GeneralObjFrame.SetContentDescriptionW(const Value: WideString);
begin
  if FContentDescription.SW <> Value then
  begin
    FContentDescription.SW := Value;
    Changed;
  end;
end;

procedure TJvID3GeneralObjFrame.SetFileName(const Value: string);
begin
  if FFileName.SA <> Value then
  begin
    FFileName.SA := Value;
    Changed;
  end;
end;

procedure TJvID3GeneralObjFrame.SetFileNameW(const Value: WideString);
begin
  if FFileName.SW <> Value then
  begin
    FFileName.SW := Value;
    Changed;
  end;
end;

procedure TJvID3GeneralObjFrame.SetMIMEType(const Value: string);
begin
  if FMIMEType <> Value then
  begin
    FMIMEType := Value;
    Changed;
  end;
end;

procedure TJvID3GeneralObjFrame.WriteFrame;
begin
  { Text encoding              $xx
    MIME type                  <text string> $00
    FileName                   <text string according to encoding> $00 (00)
    Content description        <text string according to encoding> $00 (00)
    Encapsulated object        <binary data>
  }
  with Stream do
  begin
    WriteEncoding;
    WriteStringA(MimeType);
    WriteTerminatorA;
    WriteStringEnc(FFileName);
    WriteTerminatorEnc;
    WriteStringEnc(FContentDescription);
    WriteTerminatorEnc;
    WriteData;
  end;
end;

//=== TJvID3Header ===========================================================

procedure TJvID3Header.Assign(Source: TPersistent);
begin
  if Source is TJvID3Header then
  begin
    FHasTag := TJvID3Header(Source).HasTag;
    FRevisionNumber := TJvID3Header(Source).RevisionNumber;
    FMajorVersion := TJvID3Header(Source).MajorVersion;
    FSize := TJvID3Header(Source).Size;
    FFlags := TJvID3Header(Source).Flags;
  end
  else
    inherited Assign(Source);
end;

procedure TJvID3Header.ChangeToVersion(const ANewVersion: TJvID3Version);
begin
  case ANewVersion of
    ive2_2:
      begin
        FRevisionNumber := 0;
        FMajorVersion := 2;
        { Only flag 'hfUnsynchronisation' is allowed }
        FFlags := FFlags * [hfUnsynchronisation];
      end;
    ive2_3:
      begin
        FRevisionNumber := 0;
        FMajorVersion := 3;
        Exclude(FFlags, hfFooterPresent);
      end;
    ive2_4:
      begin
        FRevisionNumber := 0;
        FMajorVersion := 4;
      end;
  else
    ID3Error(RsEID3VersionNotSupported);
  end;
end;

procedure TJvID3Header.Read;
var
  Header: TID3v2HeaderRec;
begin
  Reset;

  with Stream do
  begin
    BeginReadFrame(10);
    try
      if Read(Header, 10) <> 10 then
        Exit;

      FHasTag := Header.Identifier = cID3HeaderId;
      if not FHasTag then
        Exit;

      { This sets Controller.Version }
      FMajorVersion := Header.MajorVersion;
      FRevisionNumber := Header.RevisionNumber;

      { v2.2 : %ae000000    a - Unsynchronisation       d - Footer present
        v2.3 : %abc00000    b - Extended header         e - Compression (only v2.2)
        v2.4 : %abcd0000    c - Experimental indicator
      }
      if Header.Flags and $80 > 0 then
        Include(FFlags, hfUnsynchronisation);
      if Header.Flags and $40 > 0 then
      begin
        { v2.2:  Since no compression scheme has been decided yet, the ID3
                 decoder (for now) should just ignore the entire tag if the
                 compression bit is set. }
        if Controller.Version <> ive2_2 then
          Include(FFlags, hfExtendedHeader);
      end;
      if Header.Flags and $20 > 0 then
        Include(FFlags, hfExperimentalIndicator);
      if Header.Flags and $10 > 0 then
        Include(FFlags, hfFooterPresent);

      { The ID3v2 tag size is the size of the complete tag after unsychronisation,
        including padding, excluding the header but not excluding the extended
        header }
      UnSyncSafe(Header.Size, 4, FSize);
    finally
      EndReadFrame;
    end;
  end;
end;

procedure TJvID3Header.Reset;
begin
  FHasTag := False;
  FRevisionNumber := 0;
  FMajorVersion := 0;
  FSize := 0;
  FFlags := [];
end;

procedure TJvID3Header.SetFlags(const Value: TJvID3HeaderFlags);
var
  ChangedFlags: TJvID3HeaderFlags;
begin
  if FFlags <> Value then
  begin
    ChangedFlags := FFlags + Value - (FFlags * Value);

    { hfFooterPresent is currently not supported }
    if (hfFooterPresent in ChangedFlags) and (hfFooterPresent in Value) then
      Id3Error(RsEControllerDoesNotSupportFooter, Controller);

    FFlags := Value;
  end;
end;

procedure TJvID3Header.Write;
const
  { iveLowerThan2_2, ive2_2, ive2_3, ive2_4, iveHigherThan2_4 }
  CMajorVersion: array [TJvID3Version] of Byte = (2, 2, 3, 4, 4);
  CRevisionNumber: array [TJvID3Version] of Byte = (0, 0, 0, 0, 0);
var
  Header: TID3v2HeaderRec;
begin
  { Check max size }
  if Header.Size > $0FFFFFFF then // 28 bits = 256 MB
    Id3Error(RsETagTooBig, Controller);

  with Stream do
  begin
    BeginWriteFrame(10);
    try
      Header.Identifier := cID3HeaderId;
      Header.MajorVersion := CMajorVersion[Controller.WriteVersion];
      Header.RevisionNumber := CRevisionNumber[Controller.WriteVersion];

      { v2.2 : %ae000000    a - Unsynchronisation       d - Footer present
        v2.3 : %abc00000    b - Extended header         e - Compression (only v2.2)
        v2.4 : %abcd0000    c - Experimental indicator
      }
      if hfUnsynchronisation in Flags then
        Inc(Header.Flags, $80);
      if Controller.WriteVersion > ive2_2 then
      begin
        if hfExtendedHeader in Flags then
          Inc(Header.Flags, $40);
        if hfExperimentalIndicator in Flags then
          Inc(Header.Flags, $20);
        { Only for v2.4 }
        if (Controller.WriteVersion = ive2_4) and (hfFooterPresent in Flags) then
          Inc(Header.Flags, $20);
      end;
      { The ID3v2 tag size is the size of the complete tag after unsychronisation,
        including padding, excluding the header but not excluding the extended
        header }
      SyncSafe(FSize, Header.Size, 4);

      WriteBuffer(Header, 10);
    finally
      EndWriteFrame;
    end;
  end;
end;

//=== TJvID3NumberFrame ======================================================

procedure TJvID3NumberFrame.ChangeToVersion(const ANewVersion: TJvID3Version);
var
  Year: Word;
  LDate: TDateTime;
  Frame: TJvID3Frame;
begin
  if ANewVersion <> ive2_4 then
    Exit;

  { Change

    * fiYear, fiDate, fiTime, fiRecordingDates frames into 1 fiRecordingTime frame
    * fiOrigYear frame into 1 fiOrigReleaseTime frame }

  if FrameID = fiYear then
  begin
    if Assigned(FFrames.FindFrame(fiRecordingTime)) then
      Exit;

    { 1. Determine the year from a fiYear frame, ie this frame }
    Year := Value;

    { 2. Determine month + day from a fiDate frame }
    Frame := TJvID3TextFrame.Find(FController, fiDate);
    if Assigned(Frame) then
      with TJvID3TextFrame(Frame) do
        LDate := GetID3Date(FText, Encoding, Year)
    else
      try
        { hm, no date frame , just assume it's 1 jan }
        LDate := EncodeDate(Year, 1, 1);
      except
        on EConvertError do
          LDate := 0;
      end;

    { 3. Determine hour + min from a fiTime frame}
    Frame := TJvID3TextFrame.Find(FController, fiTime);
    if Assigned(Frame) then
      with TJvID3TextFrame(Frame) do
        LDate := LDate + GetID3Time(FText, Encoding);

    { 4. Copy constructed date to a fiRecordingTime frame }
    TJvID3TimestampFrame.FindOrCreate(FController, fiRecordingTime).Value := LDate;
  end
  else
  if FrameID = fiOrigYear then
  begin
    if Assigned(FFrames.FindFrame(fiOrigReleaseTime)) then
      Exit;

    try
      LDate := EncodeDate(Value, 1, 1);
    except
      on EConvertError do
        LDate := 0;
    end;

    { Copy date to a fiRecordingTime frame }
    TJvID3TimestampFrame.FindOrCreate(FController, fiOrigReleaseTime).Value := LDate;
  end;
end;

function TJvID3NumberFrame.CheckFrame(const HandleError: TJvID3HandleError): Boolean;
begin
  if FrameID in [fiOrigYear, fiYear] then
  begin
    { Always 4 characters long }
    Result := FValue < 10000;

    if not Result then
      case HandleError of
        heAutoCorrect:
          begin
            { No need to call UpdateFrameSize, because it's always 4 chars long }
            Result := True;
            FValue := 0;
          end;
        heRaise:
          ErrorFmt(RsEID3ValueTooBig, [FValue]);
      end;
  end
  else
    Result := True;
end;

class function TJvID3NumberFrame.Find(AController: TJvID3Controller;
  const AFrameID: TJvID3FrameID): TJvID3NumberFrame;
var
  Frame: TJvID3Frame;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  Frame := AController.Frames.FindFrame(AFrameID);
  if Frame is TJvID3NumberFrame then
    Result := TJvID3NumberFrame(Frame)
end;

class function TJvID3NumberFrame.FindOrCreate(AController: TJvID3Controller;
  const AFrameID: TJvID3FrameID): TJvID3NumberFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController, AFrameID);
  if not Assigned(Result) then
  begin
    AController.CheckFrameClass(TJvID3NumberFrame, AFrameID);
    Result := TJvID3NumberFrame(AController.AddFrame(AFrameID));
  end;
end;

function TJvID3NumberFrame.GetIsEmpty: Boolean;
begin
  if FrameID in [fiOrigYear, fiYear] then
    Result := Value = 0
  else
    Result := inherited GetIsEmpty;
end;

procedure TJvID3NumberFrame.GetText(var AText: TJvID3StringPair);
const
  CFormat: array [Boolean] of string = ('%d', '%.4d');
begin
  AText.SA := Format(CFormat[FrameID in [fiOrigYear, fiYear]], [FValue]);

  if Encoding in [ienUTF_16, ienUTF_16BE, ienUTF_8] then
    AText.SW := StringToWideStringEx(AText.SA, CP_ACP);
end;

procedure TJvID3NumberFrame.NewText(const ANewText: TJvID3StringPair);
begin
  case Encoding of
    ienISO_8859_1:
      FValue := StrToIntDef(ANewText.SA, 0);
    ienUTF_16, ienUTF_16BE, ienUTF_8:
      FValue := StrToIntDef(WideStringToStringEx(ANewText.SW, CP_ACP), 0);
  else
    Error(RsEID3UnknownEncoding);
  end;

  UpdateFrameSize;
end;

procedure TJvID3NumberFrame.SetValue(const Value: Cardinal);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    Changed;
  end;
end;

//=== TJvID3OwnershipFrame ===================================================

procedure TJvID3OwnershipFrame.Assign(Source: TPersistent);
begin
  if Source is TJvID3OwnershipFrame then
  begin
    FPricePayed := TJvID3OwnershipFrame(Source).PricePayed;
    CopyStringPair(TJvID3OwnershipFrame(Source).FSeller, FSeller);
    FDateOfPurch := TJvID3OwnershipFrame(Source).DateOfPurch;
  end;

  inherited Assign(Source);
end;

class function TJvID3OwnershipFrame.CanAddFrame(AController: TJvID3Controller;
  AFrameID: TJvID3FrameID): Boolean;
begin
  { There may only be one 'OWNE' frame in a tag }
  Result := ((AFrameID = fiOwnership) and not AController.HasFrame(fiOwnership)) or
    inherited CanAddFrame(AController, AFrameID);
end;

function TJvID3OwnershipFrame.CheckFrame(const HandleError: TJvID3HandleError): Boolean;
begin
  Result := True;
end;

procedure TJvID3OwnershipFrame.Clear;
begin
  FPricePayed := '';
  ClearStringPair(FSeller);
  FDateOfPurch := 0;
  inherited Clear;
end;

class function TJvID3OwnershipFrame.Find(AController: TJvID3Controller): TJvID3OwnershipFrame;
var
  Frame: TJvID3Frame;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  Frame := AController.Frames.FindFrame(fiOwnership);
  if Frame is TJvID3OwnershipFrame then
    Result := TJvID3OwnershipFrame(Frame)
end;

class function TJvID3OwnershipFrame.FindOrCreate(AController: TJvID3Controller): TJvID3OwnershipFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController);
  if not Assigned(Result) then
    Result := TJvID3OwnershipFrame(AController.AddFrame(fiOwnership));
end;

function TJvID3OwnershipFrame.GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal;
begin
  { Text encoding           $xx
    Price payed             <text string> $00
    Date of purch.          <text string>
    Seller                  <text string according to encoding>
  }
  Result := 1 + Cardinal(Length(FPricePayed)) + 1 + 8 +
    LengthEnc(FSeller, Encoding, ToEncoding);
end;

function TJvID3OwnershipFrame.GetIsEmpty: Boolean;
begin
  Result := (Length(FPricePayed) = 0) and CheckIsEmpty(FSeller, Encoding) and
    (FDateOfPurch = 0);
end;

procedure TJvID3OwnershipFrame.ReadFrame;
begin
  { Text encoding           $xx
    Price payed             <text string> $00
    Date of purch.          <text string>
    Seller                  <text string according to encoding>
  }
  with Stream do
  begin
    ReadEncoding;
    ReadStringA(FPricePayed);
    ReadDate(FDateOfPurch);
    ReadStringEnc(FSeller);
  end;
end;

function TJvID3OwnershipFrame.SameUniqueIDAs(const Frame: TJvID3Frame): Boolean;
begin
  { There may only be one 'OWNE' frame in a tag }
  Result := (Assigned(Frame) and (Frame.FrameID = FrameID) and (FrameID = fiOwnership)) or
    inherited SameUniqueIDAs(Frame);
end;

procedure TJvID3OwnershipFrame.SetDateOfPurch(const Value: TDateTime);
begin
  if FDateOfPurch <> Value then
  begin
    FDateOfPurch := Value;
    Changed;
  end;
end;

procedure TJvID3OwnershipFrame.SetPricePayed(const Value: string);
begin
  if FPricePayed <> Value then
  begin
    FPricePayed := Value;
    Changed;
  end;
end;

procedure TJvID3OwnershipFrame.SetSeller(const Value: string);
begin
  if FSeller.SA <> Value then
  begin
    FSeller.SA := Value;
    Changed;
  end;
end;

procedure TJvID3OwnershipFrame.SetSellerW(const Value: WideString);
begin
  if FSeller.SW <> Value then
  begin
    FSeller.SW := Value;
    Changed;
  end;
end;

function TJvID3OwnershipFrame.SupportsVersion(const AVersion: TJvID3Version): Boolean;
begin
  case FrameID of
    { ** Not supported in 2.2 ** }

    fiOwnership:
      Result := AVersion in [ive2_3, ive2_4];
  else
    Result := True;
  end;
end;

procedure TJvID3OwnershipFrame.WriteFrame;
begin
  { Text encoding           $xx
    Price payed             <text string> $00
    Date of purch.          <text string>
    Seller                  <text string according to encoding>
  }
  with Stream do
  begin
    WriteEncoding;
    WriteStringA(PricePayed);
    WriteTerminatorA;
    WriteDate(DateOfPurch);
    WriteStringEnc(FSeller);
  end;
end;

//=== TJvID3PictureFrame =====================================================

procedure TJvID3PictureFrame.Assign(Source: TPersistent);
var
  Stream: TMemoryStream;
begin
  if Source is TPicture then
    Assign(TPicture(Source).Graphic)
  else
  if Source is TGraphic then
  begin
    Stream := TMemoryStream.Create;
    try
      TGraphic(Source).SaveToStream(Stream);
      Stream.Seek(0, soFromBeginning);
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end
  else
  if Source is TJvID3PictureFrame then
  begin
    FMIMEType := TJvID3PictureFrame(Source).MIMEType;
    FPictureType := TJvID3PictureFrame(Source).PictureType;
    CopyStringPair(TJvID3PictureFrame(Source).FDescription, FDescription);
    FURL := TJvID3PictureFrame(Source).URL;
  end
  else
    inherited Assign(Source);
end;

procedure TJvID3PictureFrame.AssignTo(Dest: TPersistent);
var
  TmpFileName: string;
begin
  if (Dest is TPicture) or (Dest is TGraphic) then
  begin
    if (DataSize > 0) and (MIMEType <> cURLArrow) then
    begin
      TmpFileName := JclFileUtils.FileGetTempName(cPictureFrameFileNameTemplate);
      TmpFileName := ChangeFileExt(TmpFileName, MIMETypeToExt(MIMEType));

      SaveToFile(TmpFileName);
      try
        try
          if Dest is TPicture then
            TPicture(Dest).LoadFromFile(TmpFileName)
          else
          if Dest is TGraphic then
            TGraphic(Dest).LoadFromFile(TmpFileName);
        except
          on EInvalidGraphic do
            ; { Do nothing }
        end
      finally
        SysUtils.DeleteFile(TmpFileName);
      end;
    end
    else
      Dest.Assign(nil);
  end
  else
    inherited AssignTo(Dest);
end;

class function TJvID3PictureFrame.CanAddFrame(AController: TJvID3Controller;
  AFrameID: TJvID3FrameID): Boolean;
begin
  { There may be several pictures attached to one file, each in their
    individual "APIC" frame, but only one with the same content descriptor.
    There may only be one picture with the picture type declared as picture
    type $01 and $02 respectively.
  }
  Result := (AFrameID = fiPicture) or inherited CanAddFrame(AController, AFrameID);
end;

function TJvID3PictureFrame.CheckFrame(const HandleError: TJvID3HandleError): Boolean;
begin
  { The description has a maximum length of 64 characters, but may be empty. }

  Result := CheckMaxCharCount(Self, FDescription, 64, HandleError);
  if not Result and (HandleError = heAutoCorrect) then
  begin
    UpdateFrameSize;
    Result := True;
  end;
end;

procedure TJvID3PictureFrame.Clear;
begin
  FMIMEType := '';
  FPictureType := ptOther;
  ClearStringPair(FDescription);
  FURL := '';

  inherited Clear;
end;

class function TJvID3PictureFrame.Find(AController: TJvID3Controller;
  const AType: TJvID3PictureType): TJvID3PictureFrame;
var
  Frame: TJvID3Frame;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  if not AController.FindFirstFrame(fiPicture, Frame) then
    Exit;

  while (Frame is TJvID3PictureFrame) and
    (TJvID3PictureFrame(Frame).PictureType <> AType) do
    AController.FindNextFrame(fiPicture, Frame);

  if Frame is TJvID3PictureFrame then
    Result := TJvID3PictureFrame(Frame)
end;

class function TJvID3PictureFrame.FindOrCreate(AController: TJvID3Controller;
  const AType: TJvID3PictureType): TJvID3PictureFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController, AType);
  if not Assigned(Result) then
  begin
    Result := TJvID3PictureFrame(AController.AddFrame(fiPicture));
    Result.PictureType := AType;
  end;
end;

function TJvID3PictureFrame.GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal;
begin
  { Text encoding:    $xx
    MIME type:        <text string> $00
    Picture type:     $xx
    Description:      <text string according to encoding> $00 (00)
    Picture data:     <binary data>
  }
  Result := 1 + Cardinal(Length(MIMEType)) + 1 + 1 +
    LengthEnc(FDescription, Encoding, ToEncoding) +
    LengthTerminatorEnc(ToEncoding) + DataSize;
end;

function TJvID3PictureFrame.GetIsEmpty: Boolean;
begin
  { Don't care about FPictureType }
  Result := inherited GetIsEmpty and
    ((Length(MIMEType) = 0) or (MIMEType = cURLArrow)) and
    (Length(URL) = 0) and CheckIsEmpty(FDescription, Encoding);
end;

procedure TJvID3PictureFrame.ReadFrame;
var
  LPictureType: Byte;
begin
  {  Text encoding      $xx
     MIME type          <text string> $00
     Picture type       $xx
     Description        <text string according to encoding> $00 (00)
     Picture data       <binary data>
  }
  with Stream do
  begin
    ReadEncoding;
    ReadStringA(FMIMEType);
    if BytesTillEndOfFrame < 1 then
      Exit;

    Read(LPictureType, 1);
    if LPictureType <= Integer(High(TJvID3PictureType)) then
      FPictureType := TJvID3PictureType(LPictureType)
    else
      FPictureType := ptOther;

    ReadStringEnc(FDescription);

    if MIMEType = cURLArrow then
      { There is the possibility to put only a link to the image file by using
        the 'MIME type' "-->" and having a complete URL instead of picture data.
      }
      ReadStringA(FURL)
    else
      ReadData(BytesTillEndOfFrame);
  end;
end;

function TJvID3PictureFrame.SameUniqueIDAs(const Frame: TJvID3Frame): Boolean;
begin
  { There may be several pictures attached to one file, each in their
    individual "APIC" frame, but only one with the same content descriptor.
    There may only be one picture with the picture type declared as picture
    type $01 and $02 respectively.
  }
  Result := (Frame is TJvID3PictureFrame) and
    (Frame.FrameID = FrameID) and (FrameID = fiPicture);

  if Result then
    Result :=
      (TJvID3PictureFrame(Frame).PictureType = PictureType) and
      ((PictureType in [ptFileIcon, ptOtherFileIcon]) or
       SameStringPair(FDescription, TJvID3PictureFrame(Frame).FDescription,
         Encoding, TJvID3PictureFrame(Frame).Encoding))
  else
    Result := inherited SameUniqueIDAs(Frame);
end;

procedure TJvID3PictureFrame.SetDescription(const Value: string);
begin
  if FDescription.SA <> Value then
  begin
    FDescription.SA := Value;
    Changed;
  end;
end;

procedure TJvID3PictureFrame.SetDescriptionW(const Value: WideString);
begin
  if FDescription.SW <> Value then
  begin
    FDescription.SW := Value;
    Changed;
  end;
end;

procedure TJvID3PictureFrame.SetMIMEType(const Value: string);
begin
  if FMIMEType <> Value then
  begin
    FMIMEType := Value;
    Changed;
  end;
end;

procedure TJvID3PictureFrame.SetURL(const Value: string);
begin
  if FURL <> Value then
  begin
    FURL := Value;
    Changed;
  end;
end;

procedure TJvID3PictureFrame.WriteFrame;
var
  DoWriteURL: Boolean;
begin
  {  Text encoding      $xx
     MIME type          <text string> $00
     Picture type       $xx
     Description        <text string according to encoding> $00 (00)
     Picture data       <binary data>

     There is the possibility to put only a link to the image file by using
     the 'MIME type' "-->" and having a complete URL instead of picture data. }

  DoWriteURL := (DataSize = 0) and (URL > '');

  with Stream do
  begin
    WriteEncoding;
    if DoWriteURL then
      WriteStringA(cURLArrow)
    else
      WriteStringA(MIMEType);
    WriteTerminatorA;

    Write(PictureType, 1);

    WriteStringEnc(FDescription);
    WriteTerminatorEnc;
    if DoWriteURL then
      WriteStringA(URL)
    else
      WriteData;
  end;
end;

//=== TJvID3PlayCounterFrame =================================================

procedure TJvID3PlayCounterFrame.Assign(Source: TPersistent);
begin
  if Source is TJvID3PlayCounterFrame then
    FCounter := TJvID3PlayCounterFrame(Source).Counter;

  inherited Assign(Source);
end;

class function TJvID3PlayCounterFrame.CanAddFrame(AController: TJvID3Controller;
  AFrameID: TJvID3FrameID): Boolean;
begin
  { There may only be one "PCNT" frame in each tag. }
  Result := not AController.HasFrame(AFrameID) or
    inherited CanAddFrame(AController, AFrameID);
end;

function TJvID3PlayCounterFrame.CheckFrame(const HandleError: TJvID3HandleError): Boolean;
begin
  Result := True;
end;

procedure TJvID3PlayCounterFrame.Clear;
begin
  FCounter := 0;
  inherited Clear;
end;

class function TJvID3PlayCounterFrame.Find(AController: TJvID3Controller): TJvID3PlayCounterFrame;
var
  Frame: TJvID3Frame;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  Frame := AController.Frames.FindFrame(fiPlayCounter);
  if Frame is TJvID3PlayCounterFrame then
    Result := TJvID3PlayCounterFrame(Frame)
end;

class function TJvID3PlayCounterFrame.FindOrCreate(AController: TJvID3Controller): TJvID3PlayCounterFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController);
  if not Assigned(Result) then
    Result := TJvID3PlayCounterFrame(AController.AddFrame(fiPlayCounter));
end;

function TJvID3PlayCounterFrame.GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal;
begin
  Result := 4;
end;

function TJvID3PlayCounterFrame.GetIsEmpty: Boolean;
begin
  Result := False;
end;

procedure TJvID3PlayCounterFrame.ReadFrame;
begin
  Stream.ReadNumber(FCounter);
end;

function TJvID3PlayCounterFrame.SameUniqueIDAs(const Frame: TJvID3Frame): Boolean;
begin
  { There may only be one "PCNT" frame in each tag. }
  Result := ((Frame.FrameID = FrameID) and (FrameID = fiPlayCounter)) or
    inherited SameUniqueIDAs(Frame);
end;

procedure TJvID3PlayCounterFrame.SetCounter(const Value: Cardinal);
begin
  if FCounter <> Value then
  begin
    FCounter := Value;
    Changed;
  end;
end;

procedure TJvID3PlayCounterFrame.WriteFrame;
begin
  Stream.WriteNumber(FCounter);
end;

//=== TJvID3PopularimeterFrame ===============================================

procedure TJvID3PopularimeterFrame.Assign(Source: TPersistent);
begin
  if Source is TJvID3PopularimeterFrame then
  begin
    FRating := TJvID3PopularimeterFrame(Source).Rating;
    FCounter := TJvID3PopularimeterFrame(Source).Counter;
    FEMailAddress := TJvID3PopularimeterFrame(Source).EMailAddress;
  end;

  inherited Assign(Source);
end;

class function TJvID3PopularimeterFrame.CanAddFrame(AController: TJvID3Controller;
  AFrameID: TJvID3FrameID): Boolean;
begin
  { There may be more than one "POPM" frame in each tag, but only one with the
    same email address. }
  Result := (AFrameID = fiPopularimeter) or inherited CanAddFrame(AController, AFrameID);
end;

function TJvID3PopularimeterFrame.CheckFrame(const HandleError: TJvID3HandleError): Boolean;
begin
  Result := True;
end;

procedure TJvID3PopularimeterFrame.Clear;
begin
  FRating := 0;
  FCounter := 0;
  FEMailAddress := '';

  inherited Clear;
end;

class function TJvID3PopularimeterFrame.Find(AController: TJvID3Controller): TJvID3PopularimeterFrame;
var
  Frame: TJvID3Frame;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  Frame := AController.Frames.FindFrame(fiPopularimeter);
  if Frame is TJvID3PopularimeterFrame then
    Result := TJvID3PopularimeterFrame(Frame);
end;

class function TJvID3PopularimeterFrame.Find(AController: TJvID3Controller;
  const AEmailAddress: string): TJvID3PopularimeterFrame;
var
  Frame: TJvID3Frame;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  if not AController.FindFirstFrame(fiPopularimeter, Frame) then
    Exit;

  while (Frame is TJvID3PopularimeterFrame) and
    not AnsiSameStr(AEmailAddress, TJvID3PopularimeterFrame(Frame).EMailAddress) do
    AController.FindNextFrame(fiPopularimeter, Frame);

  if Frame is TJvID3PopularimeterFrame then
    Result := TJvID3PopularimeterFrame(Frame);
end;

class function TJvID3PopularimeterFrame.FindOrCreate(AController: TJvID3Controller): TJvID3PopularimeterFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController);
  if not Assigned(Result) then
    Result := TJvID3PopularimeterFrame(AController.AddFrame(fiPopularimeter));
end;

class function TJvID3PopularimeterFrame.FindOrCreate(AController: TJvID3Controller;
  const AEmailAddress: string): TJvID3PopularimeterFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController, AEmailAddress);
  if not Assigned(Result) then
  begin
    Result := TJvID3PopularimeterFrame(AController.AddFrame(fiPopularimeter));
    Result.EMailAddress := AEmailAddress;
  end;
end;

function TJvID3PopularimeterFrame.GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal;
begin
  { Email to user               <text string> $00
    Rating                      $xx
    Counter                     $xx xx xx xx (xx ...)
  }
  Result := Length(FEMailAddress) + 1 + 1 + 4;
end;

function TJvID3PopularimeterFrame.GetIsEmpty: Boolean;
begin
  Result := (FRating = 0) and (FCounter = 0) and (Length(FEMailAddress) = 0);
end;

procedure TJvID3PopularimeterFrame.ReadFrame;
begin
  { Email to user               <text string> $00
    Rating                      $xx
    Counter                     $xx xx xx xx (xx ...)
  }
  with Stream do
  begin
    ReadStringA(FEMailAddress);
    Read(FRating, 1);
    ReadNumber(FCounter);
  end;
end;

function TJvID3PopularimeterFrame.SameUniqueIDAs(const Frame: TJvID3Frame): Boolean;
begin
  { There may be more than one "POPM" frame in each tag, but only one with the
    same email address. }
  Result := (Frame is TJvID3PopularimeterFrame) and
    (Frame.FrameID = FrameID) and (FrameID = fiPopularimeter);

  if Result then
    Result := AnsiSameStr(TJvID3PopularimeterFrame(Frame).EMailAddress, EMailAddress)
  else
    Result := inherited SameUniqueIDAs(Frame);
end;

procedure TJvID3PopularimeterFrame.SetCounter(const Value: Cardinal);
begin
  if FCounter <> Value then
  begin
    FCounter := Value;
    Changed;
  end;
end;

procedure TJvID3PopularimeterFrame.SetEMailAddress(const Value: string);
begin
  if FEMailAddress <> Value then
  begin
    FEMailAddress := Value;
    Changed;
  end;
end;

procedure TJvID3PopularimeterFrame.SetRating(const Value: Byte);
begin
  if FRating <> Value then
  begin
    FRating := Value;
    Changed;
  end;
end;

procedure TJvID3PopularimeterFrame.WriteFrame;
begin
  { Email to user               <text string> $00
    Rating                      $xx
    Counter                     $xx xx xx xx (xx ...)
  }
  with Stream do
  begin
    WriteStringA(EMailAddress);
    WriteTerminatorA;
    Write(Rating, 1);
    WriteNumber(Counter);
  end;
end;

//=== TJvID3SimpleListFrame ==================================================

procedure TJvID3SimpleListFrame.AfterConstruction;
begin
  inherited AfterConstruction;

  FList := TJvID3StringList.Create;
  FListW := TWideStringList.Create;

  TStringList(FList).OnChange := ListChanged;
  // (rom) changed FList to FListW
  FListW.OnChange := ListChanged;
end;

procedure TJvID3SimpleListFrame.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FList.Free;
  FListW.Free;
end;

function TJvID3SimpleListFrame.CheckFrame(const HandleError: TJvID3HandleError): Boolean;
begin
  Result := False;
  case FrameID of
    fiLanguage:
      case Encoding of
        ienISO_8859_1:
          Result := CheckIsLanguageListA(Self, List, HandleError);
        ienUTF_16, ienUTF_16BE, ienUTF_8:
          Result := CheckIsLanguageListW(Self, ListW, HandleError);
      else
        Error(RsEID3UnknownEncoding);
      end;
  else
    case Encoding of
      ienISO_8859_1:
        Result := CheckListA(Self, List, Separator, HandleError);
      ienUTF_16, ienUTF_16BE, ienUTF_8:
        Result := CheckListW(Self, ListW, SeparatorW, HandleError);
    else
      Error(RsEID3UnknownEncoding);
    end;
  end;

  if not Result and (HandleError = heAutoCorrect) then
  begin
    UpdateFrameSize;
    Result := True;
  end;
end;

class function TJvID3SimpleListFrame.Find(AController: TJvID3Controller;
  const AFrameID: TJvID3FrameID): TJvID3SimpleListFrame;
var
  Frame: TJvID3Frame;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  Frame := AController.Frames.FindFrame(AFrameID);
  if Frame is TJvID3SimpleListFrame then
    Result := TJvID3SimpleListFrame(Frame);
end;

class function TJvID3SimpleListFrame.FindOrCreate(AController: TJvID3Controller;
  const AFrameID: TJvID3FrameID): TJvID3SimpleListFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController, AFrameID);
  if not Assigned(Result) then
  begin
    AController.CheckFrameClass(TJvID3SimpleListFrame, AFrameID);
    Result := TJvID3SimpleListFrame(AController.AddFrame(AFrameID));
  end;
end;

function TJvID3SimpleListFrame.GetFixedStringLength: Integer;
begin
  case FrameID of
    fiLanguage:
      Result := 3
  else
    Result := -1;
  end;
end;

function TJvID3SimpleListFrame.GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal;
var
  I: Integer;
  SP: TJvID3StringPair;
begin
  if ToEncoding = ienUTF_8 then
  begin
    GetText(SP);
    Result := 1 + Length(WideStringToUTF8(GetStringW(SP, Encoding)));
    Exit;
  end;

  { Encoding byte = 1 }
  Result := 1;

  case Encoding of
    ienISO_8859_1:
      begin
        if FixedStringLength > 0 then
          Inc(Result, List.Count * FixedStringLength)
        else
        begin
          for I := 0 to List.Count - 1 do
            Inc(Result, Length(List[I]) + 1);
          { Set one separator less, the last line does not have a trailing
            separator }
          Dec(Result);
        end;
        case ToEncoding of
          ienISO_8859_1:
            { Nothing };
          ienUTF_16:
            { x2, + BOM's }
            Result := Result * 2 + 2 * Cardinal(List.Count);
          ienUTF_16BE:
            { x2 }
            Result := Result * 2;
        else
          Error(RsEID3UnknownEncoding);
        end;
      end;
    ienUTF_16, ienUTF_16BE, ienUTF_8:
      begin
        if FixedStringLength > 0 then
          Inc(Result, ListW.Count * FixedStringLength * 2)
        else
        begin
          for I := 0 to ListW.Count - 1 do
            Inc(Result, Length(ListW[I]) + 2);

          { Set one separator less, the last line does not have a trailing
            separator }
          Dec(Result, 2);
        end;

        case ToEncoding of
          ienISO_8859_1:
            Result := Result div 2;
          ienUTF_16:
            { Add the BOM's }
            Inc(Result, ListW.Count * 2);
          ienUTF_16BE:
            { Nothing };
        else
          Error(RsEID3UnknownEncoding);
        end;
      end;
  else
    Error(RsEID3UnknownEncoding);
  end;
end;

function TJvID3SimpleListFrame.GetSeparator: Char;
begin
  case FrameID of
    fiLyricist, fiComposer, fiOrigLyricist, fiOrigArtist, fiLeadArtist:
      Result := '/';
    fiLanguage:
      Result := #0;
  else
    { ?? Unknown }
    Result := '/';
  end;
end;

function TJvID3SimpleListFrame.GetSeparatorW: WideChar;
begin
  case FrameID of
    fiLyricist, fiComposer, fiOrigLyricist, fiOrigArtist, fiLeadArtist:
      Result := WideChar('/');
    fiLanguage:
      Result := WideNull;
  else
    { ?? Unknown }
    Result := WideChar('/');
  end;
end;

procedure TJvID3SimpleListFrame.GetText(var AText: TJvID3StringPair);
begin
  case Encoding of
    ienISO_8859_1:
      if Separator <> #0 then
        AText.SA := TJvID3StringList(FList).GetSeparatedText(Separator)
      else
        AText.SA := TJvID3StringList(FList).GetSeparatedText('');
    ienUTF_16, ienUTF_16BE, ienUTF_8:
      if SeparatorW <> WideNull then
        AText.SW := ListW.GetSeparatedText(SeparatorW)
      else
        AText.SW := ListW.GetSeparatedText('');
  else
    Error(RsEID3UnknownEncoding);
  end;
end;

procedure TJvID3SimpleListFrame.ListChanged(Sender: TObject);
begin
  if not (icsReading in Controller.FState) then
    Changed;
end;

procedure TJvID3SimpleListFrame.NewText(const ANewText: TJvID3StringPair);
begin
  case Encoding of
    ienISO_8859_1:
      if FixedStringLength >= 0 then
        ExtractFixedStringsA(PChar(ANewText.SA), FixedStringLength, List)
      else
        ExtractStringsA(Separator, PChar(ANewText.SA), List);
    ienUTF_16, ienUTF_16BE, ienUTF_8:
      if FixedStringLength >= 0 then
        ExtractFixedStringsW(PWideChar(ANewText.SW), FixedStringLength, ListW)
      else
        ExtractStringsW(SeparatorW, PWideChar(ANewText.SW), ListW);
  else
    Error(RsEID3UnknownEncoding);
  end;
end;

function TJvID3SimpleListFrame.GetList: TStrings;
begin
  Result := FList;
end;

function TJvID3SimpleListFrame.GetListW: TWideStrings;
begin
  Result := FListW;
end;

procedure TJvID3SimpleListFrame.SetList(const Value: TStrings);
begin
  FList.Assign(Value);
end;

procedure TJvID3SimpleListFrame.SetListW(const Value: TWideStrings);
begin
  FListW.Assign(Value);
end;

//=== TJvID3SkipFrame ========================================================

procedure TJvID3SkipFrame.ChangeToVersion(const ANewVersion: TJvID3Version);
var
  LFrameID: TJvID3FrameID;
begin
  case ANewVersion of
    ive2_2:
      if Length(FFrameIDStr) = 4 then
      begin
        LFrameID := ID3_StringToFrameID(FFrameIDStr);
        if LFrameID in [fiErrorFrame, fiPaddingFrame] then
          FFrameIDStr := ''
        else
          FFrameIDStr := ID3_FrameIDToString(LFrameID, 3);
      end;
    ive2_3, ive2_4:
      if Length(FFrameIDStr) = 3 then
      begin
        LFrameID := ID3_StringToFrameID(FFrameIDStr);
        if LFrameID in [fiErrorFrame, fiPaddingFrame] then
          FFrameIDStr := ''
        else
          FFrameIDStr := ID3_FrameIDToString(LFrameID, 3);
      end;
  end;
end;

//=== TJvID3Stream ===========================================================

procedure TJvID3Stream.BeginReadFrame(const AFrameSize: Integer);
begin
  if FReadingFrame or FWritingFrame then
    Id3Error(RsEAlreadyReadingWritingFrame);

  FStartPosition := Position;
  FCurrentFrameSize := AFrameSize;
  FReadingFrame := True;
end;

procedure TJvID3Stream.BeginWriteFrame(const AFrameSize: Integer);
begin
  if FReadingFrame or FWritingFrame then
    Id3Error(RsEAlreadyReadingWritingFrame);

  //if not Assigned(Memory) then
  //  { $0A = 10, the size of the header }
  //  Capacity := $0A;

  FStartPosition := Position;
  FCurrentFrameSize := AFrameSize;
  FWritingFrame := True;
end;

function TJvID3Stream.CanRead(const ACount: Cardinal): Boolean;
var
  LBytesToRead: Longint;
begin
  Assert(not FWritingFrame, RsECannotCallCanRead);

  if FReadingFrame then
    LBytesToRead := BytesTillEndOfFrame
  else
    LBytesToRead := BytesTillEndOfTag;

  Result := (LBytesToRead >= 0) and (ACount <= Cardinal(LBytesToRead));
end;

procedure TJvID3Stream.EndReadFrame;
begin
  if not FReadingFrame then
    Id3Error(RsENotReadingFrame);
  MoveToNextFrame;
  FReadingFrame := False;
end;

procedure TJvID3Stream.EndWriteFrame;
begin
  if not FWritingFrame then
    Id3Error(RsENotWritingFrame);
  MoveToNextFrame;
  FWritingFrame := False;
end;

function TJvID3Stream.GetBytesTillEndOfFrame: Longint;
begin
  Result := FStartPosition + FCurrentFrameSize - Position;
end;

function TJvID3Stream.GetBytesTillEndOfTag: Longint;
begin
  Result := Size - Position;
end;

function TJvID3Stream.InFrame(P: PChar): Boolean;
begin
  { This function is used to check _when_ we're reading a frame, that we don't
    read beyond the end marker }

  Result := not FReadingFrame or (P < PChar(Memory) + FStartPosition + FCurrentFrameSize);
end;

procedure TJvID3Stream.InitAllowedEncodings(const AVersion: TJvID3Version;
  const AEncoding: TJvID3ForceEncoding);
begin
  if AEncoding = ifeDontCare then
    case AVersion of
      ive2_2, ive2_3:
        FAllowedEncodings := [ienISO_8859_1, ienUTF_16];
      ive2_4:
        FAllowedEncodings := [ienISO_8859_1, ienUTF_16, ienUTF_16BE, ienUTF_8];
    else
      ID3Error(RsEID3UnknownVersion);
    end
  else
  begin
    { Convert force encoding type to encoding type }
    FAllowedEncodings := [CForceEncodingToEncoding[AEncoding]];
    if (AVersion in [ive2_2, ive2_3]) and (FAllowedEncodings * [ienUTF_16BE, ienUTF_8] <> []) then
      FAllowedEncodings := [ienUTF_16];
  end;

  UpdateDestEncoding;
end;

procedure TJvID3Stream.MoveToNextFrame;
begin
  if FWritingFrame and (BytesTillEndOfFrame <> 0) then
    Id3Error(RsEFrameSizeDiffers);

  Seek(BytesTillEndOfFrame, soFromCurrent);
end;

function TJvID3Stream.ReadDate(var ADate: TDateTime): Longint;
var
  Year, Month, Day: Word;
  P: PChar;
begin
  P := PChar(Memory) + Position;

  Year := 0;
  Month := 0;
  Day := 0;
  Result := 0;

  while (Result < 8) and InFrame(P) and (P^ in DigitSymbols) do
  begin
    { Use Day as temp variable }
    Day := Day * 10 + Ord(P^) - Ord('0');

    { Format = YYYYMMDD }
    case Result of
      3:
        begin
          Year := Day;
          Day := 0;
        end;
      5:
        begin
          Month := Day;
          Day := 0;
        end;
    end;
    Inc(P);
    Inc(Result);
  end;

  if Result = 8 then
  begin
    Seek(Result, soFromCurrent);
    try
      ADate := EncodeDate(Year, Month, Day);
    except
      on EConvertError do
        ADate := 0;
    end;
  end
  else
  begin
    Result := 0;
    ADate := 0;
  end;
end;

function TJvID3Stream.ReadEnc(var AEncoding: TJvID3Encoding): Longint;
var
  B: Byte;
begin
  Result := Read(B, 1);
  if B <= Integer(High(TJvID3Encoding)) then
    SourceEncoding := TJvID3Encoding(B)
  else
    ID3Error(RsEID3UnknownEncoding);

  AEncoding := DestEncoding;
end;

function TJvID3Stream.ReadFixedNumber(var AValue: Cardinal): Longint;
begin
  Result := Read(AValue, 4);
  { Swap byte order from big endian to little endian }
  AValue := ReverseBytes(AValue);
end;

function TJvID3Stream.ReadFixedNumber3(var AValue: Cardinal): Longint;
type
  TBytes = array [0..3] of Byte;
begin
  AValue := 0;
  Result := Read(TBytes(AValue)[1], 3);
  { Swap byte order from big endian to little endian }
  AValue := ReverseBytes(AValue);
end;

procedure TJvID3Stream.ReadFromStream(AStream: TStream;
  const ASize: Integer);
begin
  Position := 0;
  SetSize(ASize);
  if ASize <> 0 then
    AStream.ReadBuffer(Memory^, ASize);
end;

function TJvID3Stream.ReadLanguage(var Language: string): Longint;
begin
  if not CanRead(3) then
    Result := 0
  else
  begin
    SetLength(Language, 3);
    Result := Read(Language[1], 3);
  end;

  if Result < 3 then
  begin
    Language := '';
    Exit;
  end;
end;

function TJvID3Stream.ReadNumber(var AValue: Cardinal): Longint;
begin
  { When reading a frame, a number _always_ fills up the remaining part of
    the frame; a number might be bigger than 4 bytes, but that can't be read
    currently }
  if not FReadingFrame then
    Id3Error(RsENotReadingFrame);

  if BytesTillEndOfFrame = 4 then
  begin
    Result := Read(AValue, 4);
    { Swap byte order from big endian to little endian }
    AValue := ReverseBytes(AValue);
  end
  else
  begin
    { Error (if BytesTillEndOfFrame < 4) or not implemented (if BytesTillEndOfFrame > 4) }
    AValue := 0;
    Result := 0;
  end;
end;

function TJvID3Stream.ReadStringA(var SA: string): Longint;
var
  P, StartPos: PChar;
begin
  StartPos := PChar(Memory) + Position;
  P := StartPos;

  while (P^ <> #0) and InFrame(P) do
    Inc(P);
  Result := P - StartPos;

  SetString(SA, StartPos, Result);

  { Skip terminator }
  if InFrame(P) then
    Inc(Result);

  Seek(Result, soFromCurrent);
end;

function TJvID3Stream.ReadStringEnc(var S: TJvID3StringPair): Longint;
begin
  case SourceEncoding of
    ienISO_8859_1:
      Result := ReadStringA(S.SA);
    ienUTF_16, ienUTF_16BE:
      Result := ReadStringW(S.SW);
    ienUTF_8:
      Result := ReadStringUTF8(S.SW);
  else
    Result := 0;
    ID3Error(RsEID3UnknownEncoding);
  end;

  if SourceEncoding <> DestEncoding then
    TranslatePairString(S, SourceEncoding, DestEncoding);
end;

function TJvID3Stream.ReadStringUTF8(var SW: WideString): Longint;
var
  SA: string;
begin
  Result := ReadStringA(SA);
  SW := UTF8ToWideString(SA);
end;

function TJvID3Stream.ReadStringW(var SW: WideString): Longint;
var
  Order: WideChar;
  P: PWideChar;
  StartPos: PChar;
  TerminatorFound: Boolean;
  WideCharCount: Integer;
begin
  Result := 0;

  if SourceEncoding = ienUTF_16 then
  begin
    { Try read the BOM }
    if not CanRead(2) then
    begin
      SW := '';
      Exit;
    end;

    Result := Read(Order, 2);
    if (Order <> BOM_LSB_FIRST) and (Order <> BOM_MSB_FIRST) then
    begin
      SW := '';
      Exit;
    end;
  end;

  StartPos := PChar(Memory) + Position;
  P := PWideChar(StartPos);

  { Read until #0#0 found or until FEndMarker }
  while InFrame(PChar(P)) and not (P^ = WideNull) do
    Inc(P);

  TerminatorFound := InFrame(PChar(P));
  WideCharCount := (PChar(P) - StartPos) div 2;
  Result := Result + WideCharCount * 2;

  SetLength(SW, WideCharCount);
  if WideCharCount > 0 then
    Move(StartPos[0], SW[1], 2 * WideCharCount);
  if (SourceEncoding = ienUTF_16) and (Order = BOM_MSB_FIRST) then
    StrSwapByteOrder(PWideChar(SW));

  { Skip Terminator }
  if TerminatorFound then
  begin
    Inc(Result, 2);
    Inc(WideCharCount);
  end;

  Seek(WideCharCount * 2, soFromCurrent);
end;

function TJvID3Stream.ReadSyncSafeInteger(var AInt: Cardinal;
  const ASize: Byte): Longint;
var
  Value: PChar;
begin
  GetMem(Value, ASize);
  try
    Result := Read(Value^, ASize);
    UnSyncSafe(Value^, ASize, AInt);
  finally
    FreeMem(Value);
  end;
end;

function TJvID3Stream.ReadSyncSafeInteger(var AInt: Int64;
  const ASize: Byte): Longint;
var
  Value: PChar;
begin
  GetMem(Value, ASize);
  try
    Result := Read(Value^, ASize);
    UnSyncSafe(Value^, ASize, AInt);
  finally
    FreeMem(Value);
  end;
end;

function TJvID3Stream.ReadSyncSafeInteger(var AInt: Cardinal): Longint;
var
  Value: Cardinal;
begin
  Result := Read(Value, 4);
  UnSyncSafe(Value, 4, AInt);
end;

function TJvID3Stream.ReadUserString(var S1, S2: TJvID3StringPair): Longint;
begin
  case SourceEncoding of
    ienISO_8859_1:
      Result := ReadUserStringA(S1.SA, S2.SA);
    ienUTF_16, ienUTF_16BE:
      Result := ReadUserStringW(S1.SW, S2.SW);
    ienUTF_8:
      Result := ReadUserStringUTF8(S1.SW, S2.SW);
  else
    Result := 0;
    ID3Error(RsEID3UnknownEncoding);
  end;

  if SourceEncoding <> DestEncoding then
  begin
    TranslatePairString(S1, SourceEncoding, DestEncoding);
    TranslatePairString(S2, SourceEncoding, DestEncoding);
  end;
end;

function TJvID3Stream.ReadUserStringA(var SA1, SA2: string): Longint;
begin
  Result := ReadStringA(SA1);

  if CanRead(1) then
    Result := Result + ReadStringA(SA2)
  else
    SA2 := '';
end;

function TJvID3Stream.ReadUserStringUTF8(var SW1, SW2: WideString): Longint;
var
  SA1, SA2: string;
begin
  Result := ReadUserStringA(SA1, SA2);
  SW1 := UTF8ToWideString(SA1);
  SW2 := UTF8ToWideString(SA2);
end;

function TJvID3Stream.ReadUserStringW(var SW1, SW2: WideString): Longint;
begin
  Result := ReadStringW(SW1);

  if CanRead(2) then
    Result := Result + ReadStringW(SW2)
  else
    SW2 := '';
end;

procedure TJvID3Stream.SetSourceEncoding(const Value: TJvID3Encoding);
begin
  if FSourceEncoding <> Value then
  begin
    FSourceEncoding := Value;
    UpdateDestEncoding;
  end;
end;

procedure TJvID3Stream.UpdateDestEncoding;
const
  CEncodingTry: array [0..3] of TJvID3Encoding =
    (ienUTF_16, ienUTF_16BE, ienUTF_8, ienISO_8859_1);
var
  I: Integer;
begin
  { FSourceEncoding is the encoding of a specific frame; the controller
    may prevent writing of some encodings (for example if the
    version (2.3) doesn't support it).

    Therefore we use FDestEncoding, that is set to the encoding actually
    written to the stream
    (when writing, symetrically for reading )
  }
  Assert(FAllowedEncodings <> [], RsEAllowedEncodingsIsEmpty);

  FDestEncoding := FSourceEncoding;
  if not (FDestEncoding in FAllowedEncodings) then
  begin
    I := 0;
    while (I <= High(CEncodingTry)) and not (CEncodingTry[I] in FAllowedEncodings) do
      Inc(I);
    if I > High(CEncodingTry) then
      // insanity, should not happen
      Id3Error(RsECouldNotFindAllowableEncoding);

    FDestEncoding := CEncodingTry[I];
  end;
end;

function TJvID3Stream.WriteDate(const ADate: TDateTime): Longint;
var
  Year, Month, Day: Word;
  S: string;
begin
  { Format = YYYYMMDD }
  DecodeDate(ADate, Year, Month, Day);
  S := Format('%.4d%.2d%.2d', [Year, Month, Day]);
  Result := WriteStringA(S);
end;

function TJvID3Stream.WriteEnc: Longint;
begin
  Result := Write(DestEncoding, 1);
end;

function TJvID3Stream.WriteFixedNumber(AValue: Cardinal): Longint;
begin
  { Swap byte order from little endian to big endian }
  AValue := ReverseBytes(AValue);
  Result := Write(AValue, 4);
end;

function TJvID3Stream.WriteFixedNumber3(AValue: Cardinal): Longint;
type
  TBytes = array [0..3] of Byte;
begin
  Assert(AValue <= $00FFFFFF, RsEValueTooBig);

  { Swap byte order from little endian to big endian }
  AValue := ReverseBytes(AValue);
  Result := Write(TBytes(AValue)[1], 3);
end;

function TJvID3Stream.WriteLanguage(const Language: string): Longint;
begin
  if Length(Language) <> 3 then
    Id3Error(RsELanguageNotOfLength3);

  Result := WriteStringA(Language);
end;

function TJvID3Stream.WriteNumber(AValue: Cardinal): Longint;
begin
  { Swap byte order from little endian to big endian }
  AValue := ReverseBytes(AValue);
  Result := Write(AValue, 4);
end;

function TJvID3Stream.WritePadding(const Count: Longint): Longint;
var
  Pos: Longint;
begin
  Pos := Position + Count;
  if Pos > 0 then
  begin
    if Pos > Size then
    begin
      if Pos > Capacity then
        Capacity := Pos;
      Size := Pos;
    end;
    FillChar(Pointer(Longint(Memory) + Position)^, Count, #0);
    //System.Move(Buffer, Pointer(Longint(FMemory) + FPosition)^, Count);
    Position := Pos;
    Result := Count;
    Exit;
  end;
  Result := 0;
end;

function TJvID3Stream.WriteStringA(const SA: string): Longint;
begin
  Result := Write(PChar(SA)^, Length(SA));
end;

function TJvID3Stream.WriteStringEnc(const S: TJvID3StringPair): Longint;
begin
  case DestEncoding of
    ienISO_8859_1:
      Result := WriteStringA(GetStringA(S, SourceEncoding));
    ienUTF_16, ienUTF_16BE:
      Result := WriteStringW(GetStringW(S, SourceEncoding));
    ienUTF_8:
      Result := WriteStringUTF8(GetStringW(S, SourceEncoding));
  else
    Result := 0;
    ID3Error(RsEID3UnknownEncoding);
  end;
end;

function TJvID3Stream.WriteStringUTF8(const SW: WideString): Longint;
var
  SA: string;
begin
  SA := WideStringToUTF8(SW);
  Result := WriteStringA(SA);
end;

function TJvID3Stream.WriteStringW(const SW: WideString): Longint;
var
  Order: WideChar;
begin
  Result := 0;

  if DestEncoding = ienUTF_16 then
  begin
    Order := BOM_LSB_FIRST;
    Result := Write(Order, 2);
  end;

  Result := Result + Write(SW[1], 2 * Length(SW));
end;

function TJvID3Stream.WriteSyncSafeInteger(const AInt: Int64;
  const ASize: Byte): Longint;
var
  Value: PChar;
begin
  GetMem(Value, ASize);
  try
    SyncSafe(AInt, Value^, ASize);
    Result := Write(Value^, ASize);
  finally
    FreeMem(Value);
  end;
end;

function TJvID3Stream.WriteSyncSafeInteger(const AInt: Cardinal;
  const ASize: Byte): Longint;
var
  Value: PChar;
begin
  GetMem(Value, ASize);
  try
    SyncSafe(AInt, Value^, ASize);
    Result := Write(Value^, ASize);
  finally
    FreeMem(Value);
  end;
end;

function TJvID3Stream.WriteSyncSafeInteger(const AInt: Cardinal): Longint;
var
  Value: Cardinal;
begin
  SyncSafe(AInt, Value, 4);
  Result := Write(Value, 4);
end;

function TJvID3Stream.WriteTerminatorA: Longint;
var
  Ch: Char;
begin
  Ch := #0;
  Result := Write(Ch, 1);
end;

function TJvID3Stream.WriteTerminatorEnc: Longint;
begin
  case DestEncoding of
    ienISO_8859_1, ienUTF_8:
      Result := WriteTerminatorA;
    ienUTF_16, ienUTF_16BE:
      Result := WriteTerminatorW;
  else
    Result := 0;
    ID3Error(RsEID3UnknownEncoding);
  end;
end;

function TJvID3Stream.WriteTerminatorW: Longint;
var
  Ch: WideChar;
begin
  Ch := WideNull;
  Result := Write(Ch, 2);
end;

function TJvID3Stream.WriteUserString(const S1, S2: TJvID3StringPair): Longint;
begin
  case DestEncoding of
    ienISO_8859_1:
      Result := WriteUserStringA(
        GetStringA(S1, SourceEncoding), GetStringA(S2, SourceEncoding));
    ienUTF_16, ienUTF_16BE:
      Result := WriteUserStringW(
        GetStringW(S1, SourceEncoding), GetStringW(S2, SourceEncoding));
    ienUTF_8:
      Result := WriteUserStringUTF8(
        GetStringW(S1, SourceEncoding), GetStringW(S2, SourceEncoding));
  else
    Result := 0;
    ID3Error(RsEID3UnknownEncoding);
  end;
end;

function TJvID3Stream.WriteUserStringA(const SA1, SA2: string): Longint;
begin
  Result := WriteStringA(SA1) + WriteTerminatorA + WriteStringA(SA2);
end;

function TJvID3Stream.WriteUserStringUTF8(const SW1, SW2: WideString): Longint;
var
  SA1, SA2: string;
begin
  SA1 := WideStringToUTF8(SW1);
  SA2 := WideStringToUTF8(SW2);
  Result := WriteUserStringA(SA1, SA2);
end;

function TJvID3Stream.WriteUserStringW(const SW1, SW2: WideString): Longint;
begin
  Result := WriteStringW(SW1) + WriteTerminatorW + WriteStringW(SW2);
end;

//=== TJvID3StringList =======================================================

function TJvID3StringList.GetSeparatedText(const Separator: string): string;
var
  I, L: Integer;
  Size: Integer;
  Count: Integer;
  SepLen: Integer;
  P: PChar;
  S: string;
begin
  Count := GetCount;
  Size := 0;
  SepLen := Length(Separator);
  for I := 0 to Count - 1 do
    Inc(Size, Length(Get(I)) + SepLen);

  // set one separator less, the last line does not need a trailing separator
  SetLength(Result, Size - SepLen);
  if Size > 0 then
  begin
    P := Pointer(Result);
    I := 0;
    while True do
    begin
      S := Get(I);
      L := Length(S);
      if L <> 0 then
      begin
        // add current string
        System.Move(Pointer(S)^, P^, L);
        Inc(P, L);
      end;
      Inc(I);
      if I = Count then
        Break;

      // add separators
      if SepLen <> 0 then
      begin
        System.Move(Pointer(Separator)^, P^, SepLen);
        Inc(P, SepLen);
      end;
    end;
  end;
end;

//=== TJvID3TermsOfUseFrame ==================================================

procedure TJvID3TermsOfUseFrame.Assign(Source: TPersistent);
begin
  if Source is TJvID3TermsOfUseFrame then
  begin
    CopyStringPair(TJvID3TermsOfUseFrame(Source).FText, FText);
    FLanguage := TJvID3TermsOfUseFrame(Source).FLanguage;
  end;

  inherited Assign(Source);
end;

class function TJvID3TermsOfUseFrame.CanAddFrame(AController: TJvID3Controller;
  AFrameID: TJvID3FrameID): Boolean;
begin
  { There may only be one 'USER' frame in a tag}
  Result := ((AFrameID = fiTermsOfUse) and not AController.HasFrame(fiTermsOfUse)) or
    inherited CanAddFrame(AController, AFrameID);
end;

function TJvID3TermsOfUseFrame.CheckFrame(const HandleError: TJvID3HandleError): Boolean;
begin
  Result := CheckIsLanguageA(Self, FLanguage, HandleError);

  { If something has changed update the framesize }
  if not Result and (HandleError = heAutoCorrect) then
  begin
    UpdateFrameSize;
    Result := True;
  end;
end;

procedure TJvID3TermsOfUseFrame.Clear;
begin
  ClearStringPair(FText);
  FLanguage := '';
  inherited Clear;
end;

class function TJvID3TermsOfUseFrame.Find(AController: TJvID3Controller): TJvID3TermsOfUseFrame;
var
  Frame: TJvID3Frame;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  Frame := AController.Frames.FindFrame(fiTermsOfUse);
  if Frame is TJvID3TermsOfUseFrame then
    Result := TJvID3TermsOfUseFrame(Frame);
end;

class function TJvID3TermsOfUseFrame.FindOrCreate(AController: TJvID3Controller): TJvID3TermsOfUseFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController);
  if not Assigned(Result) then
    Result := TJvID3TermsOfUseFrame(AController.AddFrame(fiTermsOfUse));
end;

function TJvID3TermsOfUseFrame.GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal;
begin
  { Text encoding          $xx
    Language               $xx xx xx
    The actual text        <text string according to encoding>
  }
  Result := 1 + 3 + LengthEnc(FText, Encoding, ToEncoding);
end;

function TJvID3TermsOfUseFrame.GetIsEmpty: Boolean;
begin
  Result := CheckIsEmpty(FText, Encoding) and (Length(FLanguage) = 0);
end;

procedure TJvID3TermsOfUseFrame.ReadFrame;
begin
  { Text encoding          $xx
    Language               $xx xx xx
    The actual text        <text string according to encoding>
  }
  with Stream do
  begin
    ReadEncoding;
    ReadLanguage(FLanguage);
    ReadStringEnc(FText);
  end;
end;

function TJvID3TermsOfUseFrame.SameUniqueIDAs(const Frame: TJvID3Frame): Boolean;
begin
  { There may only be one 'USER' frame in a tag}
  Result := (Assigned(Frame) and (Frame.FrameID = FrameID) and (FrameID = fiTermsOfUse)) or
    inherited SameUniqueIDAs(Frame);
end;

procedure TJvID3TermsOfUseFrame.SetLanguage(const Value: string);
begin
  if FLanguage <> Value then
  begin
    FLanguage := Value;
    Changed;
  end;
end;

procedure TJvID3TermsOfUseFrame.SetText(const Value: string);
begin
  if FText.SA <> Value then
  begin
    FText.SA := Value;
    Changed;
  end;
end;

procedure TJvID3TermsOfUseFrame.SetTextW(const Value: WideString);
begin
  if FText.SW <> Value then
  begin
    FText.SW := Value;
    Changed;
  end;
end;

function TJvID3TermsOfUseFrame.SupportsVersion(const AVersion: TJvID3Version): Boolean;
begin
  case FrameID of
    { ** Not supported in 2.2 ** }

    fiTermsOfUse:
      Result := AVersion in [ive2_3, ive2_4];
  else
    Result := True;
  end;
end;

procedure TJvID3TermsOfUseFrame.WriteFrame;
begin
  { Text encoding          $xx
    Language               $xx xx xx
    The actual text        <text string according to encoding>
  }
  with Stream do
  begin
    WriteEncoding;
    WriteLanguage(Language);
    WriteStringEnc(FText);
  end;
end;

//=== TJvID3TextFrame ========================================================

procedure TJvID3TextFrame.ChangeToVersion(const ANewVersion: TJvID3Version);
var
  Year: Word;
  LDate: TDateTime;
  Frame: TJvID3Frame;
begin
  if ANewVersion <> ive2_4 then
    Exit;

  { Change

    fiYear, fiDate, fiTime, fiRecordingDates frames into 1 fiRecordingTime frame }

  if FrameID in [fiDate, fiTime] then
  begin
    if Assigned(FFrames.FindFrame(fiRecordingTime)) then
      Exit;

    { 1. Determine the year from a fiYear frame}
    Frame := TJvID3NumberFrame.Find(FController, fiYear);
    if Assigned(Frame) then
      Year := TJvID3NumberFrame(Frame).Value
    else
      { hm, no year frame , just assume it's current year }
      Year := YearOfDate(Date);

    { 2. Determine month + day from a fiDate frame }
    Frame := TJvID3TextFrame.Find(FController, fiDate);
    if Assigned(Frame) then
      with TJvID3TextFrame(Frame) do
        LDate := GetID3Date(FText, Encoding, Year)
    else
    try
      { hm, no date frame , just assume it's 1 jan }
      LDate := EncodeDate(Year, 1, 1);
    except
      on EConvertError do
        LDate := 0;
    end;

    { 3. Determine hour + min from a fiTime frame}
    Frame := TJvID3TextFrame.Find(FController, fiTime);
    if Assigned(Frame) then
      with TJvID3TextFrame(Frame) do
        LDate := LDate + GetID3Time(FText, Encoding);

    { 4. Copy constructed date to a fiRecordingTime frame }
    TJvID3TimestampFrame.FindOrCreate(FController, fiRecordingTime).Value := LDate;
  end;
end;

function TJvID3TextFrame.CheckFrame(const HandleError: TJvID3HandleError): Boolean;
begin
  case FrameID of
    fiTime:
      Result := CheckIsID3Time(Self, FText, HandleError);
    fiDate:
      Result := CheckIsID3Date(Self, FText, HandleError);
    fiPartInSet:
      Result := CheckIsID3PartInSet(Self, FText, HandleError);
    fiTrackNum:
      Result := CheckIsID3PartInSet(Self, FText, HandleError);
  else
    Result := True;
  end;

  { If something has changed update the framesize }
  if not Result and (HandleError = heAutoCorrect) then
  begin
    UpdateFrameSize;
    Result := True;
  end;
end;

class function TJvID3TextFrame.Find(AController: TJvID3Controller;
  const AFrameID: TJvID3FrameID): TJvID3TextFrame;
var
  Frame: TJvID3Frame;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  Frame := AController.Frames.FindFrame(AFrameID);
  if Frame is TJvID3TextFrame then
    Result := TJvID3TextFrame(Frame);
end;

class function TJvID3TextFrame.FindOrCreate(AController: TJvID3Controller;
  const AFrameID: TJvID3FrameID): TJvID3TextFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController, AFrameID);
  if not Assigned(Result) then
  begin
    AController.CheckFrameClass(TJvID3TextFrame, AFrameID);
    Result := TJvID3TextFrame(AController.AddFrame(AFrameID));
  end;
end;

procedure TJvID3TextFrame.GetText(var AText: TJvID3StringPair);
begin
  AText := FText;
end;

procedure TJvID3TextFrame.NewText(const ANewText: TJvID3StringPair);
begin
  FText.SA := ANewText.SA;
  FText.SW := ANewText.SW;
end;

procedure TJvID3TextFrame.SetText(const Value: string);
begin
  if Value <> FText.SA then
  begin
    FText.SA := Value;
    Changed;
  end;
end;

procedure TJvID3TextFrame.SetTextW(const Value: WideString);
begin
  if Value <> FText.SW then
  begin
    FText.SW := Value;
    Changed;
  end;
end;

//=== TJvID3TimestampFrame ===================================================

procedure TJvID3TimestampFrame.ChangeToVersion(const ANewVersion: TJvID3Version);
var
  Year, Month, Day: Word;
  Hour, Min: Word;
  Dummy1, Dummy2: Word;
begin
  { Change

    * fiRecordingTime into fiYear, fiDate, fiTime, fiRecordingDates
    * fiOrigReleaseTime into fiOrigYear }

  if IsEmpty or not (ANewVersion in [ive2_2, ive2_3]) then
    Exit;

  if FrameID = fiRecordingTime then
  begin
    { Check if frames don't exists already }
    if [fiYear, fiDate, fiTime] * FFrames.GetFrameIDs = [] then
    begin
      { 1. Determine the Year, Month, Day, Hour and Min from this frame }
      DecodeTime(Value, Hour, Min, Dummy1, Dummy2);
      DecodeDate(Value, Year, Month, Day);

      { 2. Create a new fiYear frame for the Year }
      TJvID3NumberFrame.FindOrCreate(FController, fiYear).Value := Year;

      { 3. Create a new fiDate frame [format = 'DDMM'] for the Day and Month }
      TJvID3TextFrame.FindOrCreate(FController, fiDate).Text :=
        Format('%.2d%.2d', [Day, Month]);

      { 4. Create a new fiTime frame [format = 'HHMM'] for the Hour and Min }
      TJvID3TextFrame.FindOrCreate(FController, fiTime).Text :=
        Format('%.2d%.2d', [Hour, Min]);
    end;
  end
  else
  if FrameID = fiOrigReleaseTime then
  begin
    { Check if frames don't exists already }
    if not (fiOrigYear in FFrames.GetFrameIDs) then
    begin
      DecodeDate(Value, Year, Dummy1, Dummy2);

      { We can only store the year in a fiOrigYear frame, ie no other frames
        are supported in v2.3 }
      TJvID3NumberFrame.FindOrCreate(FController, fiOrigYear).Value := Year;
    end;
  end;
end;

function TJvID3TimestampFrame.CheckFrame(const HandleError: TJvID3HandleError): Boolean;
begin
  Result := True;
end;

class function TJvID3TimestampFrame.Find(AController: TJvID3Controller;
  const AFrameID: TJvID3FrameID): TJvID3TimestampFrame;
var
  Frame: TJvID3Frame;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  Frame := AController.Frames.FindFrame(AFrameID);
  if Frame is TJvID3TimestampFrame then
    Result := TJvID3TimestampFrame(Frame);
end;

class function TJvID3TimestampFrame.FindOrCreate(AController: TJvID3Controller;
  const AFrameID: TJvID3FrameID): TJvID3TimestampFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController, AFrameID);
  if not Assigned(Result) then
  begin
    AController.CheckFrameClass(TJvID3TimestampFrame, AFrameID);
    Result := TJvID3TimestampFrame(AController.AddFrame(AFrameID));
  end;
end;

procedure TJvID3TimestampFrame.GetText(var AText: TJvID3StringPair);
var
  Year, Month, Day, Hour, Min, Sec, Dummy: Word;
begin
  { The timestamp fields are based on a subset of ISO 8601. When being as
   precise as possible the format of a time string is
   yyyy-MM-ddTHH:mm:ss (year, "-", month, "-", day, "T", hour (out of
   24), ":", minutes, ":", seconds), but the precision may be reduced by
   removing as many time indicators as wanted. Hence valid timestamps
   are
   yyyy, yyyy-MM, yyyy-MM-dd, yyyy-MM-ddTHH, yyyy-MM-ddTHH:mm and
   yyyy-MM-ddTHH:mm:ss. All time stamps are UTC. For durations, use
   the slash character as described in 8601, and for multiple non-
   contiguous dates, use multiple strings, if allowed by the frame
   definition.
  }

  DecodeDate(Value, Year, Month, Day);
  DecodeTime(Value, Hour, Min, Sec, Dummy);
  if Year > 9999 then
    Year := 9999;
  if (Hour = 0) and (Min = 0) and (Sec = 0) then
    AText.SA := Format('%.4d-%.2d-%.2d', [Year, Month, Day])
  else
    AText.SA := Format('%.4d-%.2d-%.2dT%.2d:%.2d:%.2d', [Year, Month, Day, Hour, Min, Sec]);

  AText.SW := StringToWideStringEx(AText.SA, CP_ACP);
end;

procedure TJvID3TimestampFrame.NewText(const ANewText: TJvID3StringPair);
type
  TimeKind = (tkYear, tkMonth, tkDay, tkHour, tkMin, tkSec);
const
  {          1234567890123456789
    Format = yyyy-MM-ddTHH:mm:ss }
  SepPos: array [TimeKind] of Byte = (5, 8, 11, 14, 17, 20);
var
  S: string;
  TimeArray: array [TimeKind] of Word;
  BusyWith: TimeKind;
  I: Byte;
begin
  S := GetStringA(ANewText, Encoding);
  { Max. 19 chars }
  S := Copy(S, 1, 19);

  FillChar(TimeArray, SizeOf(TimeArray), #0);
  TimeArray[tkMonth] := 1;
  TimeArray[tkDay] := 1;

  I := 1;
  BusyWith := tkYear;
  while I <= Length(S) do
  begin
    { Use Timearray [Sec] as temp variable }

    if I = SepPos[BusyWith] then
    begin
      TimeArray[BusyWith] := TimeArray[tkSec];
      TimeArray[tkSec] := 0;
      Inc(BusyWith);
    end
    else
    if S[I] in DigitSymbols then
      TimeArray[tkSec] := TimeArray[tkSec] * 10 + Ord(S[I]) - Ord('0')
    else
      Break;

    Inc(I);
  end;

  if I = SepPos[BusyWith] then
  begin
    TimeArray[BusyWith] := TimeArray[tkSec];
    TimeArray[tkSec] := 0;
    //Inc(BusyWith);
  end;

  try
    FValue := EncodeDate(TimeArray[tkYear], TimeArray[tkMonth], TimeArray[tkDay]);
    if I > 11 then
      FValue := FValue + EncodeTime(TimeArray[tkHour], TimeArray[tkMin], TimeArray[tkSec], 0)
  except
    on EConvertError do
      FValue := 0;
  end;
end;

procedure TJvID3TimestampFrame.SetValue(const Value: TDateTime);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    Changed;
  end;
end;

//=== TJvID3URLFrame =========================================================

procedure TJvID3URLFrame.Assign(Source: TPersistent);
begin
  if Source is TJvID3URLFrame then
    FURL := TJvID3URLFrame(Source).URL;

  inherited Assign(Source);
end;

class function TJvID3URLFrame.CanAddFrame(AController: TJvID3Controller;
  AFrameID: TJvID3FrameID): Boolean;
begin
  { There may only be one URL link frame of its kind in an tag, except for

    "WCOM", but not with the same content.
    "WOAR", but not with the same content.
  }
  case AFrameID of
    fiWWWCommercialInfo, fiWWWArtist:
      Result := True;
    fiWWWCopyright, fiWWWAudioFile, fiWWWAudioSource, fiWWWRadioPage, fiWWWPayment, fiWWWPublisher:
      Result := not AController.HasFrame(AFrameID);
  else
    Result := inherited CanAddFrame(AController, AFrameID);
  end;
end;

function TJvID3URLFrame.CheckFrame(const HandleError: TJvID3HandleError): Boolean;
begin
  Result := CheckIsURL(Self, FURL, HandleError);

  { If something has changed update the framesize }
  if not Result and (HandleError = heAutoCorrect) then
  begin
    UpdateFrameSize;
    Result := True;
  end;
end;

procedure TJvID3URLFrame.Clear;
begin
  FURL := '';
  inherited Clear;
end;

class function TJvID3URLFrame.Find(AController: TJvID3Controller;
  const AFrameID: TJvID3FrameID): TJvID3URLFrame;
var
  Frame: TJvID3Frame;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  Frame := AController.Frames.FindFrame(AFrameID);
  if Frame is TJvID3URLFrame then
    Result := TJvID3URLFrame(Frame)
end;

class function TJvID3URLFrame.FindOrCreate(AController: TJvID3Controller;
  const AFrameID: TJvID3FrameID): TJvID3URLFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController, AFrameID);
  if not Assigned(Result) then
  begin
    AController.CheckFrameClass(TJvID3URLFrame, AFrameID);
    Result := TJvID3URLFrame(AController.AddFrame(AFrameID));
  end;
end;

function TJvID3URLFrame.GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal;
begin
  Result := Length(URL);
end;

function TJvID3URLFrame.GetIsEmpty: Boolean;
begin
  Result := Length(URL) = 0;
end;

procedure TJvID3URLFrame.ReadFrame;
begin
  with Stream do
    ReadStringA(FURL);
end;

function TJvID3URLFrame.SameUniqueIDAs(const Frame: TJvID3Frame): Boolean;
begin
  { There may only be one URL link frame of its kind in an tag, except for

    "WCOM", but not with the same content.
    "WOAR", but not with the same content.
  }
  Result := (Frame is TJvID3URLFrame) and (Frame.FrameID = FrameID);

  if Result then
    Result :=
      not (FrameID in [fiWWWCommercialInfo, fiWWWArtist]) or
      AnsiSameStr(URL, TJvID3URLFrame(Frame).URL)
  else
    Result := inherited SameUniqueIDAs(Frame);
end;

procedure TJvID3URLFrame.SetURL(const Value: string);
begin
  if FURL <> Value then
  begin
    FURL := Value;
    Changed;
  end;
end;

procedure TJvID3URLFrame.WriteFrame;
begin
  with Stream do
    WriteStringA(URL);
end;

//=== TJvID3URLUserFrame =====================================================

procedure TJvID3URLUserFrame.Assign(Source: TPersistent);
begin
  if Source is TJvID3URLUserFrame then
  begin
    CopyStringPair(TJvID3URLUserFrame(Source).FDescription, FDescription);
    FURL := TJvID3URLUserFrame(Source).URL;
  end;

  inherited Assign(Source);
end;

class function TJvID3URLUserFrame.CanAddFrame(AController: TJvID3Controller;
  AFrameID: TJvID3FrameID): Boolean;
begin
  { There may be more than one "WXXX" frame in each tag, but only one
    with the same description. }
  Result := (AFrameID = fiWWWUser) or inherited CanAddFrame(AController, AFrameID);
end;

function TJvID3URLUserFrame.CheckFrame(const HandleError: TJvID3HandleError): Boolean;
begin
  Result := CheckIsURL(Self, FURL, HandleError);

  { If something has changed update the framesize }
  if not Result and (HandleError = heAutoCorrect) then
  begin
    UpdateFrameSize;
    Result := True;
  end;
end;

procedure TJvID3URLUserFrame.Clear;
begin
  ClearStringPair(FDescription);
  FURL := '';
  inherited Clear;
end;

class function TJvID3URLUserFrame.Find(AController: TJvID3Controller;
  const Index: Integer): TJvID3URLUserFrame;
var
  FoundIndex: Integer;
  Frame: TJvID3Frame;
begin
  Result := nil;
  if not Assigned(AController) or not AController.Active then
    Exit;

  if not AController.FindFirstFrame(fiWWWUser, Frame) then
    Exit;

  FoundIndex := 0;

  while Assigned(Frame) and (FoundIndex < Index) do
  begin
    AController.FindNextFrame(fiWWWUser, Frame);
    Inc(FoundIndex);
  end;

  if Frame is TJvID3URLUserFrame then
    Result := TJvID3URLUserFrame(Frame);
end;

class function TJvID3URLUserFrame.FindOrCreate(AController: TJvID3Controller;
  const Index: Integer): TJvID3URLUserFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController, Index);
  if not Assigned(Result) then
    Result := TJvID3URLUserFrame(AController.AddFrame(fiWWWUser));
end;

function TJvID3URLUserFrame.GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal;
begin
  { Text encoding           $xx
    Description             <text string according to encoding> $00 (00)
    Value                   <text string according to encoding>
  }
  Result := 1 +
    LengthEnc(FDescription, Encoding, ToEncoding) +
    LengthTerminatorEnc(ToEncoding) +
    Cardinal(Length(FURL));
end;

function TJvID3URLUserFrame.GetIsEmpty: Boolean;
begin
  Result := (FURL = '') and CheckIsEmpty(FDescription, Encoding);
end;

procedure TJvID3URLUserFrame.ReadFrame;
begin
  with Stream do
  begin
    ReadEncoding;
    ReadStringEnc(FDescription);
    ReadStringA(FURL);
  end;
end;

procedure TJvID3URLUserFrame.SetDescription(const Value: string);
begin
  if FDescription.SA <> Value then
  begin
    FDescription.SA := Value;
    Changed;
  end;
end;

procedure TJvID3URLUserFrame.SetDescriptionW(const Value: WideString);
begin
  if FDescription.SW <> Value then
  begin
    FDescription.SW := Value;
    Changed;
  end;
end;

procedure TJvID3URLUserFrame.SetURL(const Value: string);
begin
  if FURL <> Value then
  begin
    FURL := Value;
    Changed;
  end;
end;

procedure TJvID3URLUserFrame.WriteFrame;
begin
  with Stream do
  begin
    WriteEncoding;
    WriteStringEnc(FDescription);
    WriteTerminatorEnc;
    WriteStringA(URL);
  end;
end;

//=== TJvID3UserFrame ========================================================

procedure TJvID3UserFrame.Assign(Source: TPersistent);
begin
  if Source is TJvID3CustomTextFrame then
  begin
    CopyStringPair(TJvID3UserFrame(Source).FValue, FValue);
    CopyStringPair(TJvID3UserFrame(Source).FDescription, FDescription);
  end;

  inherited Assign(Source);
end;

class function TJvID3UserFrame.CanAddFrame(AController: TJvID3Controller;
  AFrameID: TJvID3FrameID): Boolean;
begin
  { There may be more than one "TXXX" frame in each tag, but only one
    with the same description. }
  Result := (AFrameID = fiUserText) or
    inherited CanAddFrame(AController, AFrameID);
end;

function TJvID3UserFrame.CheckFrame(const HandleError: TJvID3HandleError): Boolean;
begin
  Result := True;
end;

procedure TJvID3UserFrame.Clear;
begin
  ClearStringPair(FValue);
  ClearStringPair(FDescription);
  inherited Clear;
end;

class function TJvID3UserFrame.Find(AController: TJvID3Controller;
  const Index: Integer): TJvID3UserFrame;
var
  FoundIndex: Integer;
  Frame: TJvID3Frame;
begin
  Result := nil;

  if not Assigned(AController) or not AController.Active then
    Exit;

  if not AController.FindFirstFrame(fiUserText, Frame) then
    Exit;

  FoundIndex := 0;

  while Assigned(Frame) and (FoundIndex < Index) do
  begin
    AController.FindNextFrame(fiUserText, Frame);
    Inc(FoundIndex);
  end;

  if Frame is TJvID3UserFrame then
    Result := TJvID3UserFrame(Frame);
end;

class function TJvID3UserFrame.FindOrCreate(AController: TJvID3Controller;
  const Index: Integer): TJvID3UserFrame;
begin
  if not Assigned(AController) then
    ID3Error(RsEID3NoController);

  Result := Find(AController, Index);
  if not Assigned(Result) then
    Result := TJvID3UserFrame(AController.AddFrame(fiUserText));
end;

function TJvID3UserFrame.GetFrameSize(const ToEncoding: TJvID3Encoding): Cardinal;
begin
  { Text encoding           $xx
    Description             <text string according to encoding> $00 (00)
    Value                   <text string according to encoding>
  }
  Result := 1 +
    LengthEnc(FDescription, Encoding, ToEncoding) +
    LengthTerminatorEnc(ToEncoding) +
    LengthEnc(FValue, Encoding, ToEncoding);
end;

function TJvID3UserFrame.GetIsEmpty: Boolean;
begin
  Result := CheckIsEmpty(FValue, Encoding) and CheckIsEmpty(FDescription, Encoding);
end;

procedure TJvID3UserFrame.ReadFrame;
begin
  with Stream do
  begin
    ReadEncoding;
    ReadUserString(FDescription, FValue);
  end;
end;

procedure TJvID3UserFrame.SetDescription(const Value: string);
begin
  if FDescription.SA <> Value then
  begin
    FDescription.SA := Value;
    Changed;
  end;
end;

procedure TJvID3UserFrame.SetDescriptionW(const Value: WideString);
begin
  if FDescription.SW <> Value then
  begin
    FDescription.SW := Value;
    Changed;
  end;
end;

procedure TJvID3UserFrame.SetValue(const Value: string);
begin
  if FValue.SA <> Value then
  begin
    FValue.SA := Value;
    Changed;
  end;
end;

procedure TJvID3UserFrame.SetValueW(const Value: WideString);
begin
  if FValue.SW <> Value then
  begin
    FValue.SW := Value;
    Changed;
  end;
end;

procedure TJvID3UserFrame.WriteFrame;
begin
  with Stream do
  begin
    WriteEncoding;
    WriteUserString(FDescription, FValue);
  end;
end;

end.
