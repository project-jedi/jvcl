{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRichEd.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvRichEd;

{.$DEFINE RICHEDIT_VER_10}

{$R-}

interface

uses Windows, {$IFDEF COMPILER3_UP} ActiveX, ComObj {$ELSE} Ole2, OleAuto {$ENDIF},
  CommCtrl, Messages, SysUtils, Classes, Controls, Forms, Graphics, StdCtrls,
  Dialogs, RichEdit, Menus, ComCtrls;

type
  TRichEditVersion = 1..3;

{$IFNDEF COMPILER3_UP}

  TCharFormat2A = record
    cbSize: UINT;
    dwMask: DWORD;
    dwEffects: DWORD;
    yHeight: Longint;
    yOffset: Longint;
    crTextColor: TColorRef;
    bCharSet: Byte;
    bPitchAndFamily: Byte;
    szFaceName: array[0..LF_FACESIZE - 1] of AnsiChar;
    { new fields in version 2.0 }
    wWeight: Word;                   { Font weight (LOGFONT value)             }
    sSpacing: Smallint;              { Amount to space between letters         }
    crBackColor: TColorRef;          { Background color                        }
    lid: LCID;                       { Locale ID                               }
    dwReserved: DWORD;               { Reserved. Must be 0                     }
    sStyle: Smallint;                { Style handle                            }
    wKerning: Word;                  { Twip size above which to kern char pair }
    bUnderlineType: Byte;            { Underline type                          }
    bAnimation: Byte;                { Animated text like marching ants        }
    bRevAuthor: Byte;                { Revision author index                   }
    bReserved1: Byte;
  end;
  TCharFormat2 = TCharFormat2A;

  TParaFormat2 = record
    cbSize: UINT;
    dwMask: DWORD;
    wNumbering: Word;
    wReserved: Word;
    dxStartIndent: Longint;
    dxRightIndent: Longint;
    dxOffset: Longint;
    wAlignment: Word;
    cTabCount: Smallint;
    rgxTabs: array [0..MAX_TAB_STOPS - 1] of Longint;
    { new fields in version 2.0 }
    dySpaceBefore: Longint;     { Vertical spacing before paragraph      }
    dySpaceAfter: Longint;      { Vertical spacing after paragraph       }
    dyLineSpacing: Longint;     { Line spacing depending on Rule         }
    sStyle: Smallint;           { Style handle                           }
    bLineSpacingRule: Byte;     { Rule for line spacing (see tom.doc)    }
    bCRC: Byte;                 { Reserved for CRC for rapid searching   }
    wShadingWeight: Word;       { Shading in hundredths of a per cent    }
    wShadingStyle: Word;        { Nibble 0: style, 1: cfpat, 2: cbpat    }
    wNumberingStart: Word;      { Starting value for numbering           }
    wNumberingStyle: Word;      { Alignment, roman/arabic, (), ), ., etc.}
    wNumberingTab: Word;        { Space bet 1st indent and 1st-line text }
    wBorderSpace: Word;         { Space between border and text (twips)  }
    wBorderWidth: Word;         { Border pen width (twips)               }
    wBorders: Word;             { Byte 0: bits specify which borders     }
                                { Nibble 2: border style, 3: color index }
  end;

{$ENDIF COMPILER3_UP}

{$IFDEF COMPILER5_UP}
  TCharFormat2 = TCharFormat2A;
{$ENDIF}

type
  TJvCustomRichEdit = class;

{ TJvTextAttributes }

  TJvAttributeType = (atDefaultText, atSelected, atWord);
  TJvConsistentAttribute = (caBold, caColor, caFace, caItalic, caSize,
    caStrikeOut, caUnderline, caProtected, caOffset, caHidden, caLink,
    caBackColor, caDisabled, caWeight, caSubscript, caRevAuthor);
  TJvConsistentAttributes = set of TJvConsistentAttribute;
  TSubscriptStyle = (ssNone, ssSubscript, ssSuperscript);
  TUnderlineType = (utNone, utSolid, utWord, utDouble, utDotted, utWave);

  TJvTextAttributes = class(TPersistent)
  private
    RichEdit: TJvCustomRichEdit;
    FType: TJvAttributeType;
    procedure AssignFont(Font: TFont);
    procedure GetAttributes(var Format: TCharFormat2);
{$IFNDEF VER90}
    function GetCharset: TFontCharset;
    procedure SetCharset(Value: TFontCharset);
{$ENDIF}
    function GetSubscriptStyle: TSubscriptStyle;
    procedure SetSubscriptStyle(Value: TSubscriptStyle);
    function GetBackColor: TColor;
    function GetColor: TColor;
    function GetConsistentAttributes: TJvConsistentAttributes;
    function GetHeight: Integer;
    function GetHidden: Boolean;
    function GetDisabled: Boolean;
    function GetLink: Boolean;
    function GetName: TFontName;
    function GetOffset: Integer;
    function GetPitch: TFontPitch;
    function GetProtected: Boolean;
    function GetRevAuthorIndex: Byte;
    function GetSize: Integer;
    function GetStyle: TFontStyles;
    function GetUnderlineType: TUnderlineType;
    procedure SetAttributes(var Format: TCharFormat2);
    procedure SetBackColor(Value: TColor);
    procedure SetColor(Value: TColor);
    procedure SetDisabled(Value: Boolean);
    procedure SetHeight(Value: Integer);
    procedure SetHidden(Value: Boolean);
    procedure SetLink(Value: Boolean);
    procedure SetName(Value: TFontName);
    procedure SetOffset(Value: Integer);
    procedure SetPitch(Value: TFontPitch);
    procedure SetProtected(Value: Boolean);
    procedure SetRevAuthorIndex(Value: Byte);
    procedure SetSize(Value: Integer);
    procedure SetStyle(Value: TFontStyles);
    procedure SetUnderlineType(Value: TUnderlineType);
  protected
    procedure InitFormat(var Format: TCharFormat2);
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TJvCustomRichEdit; AttributeType: TJvAttributeType);
    procedure Assign(Source: TPersistent); override;
{$IFNDEF VER90}
    property Charset: TFontCharset read GetCharset write SetCharset;
{$ENDIF}
    property BackColor: TColor read GetBackColor write SetBackColor;
    property Color: TColor read GetColor write SetColor;
    property ConsistentAttributes: TJvConsistentAttributes read GetConsistentAttributes;
    property Disabled: Boolean read GetDisabled write SetDisabled;
    property Hidden: Boolean read GetHidden write SetHidden;
    property Link: Boolean read GetLink write SetLink;
    property Name: TFontName read GetName write SetName;
    property Offset: Integer read GetOffset write SetOffset;
    property Pitch: TFontPitch read GetPitch write SetPitch;
    property Protected: Boolean read GetProtected write SetProtected;
    property RevAuthorIndex: Byte read GetRevAuthorIndex write SetRevAuthorIndex;
    property SubscriptStyle: TSubscriptStyle read GetSubscriptStyle write SetSubscriptStyle;
    property Size: Integer read GetSize write SetSize;
    property Style: TFontStyles read GetStyle write SetStyle;
    property Height: Integer read GetHeight write SetHeight;
    property UnderlineType: TUnderlineType read GetUnderlineType write SetUnderlineType;
  end;

{ TJvParaAttributes }

  TJvNumbering = (nsNone, nsBullet, nsArabicNumbers, nsLoCaseLetter,
    nsUpCaseLetter, nsLoCaseRoman, nsUpCaseRoman);
  TJvNumberingStyle = (nsParenthesis, nsPeriod, nsEnclosed, nsSimple);
  TParaAlignment = (paLeftJustify, paRightJustify, paCenter, paJustify);
  TLineSpacingRule = (lsSingle, lsOneAndHalf, lsDouble, lsSpecifiedOrMore,
    lsSpecified, lsMultiple);
  THeadingStyle = 0..9;
  TParaTableStyle = (tsNone, tsTableRow, tsTableCellEnd, tsTableCell);

  TJvParaAttributes = class(TPersistent)
  private
    RichEdit: TJvCustomRichEdit;
    procedure GetAttributes(var Paragraph: TParaFormat2);
    function GetAlignment: TParaAlignment;
    function GetFirstIndent: Longint;
    function GetHeadingStyle: THeadingStyle;
    function GetLeftIndent: Longint;
    function GetRightIndent: Longint;
    function GetSpaceAfter: Longint;
    function GetSpaceBefore: Longint;
    function GetLineSpacing: Longint;
    function GetLineSpacingRule: TLineSpacingRule;
    function GetNumbering: TJvNumbering;
    function GetNumberingStyle: TJvNumberingStyle;
    function GetNumberingTab: Word;
    function GetTab(Index: Byte): Longint;
    function GetTabCount: Integer;
    function GetTableStyle: TParaTableStyle;
    procedure SetAlignment(Value: TParaAlignment);
    procedure SetAttributes(var Paragraph: TParaFormat2);
    procedure SetFirstIndent(Value: Longint);
    procedure SetHeadingStyle(Value: THeadingStyle);
    procedure SetLeftIndent(Value: Longint);
    procedure SetRightIndent(Value: Longint);
    procedure SetSpaceAfter(Value: Longint);
    procedure SetSpaceBefore(Value: Longint);
    procedure SetLineSpacing(Value: Longint);
    procedure SetLineSpacingRule(Value: TLineSpacingRule);
    procedure SetNumbering(Value: TJvNumbering);
    procedure SetNumberingStyle(Value: TJvNumberingStyle);
    procedure SetNumberingTab(Value: Word);
    procedure SetTab(Index: Byte; Value: Longint);
    procedure SetTabCount(Value: Integer);
    procedure SetTableStyle(Value: TParaTableStyle);
  protected
    procedure InitPara(var Paragraph: TParaFormat2);
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TJvCustomRichEdit);
    procedure Assign(Source: TPersistent); override;
    property Alignment: TParaAlignment read GetAlignment write SetAlignment;
    property FirstIndent: Longint read GetFirstIndent write SetFirstIndent;
    property HeadingStyle: THeadingStyle read GetHeadingStyle write SetHeadingStyle;
    property LeftIndent: Longint read GetLeftIndent write SetLeftIndent;
    property LineSpacing: Longint read GetLineSpacing write SetLineSpacing;
    property LineSpacingRule: TLineSpacingRule read GetLineSpacingRule write SetLineSpacingRule;
    property Numbering: TJvNumbering read GetNumbering write SetNumbering;
    property NumberingStyle: TJvNumberingStyle read GetNumberingStyle write SetNumberingStyle;
    property NumberingTab: Word read GetNumberingTab write SetNumberingTab;
    property RightIndent: Longint read GetRightIndent write SetRightIndent;
    property SpaceAfter: Longint read GetSpaceAfter write SetSpaceAfter;
    property SpaceBefore: Longint read GetSpaceBefore write SetSpaceBefore;
    property Tab[Index: Byte]: Longint read GetTab write SetTab;
    property TabCount: Integer read GetTabCount write SetTabCount;
    property TableStyle: TParaTableStyle read GetTableStyle write SetTableStyle;
  end;

{ TJvOEMConversion }

  TJvOEMConversion = class(TConversion)
  public
    function ConvertReadStream(Stream: TStream; Buffer: PChar; BufSize: Integer): Integer; override;
    function ConvertWriteStream(Stream: TStream; Buffer: PChar; BufSize: Integer): Integer; override;
  end;

{ TJvCustomRichEdit }

  TUndoName = (unUnknown, unTyping, unDelete, unDragDrop, unCut, unPaste);
  TRichSearchType = (stWholeWord, stMatchCase, stBackward, stSetSelection);
  TRichSearchTypes = set of TRichSearchType;
  TRichSelection = (stText, stObject, stMultiChar, stMultiObject);
  TRichSelectionType = set of TRichSelection;
  TRichLangOption = (rlAutoKeyboard, rlAutoFont, rlImeCancelComplete,
    rlImeAlwaysSendNotify);
  TRichLangOptions = set of TRichLangOption;
  TRichStreamFormat = (sfDefault, sfRichText, sfPlainText);
  TRichStreamMode = (smSelection, smPlainRtf, smNoObjects, smUnicode);
  TRichStreamModes = set of TRichStreamMode;
  TRichEditURLClickEvent = procedure(Sender: TObject; const URLText: string;
    Button: TMouseButton) of object;
  TRichEditProtectChangeEx = procedure(Sender: TObject; const Message: TMessage;
    StartPos, EndPos: Integer; var AllowChange: Boolean) of object;
  TRichEditFindErrorEvent = procedure(Sender: TObject; const FindText: string) of object;
{$IFDEF COMPILER3_UP}
  TRichEditFindCloseEvent = procedure(Sender: TObject; Dialog: TFindDialog) of object;
{$ENDIF}

  PRichConversionFormat = ^TRichConversionFormat;
  TRichConversionFormat = record
    ConversionClass: TConversionClass;
    Extension: string;
    PlainText: Boolean;
    Next: PRichConversionFormat;
  end;

  TJvCustomRichEdit = class(TCustomMemo)
  private
    FHideScrollBars: Boolean;
    FSelectionBar: Boolean;
    FAutoURLDetect: Boolean;
    FWordSelection: Boolean;
    FPlainText: Boolean;
    FSelAttributes: TJvTextAttributes;
    FDefAttributes: TJvTextAttributes;
    FWordAttributes: TJvTextAttributes;
    FParagraph: TJvParaAttributes;
    FOldParaAlignment: TParaAlignment;
    FScreenLogPixels: Integer;
    FUndoLimit: Integer;
    FRichEditStrings: TStrings;
    FMemStream: TMemoryStream;
    FHideSelection: Boolean;
    FLangOptions: TRichLangOptions;
    FModified: Boolean;
    FLinesUpdating: Boolean;
    FPageRect: TRect;
    FClickRange: TCharRange;
    FClickBtn: TMouseButton;
    FFindDialog: TFindDialog;
    FReplaceDialog: TReplaceDialog;
    FLastFind: TFindDialog;
    FAllowObjects: Boolean;
    FCallback: TObject;
    FRichEditOle: IUnknown;
    FPopupVerbMenu: TPopupMenu;
    FTitle: string;
    FAutoVerbMenu: Boolean;
{$IFDEF COMPILER3_UP}
    FAllowInPlace: Boolean;
{$ENDIF}
    FDefaultJvConverter: TConversionClass;
    FOnSelChange: TNotifyEvent;
    FOnResizeRequest: TRichEditResizeEvent;
    FOnProtectChange: TRichEditProtectChange;
    FOnProtectChangeEx: TRichEditProtectChangeEx;
    FOnSaveClipboard: TRichEditSaveClipboard;
    FOnURLClick: TRichEditURLClickEvent;
    FOnTextNotFound: TRichEditFindErrorEvent;
{$IFDEF COMPILER3_UP}
    FOnCloseFindDialog: TRichEditFindCloseEvent;
{$ENDIF}
    function GetAutoURLDetect: Boolean;
    function GetWordSelection: Boolean;
    function GetLangOptions: TRichLangOptions;
    function GetCanRedo: Boolean;
    function GetCanPaste: Boolean;
{$IFNDEF COMPILER35_UP}
    function GetCanUndo: Boolean;
{$ENDIF}
    function GetRedoName: TUndoName;
    function GetUndoName: TUndoName;
    function GetStreamFormat: TRichStreamFormat;
    function GetStreamMode: TRichStreamModes;
    function GetSelectionType: TRichSelectionType;
    procedure PopupVerbClick(Sender: TObject);
    procedure ObjectPropsClick(Sender: TObject);
    procedure CloseObjects;
    procedure UpdateHostNames;
    procedure SetAllowObjects(Value: Boolean);
    procedure SetStreamFormat(Value: TRichStreamFormat);
    procedure SetStreamMode(Value: TRichStreamModes);
    procedure SetAutoURLDetect(Value: Boolean);
    procedure SetWordSelection(Value: Boolean);
    procedure SetHideScrollBars(Value: Boolean);
    procedure SetHideSelection(Value: Boolean);
    procedure SetTitle(const Value: string);
    procedure SetLangOptions(Value: TRichLangOptions);
    procedure SetRichEditStrings(Value: TStrings);
    procedure SetDefAttributes(Value: TJvTextAttributes);
    procedure SetSelAttributes(Value: TJvTextAttributes);
    procedure SetWordAttributes(Value: TJvTextAttributes);
    procedure SetSelectionBar(Value: Boolean);
    procedure SetUndoLimit(Value: Integer);
    procedure UpdateTextModes(Plain: Boolean);
    procedure AdjustFindDialogPosition(Dialog: TFindDialog);
    procedure SetupFindDialog(Dialog: TFindDialog; const SearchStr,
      ReplaceStr: string);
    function FindEditText(Dialog: TFindDialog; AdjustPos, Events: Boolean): Boolean;
    function GetCanFindNext: Boolean;
    procedure FindDialogFind(Sender: TObject);
    procedure ReplaceDialogReplace(Sender: TObject);
{$IFDEF COMPILER3_UP}
    procedure FindDialogClose(Sender: TObject);
    procedure SetUIActive(Active: Boolean);
    procedure CMDocWindowActivate(var Message: TMessage); message CM_DOCWINDOWACTIVATE;
    procedure CMUIDeactivate(var Message: TMessage); message CM_UIDEACTIVATE;
{$ENDIF}
{$IFDEF COMPILER4_UP}
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
{$ENDIF}
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure EMReplaceSel(var Message: TMessage); message EM_REPLACESEL;
    procedure WMDestroy(var Msg: TWMDestroy); message WM_DESTROY;
    procedure WMMouseMove(var Message: TMessage); message WM_MOUSEMOVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFont(var Message: TWMSetFont); message WM_SETFONT;
{$IFDEF COMPILER5_UP}
    procedure WMRButtonUp(var Message: TMessage); message WM_RBUTTONUP;
{$ENDIF}
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    function GetPopupMenu: TPopupMenu; override;
    procedure TextNotFound(Dialog: TFindDialog); virtual;
    procedure RequestSize(const Rect: TRect); virtual;
    procedure SelectionChange; dynamic;
    function ProtectChange(const Message: TMessage; StartPos,
      EndPos: Integer): Boolean; dynamic;
    function SaveClipboard(NumObj, NumChars: Integer): Boolean; dynamic;
    procedure URLClick(const URLText: string; Button: TMouseButton); dynamic;
    procedure SetPlainText(Value: Boolean); virtual;
{$IFDEF COMPILER3_UP}
    procedure CloseFindDialog(Dialog: TFindDialog); virtual;
    procedure DoSetMaxLength(Value: Integer); override;
    function GetSelLength: Integer; override;
    function GetSelStart: Integer; override;
    function GetSelText: string; override;
    procedure SetSelLength(Value: Integer); override;
    procedure SetSelStart(Value: Integer); override;
    property AllowInPlace: Boolean read FAllowInPlace write FAllowInPlace default True;
{$ENDIF}
    property AllowObjects: Boolean read FAllowObjects write SetAllowObjects default True;
    property AutoURLDetect: Boolean read GetAutoURLDetect write SetAutoURLDetect default True;
    property AutoVerbMenu: Boolean read FAutoVerbMenu write FAutoVerbMenu default True;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property HideScrollBars: Boolean read FHideScrollBars
      write SetHideScrollBars default True;
    property Title: string read FTitle write SetTitle;
    property LangOptions: TRichLangOptions read GetLangOptions write SetLangOptions default [rlAutoFont];
    property Lines: TStrings read FRichEditStrings write SetRichEditStrings;
    property PlainText: Boolean read FPlainText write SetPlainText default False;
    property SelectionBar: Boolean read FSelectionBar write SetSelectionBar default True;
    property StreamFormat: TRichStreamFormat read GetStreamFormat write SetStreamFormat default sfDefault;
    property StreamMode: TRichStreamModes read GetStreamMode write SetStreamMode default [];
    property UndoLimit: Integer read FUndoLimit write SetUndoLimit default 100;
    property WordSelection: Boolean read GetWordSelection write SetWordSelection default True;
    property ScrollBars default ssBoth;
    property TabStop default True;
    property OnSaveClipboard: TRichEditSaveClipboard read FOnSaveClipboard
      write FOnSaveClipboard;
    property OnSelectionChange: TNotifyEvent read FOnSelChange write FOnSelChange;
    property OnProtectChange: TRichEditProtectChange read FOnProtectChange
      write FOnProtectChange; { obsolete }
    property OnProtectChangeEx: TRichEditProtectChangeEx read FOnProtectChangeEx
      write FOnProtectChangeEx;
    property OnResizeRequest: TRichEditResizeEvent read FOnResizeRequest
      write FOnResizeRequest;
    property OnURLClick: TRichEditURLClickEvent read FOnURLClick write FOnURLClick;
    property OnTextNotFound: TRichEditFindErrorEvent read FOnTextNotFound write FOnTextNotFound;
{$IFDEF COMPILER3_UP}
    property OnCloseFindDialog: TRichEditFindCloseEvent read FOnCloseFindDialog
      write FOnCloseFindDialog;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; {$IFDEF COMPILER3_UP} override; {$ENDIF}
    procedure SetSelection(StartPos, EndPos: Longint; ScrollCaret: Boolean);
    function GetSelection: TCharRange;
    function GetTextRange(StartPos, EndPos: Longint): string;
    function LineFromChar(CharIndex: Integer): Integer;
    function GetLineIndex(LineNo: Integer): Integer;
    function GetLineLength(CharIndex: Integer): Integer;
    function WordAtCursor: string;
    function FindText(const SearchStr: string;
      StartPos, Length: Integer; Options: TRichSearchTypes): Integer;
    function GetSelTextBuf(Buffer: PChar; BufSize: Integer): Integer;
      {$IFDEF COMPILER3_UP} override; {$ENDIF}
    function GetCaretPos: TPoint; {$IFDEF COMPILER35_UP} override; {$ENDIF}
    function GetCharPos(CharIndex: Integer): TPoint;
    function InsertObjectDialog: Boolean;
    function ObjectPropertiesDialog: Boolean;
    function PasteSpecialDialog: Boolean;
    function FindDialog(const SearchStr: string): TFindDialog;
    function ReplaceDialog(const SearchStr, ReplaceStr: string): TReplaceDialog;
    function FindNext: Boolean;
    procedure Print(const Caption: string); virtual;
    class procedure RegisterConversionFormat(const AExtension: string;
      APlainText: Boolean; AConversionClass: TConversionClass);
    procedure ClearUndo;
    procedure Redo;
    procedure StopGroupTyping;
    property CanFindNext: Boolean read GetCanFindNext;
    property CanRedo: Boolean read GetCanRedo;
    property CanPaste: Boolean read GetCanPaste;
{$IFNDEF COMPILER35_UP}
    procedure Undo;
    property CanUndo: Boolean read GetCanUndo;
    property CaretPos: TPoint read GetCaretPos;
{$ENDIF}
    property RedoName: TUndoName read GetRedoName;
    property UndoName: TUndoName read GetUndoName;
    property DefaulTJvConverter: TConversionClass read FDefaultJvConverter
      write FDefaultJvConverter;
    property DefAttributes: TJvTextAttributes read FDefAttributes write SetDefAttributes;
    property SelAttributes: TJvTextAttributes read FSelAttributes write SetSelAttributes;
    property WordAttributes: TJvTextAttributes read FWordAttributes write SetWordAttributes;
    property PageRect: TRect read FPageRect write FPageRect;
    property Paragraph: TJvParaAttributes read FParagraph;
    property SelectionType: TRichSelectionType read GetSelectionType;
  end;

  TJvxRichEdit = class(TJvCustomRichEdit)
  published
    property Align;
    property Alignment;
    property AutoURLDetect;
    property AutoVerbMenu;
    property AllowObjects;
{$IFDEF COMPILER3_UP}
    property AllowInPlace;
{$ENDIF}
{$IFDEF COMPILER4_UP}
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property DragKind;
{$ENDIF}
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HideScrollBars;
    property Title;
{$IFNDEF VER90}
    property ImeMode;
    property ImeName;
{$ENDIF}
{$IFDEF COMPILER4_UP}
    property Constraints;
    property ParentBiDiMode;
{$ENDIF}
    property LangOptions;
    property Lines;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PlainText;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property SelectionBar;
    property ShowHint;
    property StreamFormat;
    property StreamMode;
    property TabOrder;
    property TabStop;
    property UndoLimit;
    property Visible;
    property WantTabs;
    property WantReturns;
    property WordSelection;
    property WordWrap;
    property OnChange;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
{$IFDEF COMPILER5_UP}
    property OnContextPopup;
{$ENDIF}
{$IFDEF COMPILER4_UP}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFDEF COMPILER4_UP}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
{$ENDIF}
    property OnProtectChange; { obsolete }
    property OnProtectChangeEx;
    property OnResizeRequest;
    property OnSaveClipboard;
    property OnSelectionChange;
    property OnStartDrag;
    property OnTextNotFound;
{$IFDEF COMPILER3_UP}
    property OnCloseFindDialog;
{$ENDIF}
    property OnURLClick;
  end;

var
  RichEditVersion: TRichEditVersion;

implementation

uses Printers, ComStrs, OleConst, OleDlg {$IFDEF COMPILER3_UP}, OleCtnrs {$ENDIF},
  JvMaxMin;

const
  RTFConversionFormat: TRichConversionFormat = (
    ConversionClass: TConversion;
    Extension: 'rtf';
    PlainText: False;
    Next: nil);
  TextConversionFormat: TRichConversionFormat = (
    ConversionClass: TConversion;
    Extension: 'txt';
    PlainText: True;
    Next: @RTFConversionFormat);

var
  ConversionFormatList: PRichConversionFormat = @TextConversionFormat;

const
  RichEdit10ModuleName = 'RICHED32.DLL';
  RichEdit20ModuleName = 'RICHED20.DLL';
{$IFNDEF COMPILER3_UP}
  RICHEDIT_CLASSA      = 'RichEdit20A';     { Richedit 2.0 Window Class }
  RICHEDIT_CLASSW      = 'RichEdit20W';     { Richedit 2.0 Unicode }
  RICHEDIT_CLASS10A    = 'RICHEDIT';        { Richedit 1.0 }
  RICHEDIT_CLASS       = RICHEDIT_CLASSA;
{$ENDIF}

{$IFNDEF COMPILER3_UP}

const
  EM_SETUNDOLIMIT                     = WM_USER + 82; 
  EM_REDO                             = WM_USER + 84; 
  EM_CANREDO                          = WM_USER + 85;
  EM_GETUNDONAME                      = WM_USER + 86; 
  EM_GETREDONAME                      = WM_USER + 87; 
  EM_STOPGROUPTYPING                  = WM_USER + 88; 
  EM_SETTEXTMODE                      = WM_USER + 89; 
  EM_GETTEXTMODE                      = WM_USER + 90; 

{ for use with EM_GET/SETTEXTMODE }

  TM_PLAINTEXT                       = 1; 
  TM_RICHTEXT                        = 2;     { default behavior }
  TM_SINGLELEVELUNDO                 = 4;
  TM_MULTILEVELUNDO                  = 8;     { default behavior }
  TM_SINGLECODEPAGE                  = 16; 
  TM_MULTICODEPAGE                   = 32;    { default behavior }

  EM_AUTOURLDETECT                    = WM_USER + 91; 
  EM_GETAUTOURLDETECT                 = WM_USER + 92;
  EM_SETPALETTE                       = WM_USER + 93;
  EM_GETTEXTEX                        = WM_USER + 94; 
  EM_GETTEXTLENGTHEX                  = WM_USER + 95; 

  EM_SETLANGOPTIONS                   = WM_USER + 120;
  EM_GETLANGOPTIONS                   = WM_USER + 121;
  EM_GETIMECOMPMODE                   = WM_USER + 122;

{ Options for EM_SETLANGOPTIONS and EM_GETLANGOPTIONS }

  IMF_AUTOKEYBOARD            = $0001;
  IMF_AUTOFONT                = $0002;
  IMF_IMECANCELCOMPLETE       = $0004;  { high completes the comp string when aborting, low cancels. }
  IMF_IMEALWAYSSENDNOTIFY     = $0008;

{ New notifications }

  EN_OLEOPFAILED                      = $0709;
  EN_OBJECTPOSITIONS                  = $070A;
  EN_LINK                             = $070B;
  EN_DRAGDROPDONE                     = $070C;

{ Event notification masks }

  ENM_SCROLLEVENTS                    = $00000008;
  ENM_DRAGDROPDONE                    = $00000010;
  ENM_LANGCHANGE                      = $01000000; 
  ENM_OBJECTPOSITIONS                 = $02000000; 
  ENM_LINK                            = $04000000;

{ New edit control styles }

  ES_NOOLEDRAGDROP                    = $00000008; 

const
  CFM_LINK = $00000020;  { Exchange hyperlink extension }

  CFM_EFFECTS = CFM_BOLD or CFM_ITALIC or CFM_UNDERLINE or CFM_COLOR or
    CFM_STRIKEOUT or CFE_PROTECTED or CFM_LINK;
  CFM_ALL = CFM_EFFECTS or CFM_SIZE or CFM_FACE or CFM_OFFSET or CFM_CHARSET;
  PFM_ALL = PFM_STARTINDENT or PFM_RIGHTINDENT or PFM_OFFSET or
    PFM_ALIGNMENT or PFM_TABSTOPS or PFM_NUMBERING or PFM_OFFSETINDENT;

{ New masks and effects -- a parenthesized asterisk indicates that
   the data is stored by RichEdit2.0, but not displayed }

  CFM_SMALLCAPS               = $0040;                  { (*)    }
  CFM_ALLCAPS                 = $0080;                  { (*)    }
  CFM_HIDDEN                  = $0100;                  { (*)    }
  CFM_OUTLINE                 = $0200;                  { (*)    }
  CFM_SHADOW                  = $0400;                  { (*)    }
  CFM_EMBOSS                  = $0800;                  { (*)    }
  CFM_IMPRINT                 = $1000;                  { (*)    }
  CFM_DISABLED                = $2000;
  CFM_REVISED                 = $4000;

  CFM_BACKCOLOR               = $04000000; 
  CFM_LCID                    = $02000000; 
  CFM_UNDERLINETYPE           = $00800000;              { (*)    }
  CFM_WEIGHT                  = $00400000;
  CFM_SPACING                 = $00200000;              { (*)    }
  CFM_KERNING                 = $00100000;              { (*)    }
  CFM_STYLE                   = $00080000;              { (*)    }
  CFM_ANIMATION               = $00040000;              { (*)    }
  CFM_REVAUTHOR               = $00008000; 

  CFE_LINK                    = $00000020;
  CFE_AUTOCOLOR               = $40000000;   { NOTE: this corresponds to CFM_COLOR, }
                                             { which controls it }
  CFE_SUBSCRIPT               = $00010000;   { Superscript and subscript are }
  CFE_SUPERSCRIPT             = $00020000;   { mutually exclusive            }

  CFM_SUBSCRIPT               = CFE_SUBSCRIPT or CFE_SUPERSCRIPT;
  CFM_SUPERSCRIPT             = CFM_SUBSCRIPT;

  CFM_EFFECTS2 = CFM_EFFECTS or CFM_DISABLED or CFM_SMALLCAPS or CFM_ALLCAPS or 
    CFM_HIDDEN  or CFM_OUTLINE or CFM_SHADOW or CFM_EMBOSS or 
    CFM_IMPRINT or CFM_DISABLED or CFM_REVISED or 
    CFM_SUBSCRIPT or CFM_SUPERSCRIPT or CFM_BACKCOLOR;

  CFM_ALL2 = CFM_ALL or CFM_EFFECTS2 or CFM_BACKCOLOR or CFM_LCID or 
    CFM_UNDERLINETYPE or CFM_WEIGHT or CFM_REVAUTHOR or 
    CFM_SPACING or CFM_KERNING or CFM_STYLE or CFM_ANIMATION;

  CFE_SMALLCAPS               = CFM_SMALLCAPS; 
  CFE_ALLCAPS                 = CFM_ALLCAPS; 
  CFE_HIDDEN                  = CFM_HIDDEN; 
  CFE_OUTLINE                 = CFM_OUTLINE;
  CFE_SHADOW                  = CFM_SHADOW; 
  CFE_EMBOSS                  = CFM_EMBOSS;
  CFE_IMPRINT                 = CFM_IMPRINT;
  CFE_DISABLED                = CFM_DISABLED;
  CFE_REVISED                 = CFM_REVISED;

  CFE_AUTOBACKCOLOR           = CFM_BACKCOLOR; 

{ Underline types }

  CFU_CF1UNDERLINE            = $FF;    { map charformat's bit underline to CF2. }
  CFU_INVERT                  = $FE;    { For IME composition fake a selection.  }
  CFU_UNDERLINEDOTTED         = $4;     { (*) displayed as ordinary underline    }
  CFU_UNDERLINEDOUBLE         = $3;     { (*) displayed as ordinary underline    }
  CFU_UNDERLINEWORD           = $2;     { (*) displayed as ordinary underline    }
  CFU_UNDERLINE               = $1; 
  CFU_UNDERLINENONE           = 0; 

{ PARAFORMAT 2.0 masks and effects }

const
  PFM_SPACEBEFORE                     = $00000040;
  PFM_SPACEAFTER                      = $00000080;
  PFM_LINESPACING                     = $00000100; 
  PFM_STYLE                           = $00000400;
  PFM_BORDER                          = $00000800;      { (*)    }
  PFM_SHADING                         = $00001000;      { (*)    }
  PFM_NUMBERINGSTYLE                  = $00002000;      { (*)    }
  PFM_NUMBERINGTAB                    = $00004000;      { (*)    }
  PFM_NUMBERINGSTART                  = $00008000;      { (*)    }

  PFM_RTLPARA                         = $00010000; 
  PFM_KEEP                            = $00020000;      { (*)    }
  PFM_KEEPNEXT                        = $00040000;      { (*)    }
  PFM_PAGEBREAKBEFORE                 = $00080000;      { (*)    }
  PFM_NOLINENUMBER                    = $00100000;      { (*)    }
  PFM_NOWIDOWCONTROL                  = $00200000;      { (*)    }
  PFM_DONOTHYPHEN                     = $00400000;      { (*)    }
  PFM_SIDEBYSIDE                      = $00800000;      { (*)    }

  PFM_TABLE                           = $C0000000;      { (*)    }

{ Note: PARAFORMAT has no effects }

  PFM_EFFECTS = PFM_RTLPARA or PFM_KEEP or PFM_KEEPNEXT or PFM_TABLE or
    PFM_PAGEBREAKBEFORE or PFM_NOLINENUMBER or 
    PFM_NOWIDOWCONTROL or PFM_DONOTHYPHEN or PFM_SIDEBYSIDE or PFM_TABLE; 

  PFM_ALL2 = PFM_ALL or PFM_EFFECTS or PFM_SPACEBEFORE or PFM_SPACEAFTER or 
    PFM_LINESPACING or PFM_STYLE or PFM_SHADING or PFM_BORDER or 
    PFM_NUMBERINGTAB or PFM_NUMBERINGSTART or PFM_NUMBERINGSTYLE;

  PFE_RTLPARA                         = PFM_RTLPARA              shr 16; 
  PFE_KEEP                            = PFM_KEEP                 shr 16;    { (*)    }
  PFE_KEEPNEXT                        = PFM_KEEPNEXT             shr 16;    { (*)    }
  PFE_PAGEBREAKBEFORE                 = PFM_PAGEBREAKBEFORE      shr 16;    { (*)    }
  PFE_NOLINENUMBER                    = PFM_NOLINENUMBER         shr 16;    { (*)    }
  PFE_NOWIDOWCONTROL                  = PFM_NOWIDOWCONTROL       shr 16;    { (*)    }
  PFE_DONOTHYPHEN                     = PFM_DONOTHYPHEN          shr 16;    { (*)    }
  PFE_SIDEBYSIDE                      = PFM_SIDEBYSIDE           shr 16;    { (*)    }

  PFE_TABLEROW                        = $C000;          { These 3 options are mutually   }
  PFE_TABLECELLEND                    = $8000;          {  exclusive and each imply      }
  PFE_TABLECELL                       = $4000;          {  that para is part of a table  }

  PFA_JUSTIFY                         = 4;      { New paragraph-alignment option 2.0 (*) }

const
  SF_UNICODE = $0010;  { Unicode file of some kind }

type
  TFindTextExA = record
    chrg: TCharRange;
    lpstrText: PAnsiChar;
    chrgText: TCharRange;
  end;

  TObjectPositions = packed record 
    nmhdr: TNMHdr;
    cObjectCount: Longint;
    pcpPositions: PLongint;
  end;

  TENLink = record 
    nmhdr: TNMHdr;
    msg: UINT;
    wParam: WPARAM;
    lParam: LPARAM;
    chrg: TCharRange;
  end;

  TENOleOpFailed = packed record 
    nmhdr: TNMHdr;
    iob: Longint;
    lOper: Longint;
    hr: HRESULT;
  end;

{ flags for the GETTEXTLENGTHEX data structure }

const
  GTL_DEFAULT         = 0;      { do the default (return # of chars)        }
  GTL_USECRLF         = 1;      { compute answer using CRLFs for paragraphs }
  GTL_PRECISE         = 2;      { compute a precise answer                  }
  GTL_CLOSE           = 4;      { fast computation of a "close" answer      }
  GTL_NUMCHARS        = 8;      { return the number of characters           }
  GTL_NUMBYTES        = 16;     { return the number of _bytes_              }

{ EM_GETTEXTLENGTHEX info; this struct is passed in the wparam of the msg }

type
  TGetTextLengthEx = record 
    flags: DWORD;              { flags (see GTL_XXX defines)  }
    codepage: UINT;            { code page for translation    }
  end;

const
  OLEOP_DOVERB = 1;

{$ENDIF COMPILER3_UP}

const
  FT_DOWN = 1;

type
  PENLink = ^TENLink;
  PENOleOpFailed = ^TENOleOpFailed;
  TFindTextEx = TFindTextExA;

  TTextRangeA = record
    chrg: TCharRange;
    lpstrText: PAnsiChar;
  end;
  TTextRangeW = record
    chrg: TCharRange;
    lpstrText: PWideChar;
  end;
  TTextRange = TTextRangeA;

{$IFDEF COMPILER3_UP}
function ResStr(const Ident: string): string;
begin
  Result := Ident;
end;
{$ELSE}
function ResStr(Ident: Cardinal): string;
begin
  Result := LoadStr(Ident);
end;
{$ENDIF}

{ TJvTextAttributes }

const
  AttrFlags: array[TJvAttributeType] of Word = (0, SCF_SELECTION,
    SCF_WORD or SCF_SELECTION);

constructor TJvTextAttributes.Create(AOwner: TJvCustomRichEdit;
  AttributeType: TJvAttributeType);
begin
  inherited Create;
  RichEdit := AOwner;
  FType := AttributeType;
end;

procedure TJvTextAttributes.InitFormat(var Format: TCharFormat2);
begin
  FillChar(Format, SizeOf(Format), 0);
  if RichEditVersion >= 2 then Format.cbSize := SizeOf(Format)
  else Format.cbSize := SizeOf(TCharFormat);
end;

function TJvTextAttributes.GetConsistentAttributes: TJvConsistentAttributes;
var
  Format: TCharFormat2;
begin
  Result := [];
  if RichEdit.HandleAllocated and (FType <> atDefaultText) then begin
    InitFormat(Format);
    SendMessage(RichEdit.Handle, EM_GETCHARFORMAT,
      AttrFlags[FType], LPARAM(@Format));
    with Format do begin
      if (dwMask and CFM_BOLD) <> 0 then Include(Result, caBold);
      if (dwMask and CFM_COLOR) <> 0 then Include(Result, caColor);
      if (dwMask and CFM_FACE) <> 0 then Include(Result, caFace);
      if (dwMask and CFM_ITALIC) <> 0 then Include(Result, caItalic);
      if (dwMask and CFM_SIZE) <> 0 then Include(Result, caSize);
      if (dwMask and CFM_STRIKEOUT) <> 0 then Include(Result, caStrikeOut);
      if (dwMask and CFM_UNDERLINE) <> 0 then Include(Result, caUnderline);
      if (dwMask and CFM_PROTECTED) <> 0 then Include(Result, caProtected);
      if (dwMask and CFM_OFFSET) <> 0 then Include(Result, caOffset);
      if (dwMask and CFM_HIDDEN) <> 0 then Include(result, caHidden);
      if RichEditVersion >= 2 then begin
        if (dwMask and CFM_LINK) <> 0 then Include(Result, caLink);
        if (dwMask and CFM_BACKCOLOR) <> 0 then Include(Result, caBackColor);
        if (dwMask and CFM_DISABLED) <> 0 then Include(Result, caDisabled);
        if (dwMask and CFM_WEIGHT) <> 0 then Include(Result, caWeight);
        if (dwMask and CFM_SUBSCRIPT) <> 0 then Include(Result, caSubscript);
        if (dwMask and CFM_REVAUTHOR) <> 0 then Include(Result, caRevAuthor);
      end;
    end;
  end;
end;

procedure TJvTextAttributes.GetAttributes(var Format: TCharFormat2);
begin
  InitFormat(Format);
  if RichEdit.HandleAllocated then
    SendMessage(RichEdit.Handle, EM_GETCHARFORMAT, AttrFlags[FType],
      LPARAM(@Format));
end;

procedure TJvTextAttributes.SetAttributes(var Format: TCharFormat2);
begin
  if RichEdit.HandleAllocated then
    SendMessage(RichEdit.Handle, EM_SETCHARFORMAT, AttrFlags[FType],
      LPARAM(@Format));
end;

{$IFNDEF VER90}
function TJvTextAttributes.GetCharset: TFontCharset;
var
  Format: TCharFormat2;
begin
  GetAttributes(Format);
  Result := Format.bCharset;
end;

procedure TJvTextAttributes.SetCharset(Value: TFontCharset);
var
  Format: TCharFormat2;
begin
  InitFormat(Format);
  with Format do
  begin
    dwMask := CFM_CHARSET;
    bCharSet := Value;
  end;
  SetAttributes(Format);
end;
{$ENDIF}

function TJvTextAttributes.GetProtected: Boolean;
var
  Format: TCharFormat2;
begin
  GetAttributes(Format);
  with Format do
    Result := (dwEffects and CFE_PROTECTED) <> 0;
end;

procedure TJvTextAttributes.SetProtected(Value: Boolean);
var
  Format: TCharFormat2;
begin
  InitFormat(Format);
  with Format do begin
    dwMask := CFM_PROTECTED;
    if Value then dwEffects := CFE_PROTECTED;
  end;
  SetAttributes(Format);
end;

function TJvTextAttributes.GetLink: Boolean;
var
  Format: TCharFormat2;
begin
  Result := False;
  if RichEditVersion < 2 then Exit;
  GetAttributes(Format);
  with Format do Result := (dwEffects and CFE_LINK) <> 0;
end;

procedure TJvTextAttributes.SetLink(Value: Boolean);
var
  Format: TCharFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitFormat(Format);
  with Format do begin
    dwMask := CFM_LINK;
    if Value then dwEffects := CFE_LINK;
  end;
  SetAttributes(Format);
end;

function TJvTextAttributes.GetRevAuthorIndex: Byte;
var
  Format: TCharFormat2;
begin
  GetAttributes(Format);
  Result := Format.bRevAuthor;
end;

procedure TJvTextAttributes.SetRevAuthorIndex(Value: Byte);
var
  Format: TCharFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitFormat(Format);
  with Format do begin
    dwMask := CFM_REVAUTHOR;
    bRevAuthor := Value;
  end;
  SetAttributes(Format);
end;

function TJvTextAttributes.GetHidden: Boolean;
var
  Format: TCharFormat2;
begin
  Result := False;
  if RichEditVersion < 2 then Exit;
  GetAttributes(Format);
  Result := Format.dwEffects and CFE_HIDDEN <> 0;
end;

procedure TJvTextAttributes.SetHidden(Value: Boolean);
var
  Format: TCharFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitFormat(Format);
  with Format do begin
    dwMask := CFM_HIDDEN;
    if Value then dwEffects := CFE_HIDDEN;
  end;
  SetAttributes(Format);
end;

function TJvTextAttributes.GetDisabled: Boolean;
var
  Format: TCharFormat2;
begin
  Result := False;
  if RichEditVersion < 2 then Exit;
  GetAttributes(Format);
  Result := Format.dwEffects and CFE_DISABLED <> 0;
end;

procedure TJvTextAttributes.SetDisabled(Value: Boolean);
var
  Format: TCharFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitFormat(Format);
  with Format do begin
    dwMask := CFM_DISABLED;
    if Value then dwEffects := CFE_DISABLED;
  end;
  SetAttributes(Format);
end;

function TJvTextAttributes.GetColor: TColor;
var
  Format: TCharFormat2;
begin
  GetAttributes(Format);
  with Format do
    if (dwEffects and CFE_AUTOCOLOR) <> 0 then Result := clWindowText
    else Result := crTextColor;
end;

procedure TJvTextAttributes.SetColor(Value: TColor);
var
  Format: TCharFormat2;
begin
  InitFormat(Format);
  with Format do begin
    dwMask := CFM_COLOR;
    if (Value = clWindowText) or (Value = clDefault) then
      dwEffects := CFE_AUTOCOLOR
    else crTextColor := ColorToRGB(Value);
  end;
  SetAttributes(Format);
end;

function TJvTextAttributes.GetBackColor: TColor;
var
  Format: TCharFormat2;
begin
  if RichEditVersion < 2 then begin
    Result := clWindow;
    Exit;
  end;
  GetAttributes(Format);
  with Format do
    if (dwEffects and CFE_AUTOBACKCOLOR) <> 0 then Result := clWindow
    else Result := crBackColor;
end;

procedure TJvTextAttributes.SetBackColor(Value: TColor);
var
  Format: TCharFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitFormat(Format);
  with Format do begin
    dwMask := CFM_BACKCOLOR;
    if (Value = clWindow) or (Value = clDefault) then
      dwEffects := CFE_AUTOBACKCOLOR
    else crBackColor := ColorToRGB(Value);
  end;
  SetAttributes(Format);
end;

function TJvTextAttributes.GetName: TFontName;
var
  Format: TCharFormat2;
begin
  GetAttributes(Format);
  Result := Format.szFaceName;
end;

procedure TJvTextAttributes.SetName(Value: TFontName);
var
  Format: TCharFormat2;
begin
  InitFormat(Format);
  with Format do begin
    dwMask := CFM_FACE;
    StrPLCopy(szFaceName, Value, SizeOf(szFaceName));
  end;
  SetAttributes(Format);
end;

function TJvTextAttributes.GetStyle: TFontStyles;
var
  Format: TCharFormat2;
begin
  Result := [];
  GetAttributes(Format);
  with Format do begin
    if (dwEffects and CFE_BOLD) <> 0 then Include(Result, fsBold);
    if (dwEffects and CFE_ITALIC) <> 0 then Include(Result, fsItalic);
    if (dwEffects and CFE_UNDERLINE) <> 0 then Include(Result, fsUnderline);
    if (dwEffects and CFE_STRIKEOUT) <> 0 then Include(Result, fsStrikeOut);
  end;
end;

procedure TJvTextAttributes.SetStyle(Value: TFontStyles);
var
  Format: TCharFormat2;
begin
  InitFormat(Format);
  with Format do begin
    dwMask := CFM_BOLD or CFM_ITALIC or CFM_UNDERLINE or CFM_STRIKEOUT;
    if fsBold in Value then dwEffects := dwEffects or CFE_BOLD;
    if fsItalic in Value then dwEffects := dwEffects or CFE_ITALIC;
    if fsUnderline in Value then dwEffects := dwEffects or CFE_UNDERLINE;
    if fsStrikeOut in Value then dwEffects := dwEffects or CFE_STRIKEOUT;
  end;
  SetAttributes(Format);
end;

function TJvTextAttributes.GetUnderlineType: TUnderlineType;
var
  Format: TCharFormat2;
begin
  Result := utNone;
  if RichEditVersion < 2 then Exit;
  GetAttributes(Format);
  with Format do begin
    if (dwEffects and CFE_UNDERLINE <> 0) and
      (dwMask and CFM_UNDERLINETYPE = CFM_UNDERLINETYPE) then
      Result := TUnderlineType(bUnderlineType);
  end;
end;

procedure TJvTextAttributes.SetUnderlineType(Value: TUnderlineType);
var
  Format: TCharFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitFormat(Format);
  with Format do begin
    dwMask := CFM_UNDERLINETYPE or CFM_UNDERLINE;
    bUnderlineType := Ord(Value);
    if Value <> utNone then dwEffects := dwEffects or CFE_UNDERLINE;
  end;
  SetAttributes(Format);
end;

function TJvTextAttributes.GetOffset: Integer;
var
  Format: TCharFormat2;
begin
  GetAttributes(Format);
  Result := Format.yOffset div 20;
end;

procedure TJvTextAttributes.SetOffset(Value: Integer);
var
  Format: TCharFormat2;
begin
  InitFormat(Format);
  with Format do begin
    dwMask := DWORD(CFM_OFFSET);
    yOffset := Value * 20;
  end;
  SetAttributes(Format);
end;

function TJvTextAttributes.GetSize: Integer;
var
  Format: TCharFormat2;
begin
  GetAttributes(Format);
  Result := Format.yHeight div 20;
end;

procedure TJvTextAttributes.SetSize(Value: Integer);
var
  Format: TCharFormat2;
begin
  InitFormat(Format);
  with Format do begin
    dwMask := DWORD(CFM_SIZE);
    yHeight := Value * 20;
  end;
  SetAttributes(Format);
end;

function TJvTextAttributes.GetHeight: Integer;
begin
  Result := MulDiv(Size, RichEdit.FScreenLogPixels, 72);
end;

procedure TJvTextAttributes.SetHeight(Value: Integer);
begin
  Size := MulDiv(Value, 72, RichEdit.FScreenLogPixels);
end;

function TJvTextAttributes.GetPitch: TFontPitch;
var
  Format: TCharFormat2;
begin
  GetAttributes(Format);
  case (Format.bPitchAndFamily and $03) of
    DEFAULT_PITCH: Result := fpDefault;
    VARIABLE_PITCH: Result := fpVariable;
    FIXED_PITCH: Result := fpFixed;
    else Result := fpDefault;
  end;
end;

procedure TJvTextAttributes.SetPitch(Value: TFontPitch);
var
  Format: TCharFormat2;
begin
  InitFormat(Format);
  with Format do begin
    case Value of
      fpVariable: bPitchAndFamily := VARIABLE_PITCH;
      fpFixed: bPitchAndFamily := FIXED_PITCH;
      else bPitchAndFamily := DEFAULT_PITCH;
    end;
  end;
  SetAttributes(Format);
end;

function TJvTextAttributes.GetSubscriptStyle: TSubscriptStyle;
var
  Format: TCharFormat2;
begin
  Result := ssNone;
  if RichEditVersion < 2 then Exit;
  GetAttributes(Format);
  with Format do begin
    if (dwEffects and CFE_SUBSCRIPT) <> 0 then
      Result := ssSubscript
    else if (dwEffects and CFE_SUPERSCRIPT) <> 0 then
      Result := ssSuperscript;
  end;
end;

procedure TJvTextAttributes.SetSubscriptStyle(Value: TSubscriptStyle);
var
  Format: TCharFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitFormat(Format);
  with Format do begin
    dwMask := DWORD(CFM_SUBSCRIPT);
    case Value of
      ssSubscript: dwEffects := CFE_SUBSCRIPT;
      ssSuperscript: dwEffects := CFE_SUPERSCRIPT;
    end;
  end;
  SetAttributes(Format);
end;

procedure TJvTextAttributes.AssignFont(Font: TFont);
var
  LogFont: TLogFont;
  Format: TCharFormat2;
begin
  InitFormat(Format);
  with Format do begin
    case Font.Pitch of
      fpVariable: bPitchAndFamily := VARIABLE_PITCH;
      fpFixed: bPitchAndFamily := FIXED_PITCH;
      else bPitchAndFamily := DEFAULT_PITCH;
    end;
    dwMask := dwMask or CFM_SIZE or CFM_BOLD or CFM_ITALIC or
      CFM_UNDERLINE or CFM_STRIKEOUT or CFM_FACE or CFM_COLOR;
    yHeight := Font.Size * 20;
    if fsBold in Font.Style then dwEffects := dwEffects or CFE_BOLD;
    if fsItalic in Font.Style then dwEffects := dwEffects or CFE_ITALIC;
    if fsUnderline in Font.Style then dwEffects := dwEffects or CFE_UNDERLINE;
    if fsStrikeOut in Font.Style then dwEffects := dwEffects or CFE_STRIKEOUT;
    StrPLCopy(szFaceName, Font.Name, SizeOf(szFaceName));
    if (Font.Color = clWindowText) or (Font.Color = clDefault) then
      dwEffects := CFE_AUTOCOLOR
    else crTextColor := ColorToRGB(Font.Color);
{$IFNDEF VER90}
    dwMask := dwMask or CFM_CHARSET;
    bCharSet := Font.Charset;
{$ENDIF}
    if GetObject(Font.Handle, SizeOf(LogFont), @LogFont) <> 0 then begin
      dwMask := dwMask or DWORD(CFM_WEIGHT);
      wWeight := Word(LogFont.lfWeight);
    end;
  end;
  SetAttributes(Format);
end;

procedure TJvTextAttributes.Assign(Source: TPersistent);
var
  Format: TCharFormat2;
begin
  if Source is TFont then AssignFont(TFont(Source))
  else if Source is TTextAttributes then begin
    Name := TTextAttributes(Source).Name;
{$IFDEF COMPILER3_UP}
    Charset := TTextAttributes(Source).Charset;
{$ENDIF}
    Style := TTextAttributes(Source).Style;
    Pitch := TTextAttributes(Source).Pitch;
    Color := TTextAttributes(Source).Color;
  end
  else if Source is TJvTextAttributes then begin
    TJvTextAttributes(Source).GetAttributes(Format);
    SetAttributes(Format);
  end
  else inherited Assign(Source);
end;

procedure TJvTextAttributes.AssignTo(Dest: TPersistent);
begin
  if Dest is TFont then begin
    TFont(Dest).Color := Color;
    TFont(Dest).Name := Name;
{$IFNDEF VER90}
    TFont(Dest).Charset := Charset;
{$ENDIF}
    TFont(Dest).Style := Style;
    TFont(Dest).Size := Size;
    TFont(Dest).Pitch := Pitch;
  end
  else if Dest is TTextAttributes then begin
    TTextAttributes(Dest).Color := Color;
    TTextAttributes(Dest).Name := Name;
{$IFDEF COMPILER3_UP}
    TTextAttributes(Dest).Charset := Charset;
{$ENDIF}
    TTextAttributes(Dest).Style := Style;
    TTextAttributes(Dest).Pitch := Pitch;
  end
  else inherited AssignTo(Dest);
end;

{ TJvParaAttributes }

constructor TJvParaAttributes.Create(AOwner: TJvCustomRichEdit);
begin
  inherited Create;
  RichEdit := AOwner;
end;

procedure TJvParaAttributes.InitPara(var Paragraph: TParaFormat2);
begin
  FillChar(Paragraph, SizeOf(Paragraph), 0);
  if RichEditVersion >= 2 then
    Paragraph.cbSize := SizeOf(Paragraph)
  else
    Paragraph.cbSize := SizeOf(TParaFormat);
end;

procedure TJvParaAttributes.GetAttributes(var Paragraph: TParaFormat2);
begin
  InitPara(Paragraph);
  if RichEdit.HandleAllocated then
    SendMessage(RichEdit.Handle, EM_GETPARAFORMAT, 0, LPARAM(@Paragraph));
end;

procedure TJvParaAttributes.SetAttributes(var Paragraph: TParaFormat2);
begin
{$IFDEF COMPILER4_UP}
  RichEdit.HandleNeeded; { we REALLY need the handle for BiDi }
{$ENDIF}
  if RichEdit.HandleAllocated then begin
{$IFDEF COMPILER4_UP}
    if RichEdit.UseRightToLeftAlignment then
      if Paragraph.wAlignment = PFA_LEFT then
        Paragraph.wAlignment := PFA_RIGHT
      else if Paragraph.wAlignment = PFA_RIGHT then
        Paragraph.wAlignment := PFA_LEFT;
{$ENDIF}
    SendMessage(RichEdit.Handle, EM_SETPARAFORMAT, 0, LPARAM(@Paragraph));
  end;
end;

function TJvParaAttributes.GetAlignment: TParaAlignment;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := TParaAlignment(Paragraph.wAlignment - 1);
end;

procedure TJvParaAttributes.SetAlignment(Value: TParaAlignment);
var
  Paragraph: TParaFormat2;
begin
  InitPara(Paragraph);
  with Paragraph do
  begin
    dwMask := PFM_ALIGNMENT;
    wAlignment := Ord(Value) + 1;
  end;
  SetAttributes(Paragraph);
end;

function TJvParaAttributes.GetNumbering: TJvNumbering;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := TJvNumbering(Paragraph.wNumbering);
  if RichEditVersion = 1 then
    if Result <> nsNone then Result := nsBullet;
end;

procedure TJvParaAttributes.SetNumbering(Value: TJvNumbering);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion = 1 then
    if Value <> nsNone then Value := TJvNumbering(PFN_BULLET);
  case Value of
    nsNone: LeftIndent := 0;
    else if LeftIndent < 10 then LeftIndent := 10;
  end;
  InitPara(Paragraph);
  with Paragraph do begin
    dwMask := PFM_NUMBERING;
    wNumbering := Ord(Value);
  end;
  SetAttributes(Paragraph);
end;

function TJvParaAttributes.GetNumberingStyle: TJvNumberingStyle;
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 2 then
    Result := nsSimple
  else begin
    GetAttributes(Paragraph);
    Result := TJvNumberingStyle(Paragraph.wNumberingStyle);
  end;
end;

procedure TJvParaAttributes.SetNumberingStyle(Value: TJvNumberingStyle);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitPara(Paragraph);
  with Paragraph do begin
    dwMask := PFM_NUMBERINGSTYLE;
    wNumberingStyle := Ord(Value);
  end;
  SetAttributes(Paragraph);
end;

function TJvParaAttributes.GetNumberingTab: Word;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.wNumberingTab div 20;
end;

procedure TJvParaAttributes.SetNumberingTab(Value: Word);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitPara(Paragraph);
  with Paragraph do begin
    dwMask := PFM_NUMBERINGTAB;
    wNumberingTab := Value * 20;
  end;
  SetAttributes(Paragraph);
end;

function TJvParaAttributes.GetFirstIndent: Longint;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.dxStartIndent div 20;
end;

procedure TJvParaAttributes.SetFirstIndent(Value: Longint);
var
  Paragraph: TParaFormat2;
begin
  InitPara(Paragraph);
  with Paragraph do
  begin
    dwMask := PFM_STARTINDENT;
    dxStartIndent := Value * 20;
  end;
  SetAttributes(Paragraph);
end;

function TJvParaAttributes.GetHeadingStyle: THeadingStyle;
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 3 then Result := 0
  else begin
    GetAttributes(Paragraph);
    Result := Paragraph.sStyle;
  end;
end;

procedure TJvParaAttributes.SetHeadingStyle(Value: THeadingStyle);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 3 then Exit;
  InitPara(Paragraph);
  with Paragraph do begin
    dwMask := PFM_STYLE;
    sStyle := Value;
  end;
  SetAttributes(Paragraph);
end;

function TJvParaAttributes.GetLeftIndent: Longint;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.dxOffset div 20;
end;

procedure TJvParaAttributes.SetLeftIndent(Value: Longint);
var
  Paragraph: TParaFormat2;
begin
  InitPara(Paragraph);
  with Paragraph do
  begin
    dwMask := PFM_OFFSET;
    dxOffset := Value * 20;
  end;
  SetAttributes(Paragraph);
end;

function TJvParaAttributes.GetRightIndent: Longint;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.dxRightIndent div 20;
end;

procedure TJvParaAttributes.SetRightIndent(Value: Longint);
var
  Paragraph: TParaFormat2;
begin
  InitPara(Paragraph);
  with Paragraph do
  begin
    dwMask := PFM_RIGHTINDENT;
    dxRightIndent := Value * 20;
  end;
  SetAttributes(Paragraph);
end;

function TJvParaAttributes.GetSpaceAfter: Longint;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.dySpaceAfter div 20;
end;

procedure TJvParaAttributes.SetSpaceAfter(Value: Longint);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitPara(Paragraph);
  with Paragraph do begin
    dwMask := PFM_SPACEAFTER;
    dySpaceAfter := Value * 20;
  end;
  SetAttributes(Paragraph);
end;

function TJvParaAttributes.GetSpaceBefore: Longint;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.dySpaceBefore div 20;
end;

procedure TJvParaAttributes.SetSpaceBefore(Value: Longint);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitPara(Paragraph);
  with Paragraph do begin
    dwMask := PFM_SPACEBEFORE;
    dySpaceBefore := Value * 20;
  end;
  SetAttributes(Paragraph);
end;

function TJvParaAttributes.GetLineSpacing: Longint;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.dyLineSpacing div 20;
end;

procedure TJvParaAttributes.SetLineSpacing(Value: Longint);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 2 then Exit;
  GetAttributes(Paragraph);
  with Paragraph do begin
    dwMask := PFM_LINESPACING;
    dyLineSpacing := Value * 20;
  end;
  SetAttributes(Paragraph);
end;

function TJvParaAttributes.GetLineSpacingRule: TLineSpacingRule;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := TLineSpacingRule(Paragraph.bLineSpacingRule);
end;

procedure TJvParaAttributes.SetLineSpacingRule(Value: TLineSpacingRule);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 2 then Exit;
  GetAttributes(Paragraph);
  with Paragraph do begin
    dwMask := PFM_LINESPACING;
    bLineSpacingRule := Ord(Value);
  end;
  SetAttributes(Paragraph);
end;

function TJvParaAttributes.GetTab(Index: Byte): Longint;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.rgxTabs[Index] div 20;
end;

procedure TJvParaAttributes.SetTab(Index: Byte; Value: Longint);
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  with Paragraph do
  begin
    rgxTabs[Index] := Value * 20;
    dwMask := PFM_TABSTOPS;
    if cTabCount < Index then cTabCount := Index;
    SetAttributes(Paragraph);
  end;
end;

function TJvParaAttributes.GetTabCount: Integer;
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  Result := Paragraph.cTabCount;
end;

procedure TJvParaAttributes.SetTabCount(Value: Integer);
var
  Paragraph: TParaFormat2;
begin
  GetAttributes(Paragraph);
  with Paragraph do
  begin
    dwMask := PFM_TABSTOPS;
    cTabCount := Value;
    SetAttributes(Paragraph);
  end;
end;

function TJvParaAttributes.GetTableStyle: TParaTableStyle;
var
  Paragraph: TParaFormat2;
begin
  Result := tsNone;
  if RichEditVersion < 2 then Exit;
  GetAttributes(Paragraph);
  with Paragraph do begin
    if (wReserved and PFE_TABLEROW) <> 0 then
      Result := tsTableRow
    else if (wReserved and PFE_TABLECELLEND) <> 0 then
      Result := tsTableCellEnd
    else if (wReserved and PFE_TABLECELL) <> 0 then
      Result := tsTableCell;
  end;
end;

procedure TJvParaAttributes.SetTableStyle(Value: TParaTableStyle);
var
  Paragraph: TParaFormat2;
begin
  if RichEditVersion < 2 then Exit;
  InitPara(Paragraph);
  with Paragraph do begin
    dwMask := PFM_TABLE;
    case Value of
      tsTableRow: wReserved := PFE_TABLEROW;
      tsTableCellEnd: wReserved := PFE_TABLECELLEND;
      tsTableCell: wReserved := PFE_TABLECELL;
    end;
  end;
  SetAttributes(Paragraph);
end;

procedure TJvParaAttributes.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TParaAttributes then begin
    with TParaAttributes(Dest) do begin
      if Self.Alignment = paJustify then Alignment := taLeftJustify
      else Alignment := TAlignment(Self.Alignment);
      FirstIndent := Self.FirstIndent;
      LeftIndent := Self.LeftIndent;
      RightIndent := Self.RightIndent;
      if Self.Numbering <> nsNone then
        Numbering := TNumberingStyle(nsBullet)
      else Numbering := TNumberingStyle(nsNone);
      for I := 0 to MAX_TAB_STOPS - 1 do
        Tab[I] := Self.Tab[I];
    end;
  end
  else inherited AssignTo(Dest);
end;

procedure TJvParaAttributes.Assign(Source: TPersistent);
var
  I: Integer;
  Paragraph: TParaFormat2;
begin
  if Source is TParaAttributes then begin
    Alignment := TParaAlignment(TParaAttributes(Source).Alignment);
    FirstIndent := TParaAttributes(Source).FirstIndent;
    LeftIndent := TParaAttributes(Source).LeftIndent;
    RightIndent := TParaAttributes(Source).RightIndent;
    Numbering := TJvNumbering(TParaAttributes(Source).Numbering);
    for I := 0 to MAX_TAB_STOPS - 1 do
      Tab[I] := TParaAttributes(Source).Tab[I];
  end
  else if Source is TJvParaAttributes then begin
    TJvParaAttributes(Source).GetAttributes(Paragraph);
    SetAttributes(Paragraph);
  end
  else inherited Assign(Source);
end;

{ OLE utility routines }

function WStrLen(Str: PWideChar): Integer;
begin
  Result := 0;
  while Str[Result] <> #0 do Inc(Result);
end;

procedure ReleaseObject(var Obj);
begin
  if IUnknown(Obj) <> nil then begin
{$IFNDEF COMPILER3_UP}
    IUnknown(Obj).Release;
{$ENDIF}
    IUnknown(Obj) := nil;
  end;
end;

procedure CreateStorage(var Storage: IStorage);
var
  LockBytes: ILockBytes;
begin
  OleCheck(CreateILockBytesOnHGlobal(0, True, LockBytes));
  try
    OleCheck(StgCreateDocfileOnILockBytes(LockBytes, STGM_READWRITE
      or STGM_SHARE_EXCLUSIVE or STGM_CREATE, 0, Storage));
  finally
    ReleaseObject(LockBytes);
  end;
end;

procedure DestroyMetaPict(MetaPict: HGlobal);
begin
  if MetaPict <> 0 then begin
    DeleteMetaFile(PMetaFilePict(GlobalLock(MetaPict))^.hMF);
    GlobalUnlock(MetaPict);
    GlobalFree(MetaPict);
  end;
end;

function OleSetDrawAspect(OleObject: IOleObject; Iconic: Boolean;
  IconMetaPict: HGlobal; var DrawAspect: Longint): HResult;
var
  OleCache: IOleCache;
  EnumStatData: IEnumStatData;
  OldAspect, AdviseFlags, Connection: Longint;
  TempMetaPict: HGlobal;
  FormatEtc: TFormatEtc;
  Medium: TStgMedium;
  ClassID: TCLSID;
  StatData: TStatData;
begin
  Result := S_OK;
  OldAspect := DrawAspect;
  if Iconic then begin
    DrawAspect := DVASPECT_ICON;
    AdviseFlags := ADVF_NODATA;
  end
  else begin
    DrawAspect := DVASPECT_CONTENT;
    AdviseFlags := ADVF_PRIMEFIRST;
  end;
  if (DrawAspect <> OldAspect) or (DrawAspect = DVASPECT_ICON) then begin
{$IFDEF COMPILER3_UP}
    Result := OleObject.QueryInterface(IOleCache, OleCache);
{$ELSE}
    Result := OleObject.QueryInterface(IID_IOleCache, OleCache);
{$ENDIF}
    if Succeeded(Result) then
    try
      if DrawAspect <> OldAspect then begin
        { Setup new cache with the new aspect }
        FillChar(FormatEtc, SizeOf(FormatEtc), 0);
        FormatEtc.dwAspect := DrawAspect;
        FormatEtc.lIndex := -1;
        Result := OleCache.Cache(FormatEtc, AdviseFlags, Connection);
      end;
      if Succeeded(Result) and (DrawAspect = DVASPECT_ICON) then begin
        TempMetaPict := 0;
        if IconMetaPict = 0 then begin
          if Succeeded(OleObject.GetUserClassID(ClassID)) then begin
            TempMetaPict := OleGetIconOfClass(ClassID, nil, True);
            IconMetaPict := TempMetaPict;
          end;
        end;
        try
          FormatEtc.cfFormat := CF_METAFILEPICT;
          FormatEtc.ptd := nil;
          FormatEtc.dwAspect := DVASPECT_ICON;
          FormatEtc.lIndex := -1;
          FormatEtc.tymed := TYMED_MFPICT;
          Medium.tymed := TYMED_MFPICT;
          Medium.hMetaFilePict := IconMetaPict;
          Medium.unkForRelease := nil;
          Result := OleCache.SetData(FormatEtc, Medium, False);
        finally
          DestroyMetaPict(TempMetaPict);
        end;
      end;
      if Succeeded(Result) and (DrawAspect <> OldAspect) then begin
        { remove any existing caches that are set up for the old display aspect }
        OleCache.EnumCache(EnumStatData);
        if EnumStatData <> nil then
        try
          while EnumStatData.Next(1, StatData, nil) = 0 do
            if StatData.formatetc.dwAspect = OldAspect then
              OleCache.Uncache(StatData.dwConnection);
        finally
          ReleaseObject(EnumStatData);
        end;
      end;
    finally
      ReleaseObject(OleCache);
    end;
    if Succeeded(Result) and (DrawAspect <> DVASPECT_ICON) then
      OleObject.Update;
  end;
end;

function GetIconMetaPict(OleObject: IOleObject; DrawAspect: Longint): HGlobal;
var
  DataObject: IDataObject;
  FormatEtc: TFormatEtc;
  Medium: TStgMedium;
  ClassID: TCLSID;
begin
  Result := 0;
  if DrawAspect = DVASPECT_ICON then begin
{$IFDEF COMPILER3_UP}
    OleObject.QueryInterface(IDataObject, DataObject);
{$ELSE}
    OleObject.QueryInterface(IID_IDataObject, DataObject);
{$ENDIF}
    if DataObject <> nil then begin
      FormatEtc.cfFormat := CF_METAFILEPICT;
      FormatEtc.ptd := nil;
      FormatEtc.dwAspect := DVASPECT_ICON;
      FormatEtc.lIndex := -1;
      FormatEtc.tymed := TYMED_MFPICT;
      if Succeeded(DataObject.GetData(FormatEtc, Medium)) then
        Result := Medium.hMetaFilePict;
      ReleaseObject(DataObject);
    end;
  end;
  if Result = 0 then begin
    OleCheck(OleObject.GetUserClassID(ClassID));
    Result := OleGetIconOfClass(ClassID, nil, True);
  end;
end;

{ Return the first piece of a moniker }

function OleStdGetFirstMoniker(Moniker: IMoniker): IMoniker;
var
  Mksys: Longint;
  EnumMoniker: IEnumMoniker;
begin
  Result := nil;
  if Moniker <> nil then begin
    if (Moniker.IsSystemMoniker(Mksys) = 0) and
      (Mksys = MKSYS_GENERICCOMPOSITE) then
    begin
      if Moniker.Enum(True, EnumMoniker) <> 0 then Exit;
      EnumMoniker.Next(1, Result, nil);
      ReleaseObject(EnumMoniker);
    end
    else begin
{$IFNDEF COMPILER3_UP}
      Moniker.AddRef;
{$ENDIF}
      Result := Moniker;
    end;
  end;
end;

{ Return length of file moniker piece of the given moniker }

function OleStdGetLenFilePrefixOfMoniker(Moniker: IMoniker): Integer;
var
  MkFirst: IMoniker;
  BindCtx: IBindCtx;
  Mksys: Longint;
  P: PWideChar;
begin
  Result := 0;
  if Moniker <> nil then begin
    MkFirst := OleStdGetFirstMoniker(Moniker);
    if MkFirst <> nil then begin
      if (MkFirst.IsSystemMoniker(Mksys) = 0) and
        (Mksys = MKSYS_FILEMONIKER) then
      begin
        if CreateBindCtx(0, BindCtx) = 0 then begin
          if (MkFirst.GetDisplayName(BindCtx, nil, P) = 0) and (P <> nil) then
          begin
            Result := WStrLen(P);
            CoTaskMemFree(P);
          end;
          ReleaseObject(BindCtx);
        end;
      end;
      ReleaseObject(MkFirst);
    end;
  end;
end;

function CoAllocCStr(const S: string): PChar;
begin
  Result := StrCopy(CoTaskMemAlloc(Length(S) + 1), PChar(S));
end;

function WStrToString(P: PWideChar): string;
begin
  Result := '';
  if P <> nil then begin
    Result := WideCharToString(P);
    CoTaskMemFree(P);
  end;
end;

function GetFullNameStr(OleObject: IOleObject): string;
var
  P: PWideChar;
begin
  OleObject.GetUserType(USERCLASSTYPE_FULL, P);
  Result := WStrToString(P);
end;

function GetShortNameStr(OleObject: IOleObject): string;
var
  P: PWideChar;
begin
  OleObject.GetUserType(USERCLASSTYPE_SHORT, P);
  Result := WStrToString(P);
end;

function GetDisplayNameStr(OleLink: IOleLink): string;
var
  P: PWideChar;
begin
  OleLink.GetSourceDisplayName(P);
  Result := WStrToString(P);
end;

{$IFDEF COMPILER3_UP}
function GetVCLFrameForm(Form: TCustomForm): IVCLFrameForm;
begin
  if Form.OleFormObject = nil then TOleForm.Create(Form);
  Result := Form.OleFormObject as IVCLFrameForm;
end;

function IsFormMDIChild(Form: TCustomForm): Boolean;
begin
  Result := (Form is TForm) and (TForm(Form).FormStyle = fsMDIChild);
end;
{$ENDIF}

{ Clipboard formats }

var
  CFEmbeddedObject: Integer;
  CFLinkSource: Integer;
  CFRtf: Integer;
  CFRtfNoObjs: Integer;

const
{$IFNDEF COMPILER3_UP}
  CF_RTFNOOBJS = 'Rich Text Format Without Objects';
{$ENDIF}
  CF_EMBEDDEDOBJECT = 'Embedded Object';
  CF_LINKSOURCE = 'Link Source';

{************************************************************************}

{ OLE Extensions to the Rich Text Editor }
{ Converted from RICHOLE.H               }

{ Structure passed to GetObject and InsertObject }

type
  _ReObject = record
    cbStruct: DWORD;           { Size of structure                }
    cp: ULONG;                 { Character position of object     }
    clsid: TCLSID;             { Class ID of object               }
    poleobj: IOleObject;       { OLE object interface             }
    pstg: IStorage;            { Associated storage interface     }
    polesite: IOleClientSite;  { Associated client site interface }
    sizel: TSize;              { Size of object (may be 0,0)      }
    dvAspect: Longint;         { Display aspect to use            }
    dwFlags: DWORD;            { Object status flags              }
    dwUser: DWORD;             { Dword for user's use             }
  end;
  TReObject = _ReObject;

const

{ Flags to specify which interfaces should be returned in the structure above }

  REO_GETOBJ_NO_INTERFACES   =  $00000000;
  REO_GETOBJ_POLEOBJ         =  $00000001;
  REO_GETOBJ_PSTG            =  $00000002;
  REO_GETOBJ_POLESITE        =  $00000004;
  REO_GETOBJ_ALL_INTERFACES  =  $00000007;

{ Place object at selection }

  REO_CP_SELECTION    = ULONG(-1);

{ Use character position to specify object instead of index }

  REO_IOB_SELECTION   = ULONG(-1);
  REO_IOB_USE_CP      = ULONG(-2);

{ Object flags }

  REO_NULL            = $00000000;  { No flags                         }
  REO_READWRITEMASK   = $0000003F;  { Mask out RO bits                 }
  REO_DONTNEEDPALETTE = $00000020;  { Object doesn't need palette      }
  REO_BLANK           = $00000010;  { Object is blank                  }
  REO_DYNAMICSIZE     = $00000008;  { Object defines size always       }
  REO_INVERTEDSELECT  = $00000004;  { Object drawn all inverted if sel }
  REO_BELOWBASELINE   = $00000002;  { Object sits below the baseline   }
  REO_RESIZABLE       = $00000001;  { Object may be resized            }
  REO_LINK            = $80000000;  { Object is a link (RO)            }
  REO_STATIC          = $40000000;  { Object is static (RO)            }
  REO_SELECTED        = $08000000;  { Object selected (RO)             }
  REO_OPEN            = $04000000;  { Object open in its server (RO)   }
  REO_INPLACEACTIVE   = $02000000;  { Object in place active (RO)      }
  REO_HILITED         = $01000000;  { Object is to be hilited (RO)     }
  REO_LINKAVAILABLE   = $00800000;  { Link believed available (RO)     }
  REO_GETMETAFILE     = $00400000;  { Object requires metafile (RO)    }

{ Flags for IRichEditOle.GetClipboardData,   }
{ IRichEditOleCallback.GetClipboardData and  }
{ IRichEditOleCallback.QueryAcceptData       }

  RECO_PASTE          = $00000000;  { paste from clipboard  }
  RECO_DROP           = $00000001;  { drop                  }
  RECO_COPY           = $00000002;  { copy to the clipboard }
  RECO_CUT            = $00000003;  { cut to the clipboard  }
  RECO_DRAG           = $00000004;  { drag                  }

{ RichEdit GUIDs }

  IID_IRichEditOle: TGUID = (
    D1:$00020D00;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  IID_IRichEditOleCallback: TGUID = (
    D1:$00020D03;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));

type

{
 *  IRichEditOle
 *
 *  Purpose:
 *    Interface used by the client of RichEdit to perform OLE-related
 *    operations.
 *
 *    The methods herein may just want to be regular Windows messages.
}

{$IFDEF COMPILER3_UP}
  IRichEditOle = interface(IUnknown)
    ['{00020d00-0000-0000-c000-000000000046}']
    function GetClientSite(out clientSite: IOleClientSite): HResult; stdcall;
    function GetObjectCount: HResult; stdcall;
    function GetLinkCount: HResult; stdcall;
    function GetObject(iob: Longint; out reobject: TReObject;
      dwFlags: DWORD): HResult; stdcall;
    function InsertObject(var reobject: TReObject): HResult; stdcall;
    function ConvertObject(iob: Longint; rclsidNew: TIID;
      lpstrUserTypeNew: LPCSTR): HResult; stdcall;
    function ActivateAs(rclsid: TIID; rclsidAs: TIID): HResult; stdcall;
    function SetHostNames(lpstrContainerApp: LPCSTR;
      lpstrContainerObj: LPCSTR): HResult; stdcall;
    function SetLinkAvailable(iob: Longint; fAvailable: BOOL): HResult; stdcall;
    function SetDvaspect(iob: Longint; dvaspect: DWORD): HResult; stdcall;
    function HandsOffStorage(iob: Longint): HResult; stdcall;
    function SaveCompleted(iob: Longint; const stg: IStorage): HResult; stdcall;
    function InPlaceDeactivate: HResult; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    function GetClipboardData(var chrg: TCharRange; reco: DWORD;
      out dataobj: IDataObject): HResult; stdcall;
    function ImportDataObject(dataobj: IDataObject; cf: TClipFormat;
      hMetaPict: HGLOBAL): HResult; stdcall;
  end;
{$ELSE}
  IRichEditOle = class(IUnknown)
    function GetClientSite(var clientSite: IOleClientSite): HResult; virtual; stdcall; abstract;
    function GetObjectCount: HResult; virtual; stdcall; abstract;
    function GetLinkCount: HResult; virtual; stdcall; abstract;
    function GetObject(iob: Longint; var reobject: TReObject;
      dwFlags: DWORD): HResult; virtual; stdcall; abstract;
    function InsertObject(var reobject: TReObject): HResult; virtual; stdcall; abstract;
    function ConvertObject(iob: Longint; rclsidNew: TIID;
      lpstrUserTypeNew: LPCSTR): HResult; virtual; stdcall; abstract;
    function ActivateAs(rclsid: TIID; rclsidAs: TIID): HResult; virtual; stdcall; abstract;
    function SetHostNames(lpstrContainerApp: LPCSTR;
      lpstrContainerObj: LPCSTR): HResult; virtual; stdcall; abstract;
    function SetLinkAvailable(iob: Longint; fAvailable: BOOL): HResult; virtual; stdcall; abstract;
    function SetDvaspect(iob: Longint; dvaspect: DWORD): HResult; virtual; stdcall; abstract;
    function HandsOffStorage(iob: Longint): HResult; virtual; stdcall; abstract;
    function SaveCompleted(iob: Longint; const stg: IStorage): HResult; virtual; stdcall; abstract;
    function InPlaceDeactivate: HResult; virtual; stdcall; abstract;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; virtual; stdcall; abstract;
    function GetClipboardData(var chrg: TCharRange; reco: DWORD;
      var dataobj: IDataObject): HResult; virtual; stdcall; abstract;
    function ImportDataObject(dataobj: IDataObject; cf: TClipFormat;
      hMetaPict: HGLOBAL): HResult; virtual; stdcall; abstract;
  end;
{$ENDIF}

{
 *  IRichEditOleCallback
 *
 *  Purpose:
 *    Interface used by the RichEdit to get OLE-related stuff from the
 *    application using RichEdit.
}

{$IFDEF COMPILER3_UP}
  IRichEditOleCallback = interface(IUnknown)
    ['{00020d03-0000-0000-c000-000000000046}']
    function GetNewStorage(out stg: IStorage): HResult; stdcall;
    function GetInPlaceContext(out Frame: IOleInPlaceFrame;
      out Doc: IOleInPlaceUIWindow;
      lpFrameInfo: POleInPlaceFrameInfo): HResult; stdcall;
    function ShowContainerUI(fShow: BOOL): HResult; stdcall;
    function QueryInsertObject(const clsid: TCLSID; const stg: IStorage;
      cp: Longint): HResult; stdcall;
    function DeleteObject(const oleobj: IOleObject): HResult; stdcall;
    function QueryAcceptData(const dataobj: IDataObject;
      var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL;
      hMetaPict: HGLOBAL): HResult; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
      out dataobj: IDataObject): HResult; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
      var dwEffect: DWORD): HResult; stdcall;
    function GetContextMenu(seltype: Word; const oleobj: IOleObject;
      const chrg: TCharRange; out menu: HMENU): HResult; stdcall;
  end;
{$ELSE}
  IRichEditOleCallback = class(IUnknown)
    function GetNewStorage(var stg: IStorage): HResult; virtual; stdcall; abstract;
    function GetInPlaceContext(var Frame: IOleInPlaceFrame;
      var Doc: IOleInPlaceUIWindow;
      lpFrameInfo: POleInPlaceFrameInfo): HResult; virtual; stdcall; abstract;
    function ShowContainerUI(fShow: BOOL): HResult; virtual; stdcall; abstract;
    function QueryInsertObject(const clsid: TCLSID; const stg: IStorage;
      cp: Longint): HResult; virtual; stdcall; abstract;
    function DeleteObject(const oleobj: IOleObject): HResult; virtual; stdcall; abstract;
    function QueryAcceptData(const dataobj: IDataObject;
      var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL;
      hMetaPict: HGLOBAL): HResult; virtual; stdcall; abstract;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; virtual; stdcall; abstract;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
      var dataobj: IDataObject): HResult; virtual; stdcall; abstract;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
      var dwEffect: DWORD): HResult; virtual; stdcall; abstract;
    function GetContextMenu(seltype: Word; const oleobj: IOleObject;
      const chrg: TCharRange; var menu: HMENU): HResult; virtual; stdcall; abstract;
  end;
{$ENDIF}

{************************************************************************}

{ TRichEditOleCallback }

type
{$IFDEF COMPILER3_UP}
  TRichEditOleCallback = class(TObject, IUnknown, IRichEditOleCallback)
  private
    FDocForm: IVCLFrameForm;
    FFrameForm: IVCLFrameForm;
    FAccelTable: HAccel;
    FAccelCount: Integer;
{$IFDEF COMPILER4_UP}
    FAutoScroll: Boolean;
{$ENDIF}
    procedure CreateAccelTable;
    procedure DestroyAccelTable;
    procedure AssignFrame;
{$ELSE}
  TRichEditOleCallback = class(IRichEditOleCallback)
{$ENDIF}
  private
    FRefCount: Longint;
    FRichEdit: TJvCustomRichEdit;
  public
    constructor Create(RichEdit: TJvCustomRichEdit);
    destructor Destroy; override;
{$IFDEF COMPILER3_UP}
    function QueryInterface(const iid: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Longint; stdcall;
    function _Release: Longint; stdcall;
    function GetNewStorage(out stg: IStorage): HResult; stdcall;
    function GetInPlaceContext(out Frame: IOleInPlaceFrame;
      out Doc: IOleInPlaceUIWindow;
      lpFrameInfo: POleInPlaceFrameInfo): HResult; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
      out dataobj: IDataObject): HResult; stdcall;
    function GetContextMenu(seltype: Word; const oleobj: IOleObject;
      const chrg: TCharRange; out menu: HMENU): HResult; stdcall;
{$ELSE}
    function QueryInterface(const iid: TIID; var Obj): HResult; override;
    function AddRef: Longint; override;
    function Release: Longint; override;
    function GetNewStorage(var stg: IStorage): HResult; override;
    function GetInPlaceContext(var Frame: IOleInPlaceFrame;
      var Doc: IOleInPlaceUIWindow;
      lpFrameInfo: POleInPlaceFrameInfo): HResult; override;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
      var dataobj: IDataObject): HResult; override;
    function GetContextMenu(seltype: Word; const oleobj: IOleObject;
      const chrg: TCharRange; var menu: HMENU): HResult; override;
{$ENDIF}
    function ShowContainerUI(fShow: BOOL): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
    function QueryInsertObject(const clsid: TCLSID; const stg: IStorage;
      cp: Longint): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
    function DeleteObject(const oleobj: IOleObject): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
    function QueryAcceptData(const dataobj: IDataObject; var cfFormat: TClipFormat;
      reco: DWORD; fReally: BOOL; hMetaPict: HGLOBAL): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
      var dwEffect: DWORD): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
  end;

constructor TRichEditOleCallback.Create(RichEdit: TJvCustomRichEdit);
begin
  inherited Create;
  FRichEdit := RichEdit;
end;

destructor TRichEditOleCallback.Destroy;
begin
{$IFDEF COMPILER3_UP}
  DestroyAccelTable;
  FFrameForm := nil;
  FDocForm := nil;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF COMPILER3_UP}

function TRichEditOleCallback.QueryInterface(const iid: TGUID; out Obj): HResult;
begin
  if GetInterface(iid, Obj) then Result := S_OK
  else Result := E_NOINTERFACE;
end;

function TRichEditOleCallback._AddRef: Longint;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TRichEditOleCallback._Release: Longint;
begin
  Dec(FRefCount);
  Result := FRefCount;
end;

procedure TRichEditOleCallback.CreateAccelTable;
var
  Menu: TMainMenu;
begin
  if (FAccelTable = 0) and Assigned(FFrameForm) then begin
    Menu := FFrameForm.Form.Menu;
    if Menu <> nil then
      Menu.GetOle2AcceleratorTable(FAccelTable, FAccelCount, [0, 2, 4]);
  end;
end;

procedure TRichEditOleCallback.DestroyAccelTable;
begin
  if FAccelTable <> 0 then begin
    DestroyAcceleratorTable(FAccelTable);
    FAccelTable := 0;
    FAccelCount := 0;
  end;
end;

procedure TRichEditOleCallback.AssignFrame;
begin
  if (GetParentForm(FRichEdit) <> nil) and not Assigned(FFrameForm) and
    FRichEdit.AllowInPlace then
  begin
    FDocForm := GetVCLFrameForm(ValidParentForm(FRichEdit));
    FFrameForm := FDocForm;
    if IsFormMDIChild(FDocForm.Form) then
      FFrameForm := GetVCLFrameForm(Application.MainForm);
  end;
end;

{$ELSE}

function TRichEditOleCallback.QueryInterface(const iid: TIID; var Obj): HResult;
begin
  if IsEqualIID(iid, IID_IUnknown) or
    IsEqualIID(iid, IID_IRichEditOleCallback) then
  begin
    Pointer(Obj) := Self;
    AddRef;
    Result := S_OK;
  end else begin
    Pointer(Obj) := nil;
    Result := E_NOINTERFACE;
  end;
end;

function TRichEditOleCallback.AddRef: Longint;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TRichEditOleCallback.Release: Longint;
begin
  Dec(FRefCount);
  Result := FRefCount;
end;

{$ENDIF COMPILER3_UP}

function TRichEditOleCallback.GetNewStorage(
  {$IFDEF COMPILER3_UP} out {$ELSE} var {$ENDIF} stg: IStorage): HResult;
begin
  try
    CreateStorage(stg);
    Result := S_OK;
  except
    Result:= E_OUTOFMEMORY;
  end;
end;

function TRichEditOleCallback.GetInPlaceContext(
  {$IFDEF COMPILER3_UP} out {$ELSE} var {$ENDIF} Frame: IOleInPlaceFrame;
  {$IFDEF COMPILER3_UP} out {$ELSE} var {$ENDIF} Doc: IOleInPlaceUIWindow;
  lpFrameInfo: POleInPlaceFrameInfo): HResult;
begin
{$IFDEF COMPILER3_UP}
  AssignFrame;
  if Assigned(FFrameForm) and FRichEdit.AllowInPlace then begin
    Frame := FFrameForm;
    Doc := FDocForm;
    CreateAccelTable;
    with lpFrameInfo^ do begin
      fMDIApp := False;
      FFrameForm.GetWindow(hWndFrame);
      hAccel := FAccelTable;
      cAccelEntries := FAccelCount;
    end;
    Result := S_OK;
  end
  else Result := E_NOTIMPL;
{$ELSE}
  Result := E_NOTIMPL;
{$ENDIF}
end;

function TRichEditOleCallback.QueryInsertObject(const clsid: TCLSID; const stg: IStorage;
  cp: Longint): HResult;
begin
  Result := NOERROR;
end;

function TRichEditOleCallback.DeleteObject(const oleobj: IOleObject): HResult;
begin
  if Assigned(oleobj) then oleobj.Close(OLECLOSE_NOSAVE);
  Result := NOERROR;
end;

function TRichEditOleCallback.QueryAcceptData(const dataobj: IDataObject;
  var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL;
  hMetaPict: HGLOBAL): HResult;
begin
  Result := S_OK;
end;

function TRichEditOleCallback.ContextSensitiveHelp(fEnterMode: BOOL): HResult;
begin
  Result := NOERROR;
end;

function TRichEditOleCallback.GetClipboardData(const chrg: TCharRange; reco: DWORD;
  {$IFDEF COMPILER3_UP} out {$ELSE} var {$ENDIF} dataobj: IDataObject): HResult;
begin
  Result := E_NOTIMPL;
end;

function TRichEditOleCallback.GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
  var dwEffect: DWORD): HResult;
begin
  Result := E_NOTIMPL;
end;

function TRichEditOleCallback.GetContextMenu(seltype: Word;
  const oleobj: IOleObject; const chrg: TCharRange;
  {$IFDEF COMPILER3_UP} out {$ELSE} var {$ENDIF} menu: HMENU): HResult;
begin
  Result := E_NOTIMPL;
end;

function TRichEditOleCallback.ShowContainerUI(fShow: BOOL): HResult;
begin
{$IFDEF COMPILER3_UP}
  if not fShow then AssignFrame;
  if Assigned(FFrameForm) then begin
    if fShow then begin
      FFrameForm.SetMenu(0, 0, 0);
      FFrameForm.ClearBorderSpace;
      FRichEdit.SetUIActive(False);
      DestroyAccelTable;
{$IFDEF COMPILER4_UP}
      TForm(FFrameForm.Form).AutoScroll := FAutoScroll;
{$ENDIF}
      FFrameForm := nil;
      FDocForm := nil;
    end
    else begin
{$IFDEF COMPILER4_UP}
      FAutoScroll := TForm(FFrameForm.Form).AutoScroll;
      TForm(FFrameForm.Form).AutoScroll := False;
{$ENDIF}
      FRichEdit.SetUIActive(True);
    end;
    Result := S_OK;
  end
  else Result := E_NOTIMPL;
{$ELSE}
  Result := E_NOTIMPL;
{$ENDIF}
end;

{ TOleUIObjInfo - helper interface for Object Properties dialog }

type
{$IFDEF COMPILER3_UP}
  TOleUIObjInfo = class(TInterfacedObject, IOleUIObjInfo)
{$ELSE}
  TOleUIObjInfo = class(IOleUIObjInfo)
{$ENDIF}
  private
    FRichEdit: TJvCustomRichEdit;
    FReObject: TReObject;
  public
    constructor Create(RichEdit: TJvCustomRichEdit; ReObject: TReObject);
{$IFNDEF COMPILER3_UP}
    function QueryInterface(const iid: TIID; var obj): HResult; override;
    function AddRef: Longint; override;
    function Release: Longint; override;
{$ENDIF}
    function GetObjectInfo(dwObject: Longint;
      var dwObjSize: Longint; var lpszLabel: PChar;
      var lpszType: PChar; var lpszShortType: PChar;
      var lpszLocation: PChar): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
    function GetConvertInfo(dwObject: Longint; var ClassID: TCLSID;
      var wFormat: Word; var ConvertDefaultClassID: TCLSID;
      var lpClsidExclude: PCLSID; var cClsidExclude: Longint): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
    function ConvertObject(dwObject: Longint;
      const clsidNew: TCLSID): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
    function GetViewInfo(dwObject: Longint; var hMetaPict: HGlobal;
      var dvAspect: Longint; var nCurrentScale: Integer): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
    function SetViewInfo(dwObject: Longint; hMetaPict: HGlobal;
      dvAspect: Longint; nCurrentScale: Integer;
      bRelativeToOrig: BOOL): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
  end;

constructor TOleUIObjInfo.Create(RichEdit: TJvCustomRichEdit;
  ReObject: TReObject);
begin
  inherited Create;
  FRichEdit := RichEdit;
  FReObject := ReObject;
end;

{$IFNDEF COMPILER3_UP}

function TOleUIObjInfo.QueryInterface(const iid: TIID; var obj): HResult;
begin
  Pointer(obj) := nil;
  Result := E_NOINTERFACE;
end;

function TOleUIObjInfo.AddRef: Longint;
begin
  Result := 0;
end;

function TOleUIObjInfo.Release: Longint;
begin
  Result := 0;
end;

{$ENDIF COMPILER3_UP}

function TOleUIObjInfo.GetObjectInfo(dwObject: Longint;
  var dwObjSize: Longint; var lpszLabel: PChar;
  var lpszType: PChar; var lpszShortType: PChar;
  var lpszLocation: PChar): HResult;
begin
  if @dwObjSize <> nil then
    dwObjSize := -1 { Unknown size };
  if @lpszLabel <> nil then
    lpszLabel := CoAllocCStr(GetFullNameStr(FReObject.poleobj));
  if @lpszType <> nil then
    lpszType := CoAllocCStr(GetFullNameStr(FReObject.poleobj));
  if @lpszShortType <> nil then
    lpszShortType := CoAllocCStr(GetShortNameStr(FReObject.poleobj));
  if (@lpszLocation <> nil) then begin
    if Trim(FRichEdit.Title) <> '' then
      lpszLocation := CoAllocCStr(Format('%s - %s',
        [FRichEdit.Title, Application.Title]))
    else
      lpszLocation := CoAllocCStr(Application.Title);
  end;
  Result := S_OK;
end;

function TOleUIObjInfo.GetConvertInfo(dwObject: Longint; var ClassID: TCLSID;
  var wFormat: Word; var ConvertDefaultClassID: TCLSID;
  var lpClsidExclude: PCLSID; var cClsidExclude: Longint): HResult;
begin
  FReObject.poleobj.GetUserClassID(ClassID);
  Result := S_OK;
end;

function TOleUIObjInfo.ConvertObject(dwObject: Longint;
  const clsidNew: TCLSID): HResult;
begin
  Result := E_NOTIMPL;
end;

function TOleUIObjInfo.GetViewInfo(dwObject: Longint; var hMetaPict: HGlobal;
  var dvAspect: Longint; var nCurrentScale: Integer): HResult;
begin
  if @hMetaPict <> nil then
    hMetaPict := GetIconMetaPict(FReObject.poleobj, FReObject.dvAspect);
  if @dvAspect <> nil then dvAspect := FReObject.dvAspect;
  if @nCurrentScale <> nil then nCurrentScale := 0;
  Result := S_OK;
end;

function TOleUIObjInfo.SetViewInfo(dwObject: Longint; hMetaPict: HGlobal;
  dvAspect: Longint; nCurrentScale: Integer;
  bRelativeToOrig: BOOL): HResult;
var
  Iconic: Boolean;
begin
  if Assigned(FRichEdit.FRichEditOle) then begin
    case dvAspect of
      DVASPECT_CONTENT:
        Iconic := False;
      DVASPECT_ICON:
        Iconic := True;
      else
        Iconic := FReObject.dvAspect = DVASPECT_ICON;
    end;
    IRichEditOle(FRichEdit.FRichEditOle).InPlaceDeactivate;
    Result := OleSetDrawAspect(FReObject.poleobj, Iconic, hMetaPict,
      FReObject.dvAspect);
    if Succeeded(Result) then
      IRichEditOle(FRichEdit.FRichEditOle).SetDvaspect(
        Longint(REO_IOB_SELECTION), FReObject.dvAspect);
  end
  else Result := E_NOTIMPL;
end;

{ TOleUILinkInfo - helper interface for Object Properties dialog }

type
{$IFDEF COMPILER3_UP}
  TOleUILinkInfo = class(TInterfacedObject, IOleUILinkInfo)
{$ELSE}
  TOleUILinkInfo = class(IOleUILinkInfo)
{$ENDIF}
  private
    FReObject: TReObject;
    FRichEdit: TJvCustomRichEdit;
    FOleLink: IOleLink;
  public
    constructor Create(RichEdit: TJvCustomRichEdit; ReObject: TReObject);
{$IFNDEF COMPILER3_UP}
    destructor Destroy; override;
    function QueryInterface(const iid: TIID; var obj): HResult; override;
    function AddRef: Longint; override;
    function Release: Longint; override;
{$ENDIF}
    function GetNextLink(dwLink: Longint): Longint;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
    function SetLinkUpdateOptions(dwLink: Longint;
      dwUpdateOpt: Longint): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
    function GetLinkUpdateOptions(dwLink: Longint;
      var dwUpdateOpt: Longint): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
    function SetLinkSource(dwLink: Longint; pszDisplayName: PChar;
      lenFileName: Longint; var chEaten: Longint;
      fValidateSource: BOOL): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
    function GetLinkSource(dwLink: Longint; var pszDisplayName: PChar;
      var lenFileName: Longint; var pszFullLinkType: PChar;
      var pszShortLinkType: PChar; var fSourceAvailable: BOOL;
      var fIsSelected: BOOL): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
    function OpenLinkSource(dwLink: Longint): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
    function UpdateLink(dwLink: Longint; fErrorMessage: BOOL;
      fErrorAction: BOOL): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
    function CancelLink(dwLink: Longint): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
    function GetLastUpdate(dwLink: Longint;
      var LastUpdate: TFileTime): HResult;
      {$IFDEF COMPILER3_UP} stdcall {$ELSE} override {$ENDIF};
  end;

{$IFDEF COMPILER3_UP}
procedure LinkError(const Ident: string);
begin
  Application.MessageBox(PChar(Ident), PChar(SLinkProperties),
    MB_OK or MB_ICONSTOP);
end;
{$ELSE}
procedure LinkError(Ident: Integer);
begin
  Application.MessageBox(PChar(LoadStr(Ident)),
    PChar(LoadStr(SLinkProperties)), MB_OK or MB_ICONSTOP);
end;
{$ENDIF}

constructor TOleUILinkInfo.Create(RichEdit: TJvCustomRichEdit;
  ReObject: TReObject);
begin
  inherited Create;
  FReObject := ReObject;
  FRichEdit := RichEdit;
{$IFDEF COMPILER3_UP}
  OleCheck(FReObject.poleobj.QueryInterface(IOleLink, FOleLink));
{$ELSE}
  OleCheck(FReObject.poleobj.QueryInterface(IID_IOleLink, FOleLink));
{$ENDIF}
end;

{$IFNDEF COMPILER3_UP}

destructor TOleUILinkInfo.Destroy;
begin
  ReleaseObject(FOleLink);
  inherited Destroy;
end;

function TOleUILinkInfo.QueryInterface(const iid: TIID; var obj): HResult;
begin
  Pointer(obj) := nil;
  Result := E_NOINTERFACE;
end;

function TOleUILinkInfo.AddRef: Longint;
begin
  Result := 0;
end;

function TOleUILinkInfo.Release: Longint;
begin
  Result := 0;
end;

{$ENDIF}

function TOleUILinkInfo.GetNextLink(dwLink: Longint): Longint;
begin
  if dwLink = 0 then Result := Longint(FRichEdit)
  else Result := 0;
end;

function TOleUILinkInfo.SetLinkUpdateOptions(dwLink: Longint;
  dwUpdateOpt: Longint): HResult;
begin
  Result := FOleLink.SetUpdateOptions(dwUpdateOpt);
  if Succeeded(Result) then FRichEdit.Modified := True;
end;

function TOleUILinkInfo.GetLinkUpdateOptions(dwLink: Longint;
  var dwUpdateOpt: Longint): HResult;
begin
  Result := FOleLink.GetUpdateOptions(dwUpdateOpt);
end;

function TOleUILinkInfo.SetLinkSource(dwLink: Longint; pszDisplayName: PChar;
  lenFileName: Longint; var chEaten: Longint;
  fValidateSource: BOOL): HResult;
var
  DisplayName: string;
  Buffer: array[0..255] of WideChar;
begin
  Result := E_FAIL;
  if fValidateSource then begin
    DisplayName := pszDisplayName;
    if Succeeded(FOleLink.SetSourceDisplayName(StringToWideChar(DisplayName,
      Buffer, SizeOf(Buffer) div 2))) then
    begin
      chEaten := Length(DisplayName);
      try
        OleCheck(FReObject.poleobj.Update);
      except
        Application.HandleException(FRichEdit);
      end;
      Result := S_OK;
    end;
  end
  else LinkError(SInvalidLinkSource);
end;

function TOleUILinkInfo.GetLinkSource(dwLink: Longint; var pszDisplayName: PChar;
  var lenFileName: Longint; var pszFullLinkType: PChar;
  var pszShortLinkType: PChar; var fSourceAvailable: BOOL;
  var fIsSelected: BOOL): HResult;
var
  Moniker: IMoniker;
begin
  if @pszDisplayName <> nil then
    pszDisplayName := CoAllocCStr(GetDisplayNameStr(FOleLink));
  if @lenFileName <> nil then begin
    lenFileName := 0;
    FOleLink.GetSourceMoniker(Moniker);
    if Moniker <> nil then begin
      lenFileName := OleStdGetLenFilePrefixOfMoniker(Moniker);
      ReleaseObject(Moniker);
    end;
  end;
  if @pszFullLinkType <> nil then
    pszFullLinkType := CoAllocCStr(GetFullNameStr(FReObject.poleobj));
  if @pszShortLinkType <> nil then
    pszShortLinkType := CoAllocCStr(GetShortNameStr(FReObject.poleobj));
  Result := S_OK;
end;

function TOleUILinkInfo.OpenLinkSource(dwLink: Longint): HResult;
begin
  try
    OleCheck(FReObject.poleobj.DoVerb(OLEIVERB_SHOW, nil, FReObject.polesite,
      0, FRichEdit.Handle, FRichEdit.ClientRect));
  except
    Application.HandleException(FRichEdit);
  end;
  Result := S_OK;
end;

function TOleUILinkInfo.UpdateLink(dwLink: Longint; fErrorMessage: BOOL;
  fErrorAction: BOOL): HResult;
begin
  try
    OleCheck(FReObject.poleobj.Update);
  except
    Application.HandleException(FRichEdit);
  end;
  Result := S_OK;
end;

function TOleUILinkInfo.CancelLink(dwLink: Longint): HResult;
begin
  LinkError(SCannotBreakLink);
  Result := E_NOTIMPL;
end;

function TOleUILinkInfo.GetLastUpdate(dwLink: Longint;
  var LastUpdate: TFileTime): HResult;
begin
  Result := S_OK;
end;

{ Get RichEdit OLE interface }

function GetRichEditOle(Wnd: HWnd; var RichEditOle): Boolean;
begin
  Result := SendMessage(Wnd, EM_GETOLEINTERFACE, 0, Longint(@RichEditOle)) <> 0;
end;

{ TJvRichEditStrings }

const
  ReadError  = $0001;
  WriteError = $0002;
  NoError    = $0000;

type
  TJvRichEditStrings = class(TStrings)
  private
    RichEdit: TJvCustomRichEdit;
    FFormat: TRichStreamFormat;
    FMode: TRichStreamModes;
    FConverter: TConversion;
    procedure EnableChange(const Value: Boolean);
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure SetTextStr(const Value: string); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure AddStrings(Strings: TStrings); override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToFile(const FileName: string); override;
    procedure SaveToStream(Stream: TStream); override;
    property Format: TRichStreamFormat read FFormat write FFormat;
    property Mode: TRichStreamModes read FMode write FMode;
  end;

destructor TJvRichEditStrings.Destroy;
begin
  FConverter.Free;
  inherited Destroy;
end;

procedure TJvRichEditStrings.AddStrings(Strings: TStrings);
var
  SelChange: TNotifyEvent;
begin
  SelChange := RichEdit.OnSelectionChange;
  RichEdit.OnSelectionChange := nil;
  try
    inherited AddStrings(Strings);
  finally
    RichEdit.OnSelectionChange := SelChange;
  end;
end;

function TJvRichEditStrings.GetCount: Integer;
begin
  with RichEdit do begin
    Result := SendMessage(Handle, EM_GETLINECOUNT, 0, 0);
    if GetLineLength(GetLineIndex(Result - 1)) = 0 then Dec(Result);
  end;
end;

function TJvRichEditStrings.Get(Index: Integer): string;
var
  Text: array[0..4095] of Char;
  L: Integer;
begin
  Word((@Text)^) := SizeOf(Text);
  L := SendMessage(RichEdit.Handle, EM_GETLINE, Index, Longint(@Text));
  if (Text[L - 2] = #13) and (Text[L - 1] = #10) then Dec(L, 2)
  else if (RichEditVersion >= 2) and (Text[L - 1] = #13) then Dec(L);
  SetString(Result, Text, L);
end;

procedure TJvRichEditStrings.Put(Index: Integer; const S: string);
var
  Selection: TCharRange;
begin
  if Index >= 0 then
  begin
    Selection.cpMin := RichEdit.GetLineIndex(Index);
    if Selection.cpMin <> -1 then begin
      Selection.cpMax := Selection.cpMin +
        RichEdit.GetLineLength(Selection.cpMin);
      SendMessage(RichEdit.Handle, EM_EXSETSEL, 0, Longint(@Selection));
      RichEdit.FLinesUpdating := True;
      try
        SendMessage(RichEdit.Handle, EM_REPLACESEL, 0, Longint(PChar(S)));
      finally
        RichEdit.FLinesUpdating := False;
      end;
    end;
  end;
end;

procedure TJvRichEditStrings.Insert(Index: Integer; const S: string);
var
  L: Integer;
  Selection: TCharRange;
  Fmt: PChar;
  Str: string;
begin
  if Index >= 0 then begin
    Selection.cpMin := RichEdit.GetLineIndex(Index);
    if Selection.cpMin >= 0 then begin
      if RichEditVersion = 1 then Fmt := '%s'#13#10
      else Fmt := '%s'#13;
    end
    else begin
      Selection.cpMin := RichEdit.GetLineIndex(Index - 1);
      if Selection.cpMin < 0 then Exit;
      L := RichEdit.GetLineLength(Selection.cpMin);
      if L = 0 then Exit;
      Inc(Selection.cpMin, L);
      if RichEditVersion = 1 then Fmt := #13#10'%s'
      else Fmt := #13'%s';
    end;
    Selection.cpMax := Selection.cpMin;
    SendMessage(RichEdit.Handle, EM_EXSETSEL, 0, Longint(@Selection));
    Str := SysUtils.Format(Fmt, [S]);
    RichEdit.FLinesUpdating := True;
    try
      SendMessage(RichEdit.Handle, EM_REPLACESEL, 0, Longint(PChar(Str)));
    finally
      RichEdit.FLinesUpdating := False;
    end;
    if RichEditVersion = 1 then
      if RichEdit.SelStart <> (Selection.cpMax + Length(Str)) then
        raise EOutOfResources.Create(ResStr(sRichEditInsertError));
  end;
end;

procedure TJvRichEditStrings.Delete(Index: Integer);
const
  Empty: PChar = '';
var
  Selection: TCharRange;
begin
  if Index < 0 then Exit;
  Selection.cpMin := RichEdit.GetLineIndex(Index);
  if Selection.cpMin <> -1 then begin
    Selection.cpMax := RichEdit.GetLineIndex(Index + 1);
    if Selection.cpMax = -1 then
      Selection.cpMax := Selection.cpMin +
        RichEdit.GetLineLength(Selection.cpMin);
    SendMessage(RichEdit.Handle, EM_EXSETSEL, 0, Longint(@Selection));
    RichEdit.FLinesUpdating := True;
    try
      SendMessage(RichEdit.Handle, EM_REPLACESEL, 0, Longint(Empty));
    finally
      RichEdit.FLinesUpdating := False;
    end;
  end;
end;

procedure TJvRichEditStrings.Clear;
begin
  RichEdit.Clear;
end;

procedure TJvRichEditStrings.SetUpdateState(Updating: Boolean);
begin
  if RichEdit.Showing then
    SendMessage(RichEdit.Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if not Updating then begin
    RichEdit.Refresh;
    RichEdit.Perform(CM_TEXTCHANGED, 0, 0);
  end;
end;

procedure TJvRichEditStrings.EnableChange(const Value: Boolean);
var
  EventMask: Longint;
begin
  with RichEdit do begin
    EventMask := SendMessage(Handle, EM_GETEVENTMASK, 0, 0);
    if Value then
      EventMask := EventMask or ENM_CHANGE
    else
      EventMask := EventMask and not ENM_CHANGE;
    SendMessage(Handle, EM_SETEVENTMASK, 0, EventMask);
  end;
end;

procedure TJvRichEditStrings.SetTextStr(const Value: string);
begin
  EnableChange(False);
  try
    inherited SetTextStr(Value);
  finally
    EnableChange(True);
  end;
end;

function AdjustLineBreaks(Dest, Source: PChar): Integer; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     EDI,EAX
        MOV     ESI,EDX
        MOV     EDX,EAX
        CLD
@@1:    LODSB
@@2:    OR      AL,AL
        JE      @@4
        CMP     AL,0AH
        JE      @@3
        STOSB
        CMP     AL,0DH
        JNE     @@1
        MOV     AL,0AH
        STOSB
        LODSB
        CMP     AL,0AH
        JE      @@1
        JMP     @@2
@@3:    MOV     EAX,0A0DH
        STOSW
        JMP     @@1
@@4:    STOSB
        LEA     EAX,[EDI-1]
        SUB     EAX,EDX
        POP     EDI
        POP     ESI
end;

function StreamSave(dwCookie: Longint; pbBuff: PByte;
  cb: Longint; var pcb: Longint): Longint; stdcall;
var
  StreamInfo: PRichEditStreamInfo;
begin
  Result := NoError;
  StreamInfo := PRichEditStreamInfo(Pointer(dwCookie));
  try
    pcb := 0;
    if StreamInfo^.Converter <> nil then
      pcb := StreamInfo^.Converter.ConvertWriteStream(StreamInfo^.Stream, PChar(pbBuff), cb);
  except
    Result := WriteError;
  end;
end;

function StreamLoad(dwCookie: Longint; pbBuff: PByte;
  cb: Longint; var pcb: Longint): Longint; stdcall;
var
  Buffer, pBuff: PChar;
  StreamInfo: PRichEditStreamInfo;
begin
  Result := NoError;
  StreamInfo := PRichEditStreamInfo(Pointer(dwCookie));
  Buffer := StrAlloc(cb + 1);
  try
    cb := cb div 2;
    pcb := 0;
    pBuff := Buffer + cb;
    try
      if StreamInfo^.Converter <> nil then
        pcb := StreamInfo^.Converter.ConvertReadStream(StreamInfo^.Stream, pBuff, cb);
      if pcb > 0 then
      begin
        pBuff[pcb] := #0;
        if pBuff[pcb - 1] = #13 then pBuff[pcb - 1] := #0;
        pcb := AdjustLineBreaks(Buffer, pBuff);
        Move(Buffer^, pbBuff^, pcb);
      end;
    except
      Result := ReadError;
    end;
  finally
    StrDispose(Buffer);
  end;
end;

procedure TJvRichEditStrings.LoadFromStream(Stream: TStream);
var
  EditStream: TEditStream;
  Position: Longint;
  TextType: Longint;
  StreamInfo: TRichEditStreamInfo;
  Converter: TConversion;
begin
  StreamInfo.Stream := Stream;
  if FConverter <> nil then Converter := FConverter
  else Converter := RichEdit.DefaulTJvConverter.Create;
  StreamInfo.Converter := Converter;
  try
    with EditStream do
    begin
      dwCookie := Longint(Pointer(@StreamInfo));
      pfnCallBack := @StreamLoad;
      dwError := 0;
    end;
    Position := Stream.Position;
    case FFormat of
      sfDefault:
        if RichEdit.PlainText then TextType := SF_TEXT
        else TextType := SF_RTF;
      sfRichText: TextType := SF_RTF;
      else {sfPlainText} TextType := SF_TEXT;
    end;
    if TextType = SF_RTF then begin
      if smPlainRtf in Mode then TextType := TextType or SFF_PLAINRTF;
    end;
    if TextType = SF_TEXT then begin
      if (smUnicode in Mode) and (RichEditVersion > 1) then
        TextType := TextType or SF_UNICODE;
    end;
    if smSelection in Mode then TextType := TextType or SFF_SELECTION;
    SendMessage(RichEdit.Handle, EM_STREAMIN, TextType, Longint(@EditStream));
    if (EditStream.dwError <> 0) then begin
      Stream.Position := Position;
      if (TextType and SF_RTF = SF_RTF) then TextType := SF_TEXT
      else TextType := SF_RTF;
      SendMessage(RichEdit.Handle, EM_STREAMIN, TextType, Longint(@EditStream));
      if EditStream.dwError <> 0 then
        raise EOutOfResources.Create(ResStr(sRichEditLoadFail));
    end;
    RichEdit.SetSelection(0, 0, True);
  finally
    if FConverter = nil then Converter.Free;
  end;
end;

procedure TJvRichEditStrings.SaveToStream(Stream: TStream);
var
  EditStream: TEditStream;
  TextType: Longint;
  StreamInfo: TRichEditStreamInfo;
  Converter: TConversion;
begin
  if FConverter <> nil then Converter := FConverter
  else Converter := RichEdit.DefaulTJvConverter.Create;
  StreamInfo.Stream := Stream;
  StreamInfo.Converter := Converter;
  try
    with EditStream do
    begin
      dwCookie := Longint(Pointer(@StreamInfo));
      pfnCallBack := @StreamSave;
      dwError := 0;
    end;
    case FFormat of
      sfDefault:
        if RichEdit.PlainText then TextType := SF_TEXT
        else TextType := SF_RTF;
      sfRichText: TextType := SF_RTF;
      else {sfPlainText} TextType := SF_TEXT;
    end;
    if TextType = SF_RTF then begin
      if smNoObjects in Mode then TextType := SF_RTFNOOBJS;
      if smPlainRtf in Mode then TextType := TextType or SFF_PLAINRTF;
    end
    else if TextType = SF_TEXT then begin
      if (smUnicode in Mode) and (RichEditVersion > 1) then
        TextType := TextType or SF_UNICODE;
    end;
    if smSelection in Mode then TextType := TextType or SFF_SELECTION;
    SendMessage(RichEdit.Handle, EM_STREAMOUT, TextType, Longint(@EditStream));
    if EditStream.dwError <> 0 then
      raise EOutOfResources.Create(ResStr(sRichEditSaveFail));
  finally
    if FConverter = nil then Converter.Free;
  end;
end;

procedure TJvRichEditStrings.LoadFromFile(const FileName: string);
var
  Ext: string;
  Convert: PRichConversionFormat;
  SaveFormat: TRichStreamFormat;
begin
{$IFNDEF VER90}
  Ext := AnsiLowerCaseFileName(ExtractFileExt(Filename));
{$ELSE}
  Ext := LowerCase(ExtractFileExt(Filename));
{$ENDIF}
  System.Delete(Ext, 1, 1);
  Convert := ConversionFormatList;
  while Convert <> nil do
    with Convert^ do
      if Extension <> Ext then Convert := Next
      else Break;
  if (FConverter = nil) and (Convert <> nil) then
    FConverter := Convert^.ConversionClass.Create;
  try
    SaveFormat := Format;
    try
      if Convert <> nil then begin
        if Convert^.PlainText then FFormat := sfPlainText
        else FFormat := sfRichText;
      end;
      inherited LoadFromFile(FileName);
    finally
      FFormat := SaveFormat;
    end;
  except
    FConverter.Free;
    FConverter := nil;
    raise;
  end;
end;

procedure TJvRichEditStrings.SaveToFile(const FileName: string);
var
  Ext: string;
  Convert: PRichConversionFormat;
  SaveFormat: TRichStreamFormat;
begin
{$IFNDEF VER90}
  Ext := AnsiLowerCaseFileName(ExtractFileExt(Filename));
{$ELSE}
  Ext := LowerCase(ExtractFileExt(Filename));
{$ENDIF}
  System.Delete(Ext, 1, 1);
  Convert := ConversionFormatList;
  while Convert <> nil do
    with Convert^ do
      if Extension <> Ext then Convert := Next
      else Break;
  if (FConverter = nil) and (Convert <> nil) then
    FConverter := Convert^.ConversionClass.Create;
  try
    SaveFormat := Format;
    try
      if Convert <> nil then begin
        if Convert^.PlainText then FFormat := sfPlainText
        else FFormat := sfRichText;
      end;
      inherited SaveToFile(FileName);
    finally
      FFormat := SaveFormat;
    end;
  except
    FConverter.Free;
    FConverter := nil;
    raise;
  end;
end;

{ TJvOEMConversion }

function TJvOEMConversion.ConvertReadStream(Stream: TStream; Buffer: PChar;
  BufSize: Integer): Integer;
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    Mem.SetSize(BufSize);
    Result := inherited ConvertReadStream(Stream, PChar(Mem.Memory), BufSize);
    OemToCharBuff(PChar(Mem.Memory), Buffer, Result);
  finally
    Mem.Free;
  end;
end;

function TJvOEMConversion.ConvertWriteStream(Stream: TStream; Buffer: PChar;
  BufSize: Integer): Integer;
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    Mem.SetSize(BufSize);
    CharToOemBuff(Buffer, PChar(Mem.Memory), BufSize);
    Result := inherited ConvertWriteStream(Stream, PChar(Mem.Memory), BufSize);
  finally
    Mem.Free;
  end;
end;

{ TJvCustomRichEdit }

constructor TJvCustomRichEdit.Create(AOwner: TComponent);
var
  DC: HDC;
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  FSelAttributes := TJvTextAttributes.Create(Self, atSelected);
  FDefAttributes := TJvTextAttributes.Create(Self, atDefaultText);
  FWordAttributes := TJvTextAttributes.Create(Self, atWord);
  FParagraph := TJvParaAttributes.Create(Self);
  FRichEditStrings := TJvRichEditStrings.Create;
  TJvRichEditStrings(FRichEditStrings).RichEdit := Self;
  TabStop := True;
  Width := 185;
  Height := 89;
  AutoSize := False;
{$IFDEF COMPILER4_UP}
  DoubleBuffered := False;
{$ENDIF}
  FAllowObjects := True;
{$IFDEF COMPILER3_UP}
  FAllowInPlace := True;
{$ENDIF}
  FAutoVerbMenu := True;
  FHideSelection := True;
  FHideScrollBars := True;
  ScrollBars := ssBoth;
  FSelectionBar := True;
  FLangOptions := [rlAutoFont];
  DC := GetDC(0);
  FScreenLogPixels := GetDeviceCaps(DC, LOGPIXELSY);
  ReleaseDC(0, DC);
  DefaulTJvConverter := TConversion;
  FOldParaAlignment := TParaAlignment(Alignment);
  FUndoLimit := 100;
  FAutoURLDetect := True;
  FWordSelection := True;
  with FClickRange do begin
    cpMin := -1;
    cpMax := -1;
  end;
  FCallback := TRichEditOleCallback.Create(Self);
{$IFDEF COMPILER4_UP}
  Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
{$ENDIF}
end;

destructor TJvCustomRichEdit.Destroy;
begin
  FLastFind := nil;
  FSelAttributes.Free;
  FDefAttributes.Free;
  FWordAttributes.Free;
  FParagraph.Free;
  FRichEditStrings.Free;
  FMemStream.Free;
  FPopupVerbMenu.Free;
  FFindDialog.Free;
  FReplaceDialog.Free;
  inherited Destroy;
  { be sure that callback object is destroyed after inherited Destroy }
  TRichEditOleCallback(FCallback).Free;
end;

procedure TJvCustomRichEdit.Clear;
begin
  CloseObjects;
  inherited Clear;
  Modified := False;
end;

procedure TJvCustomRichEdit.CreateParams(var Params: TCreateParams);
const
  HideScrollBars: array[Boolean] of DWORD = (ES_DISABLENOSCROLL, 0);
  HideSelections: array[Boolean] of DWORD = (ES_NOHIDESEL, 0);
  WordWraps: array[Boolean] of DWORD = (0, ES_AUTOHSCROLL);
  SelectionBars: array[Boolean] of DWORD = (0, ES_SELECTIONBAR);
begin
  inherited CreateParams(Params);
  case RichEditVersion of
    1: CreateSubClass(Params, RICHEDIT_CLASS10A);
    else CreateSubClass(Params, RICHEDIT_CLASS);
  end;
  with Params do begin
    Style := (Style and not (WS_HSCROLL or WS_VSCROLL)) or ES_SAVESEL or
      (WS_CLIPSIBLINGS or WS_CLIPCHILDREN);
    { NOTE: WS_CLIPCHILDREN and WS_CLIPSIBLINGS are essential otherwise }
    { once the object is inserted you see some painting problems.       }
    Style := Style and not (WS_HSCROLL or WS_VSCROLL);
    if ScrollBars in [ssVertical, ssBoth] then
      Style := Style or WS_VSCROLL;
    if (ScrollBars in [ssHorizontal, ssBoth]) and not WordWrap then
      Style := Style or WS_HSCROLL;
    Style := Style or HideScrollBars[FHideScrollBars] or
      SelectionBars[FSelectionBar] or HideSelections[FHideSelection] and
      not WordWraps[WordWrap];
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TJvCustomRichEdit.CreateWnd;
var
  StreamFmt: TRichStreamFormat;
  Mode: TRichStreamModes;
  DesignMode: Boolean;
  Mask: Longint;
begin
  StreamFmt := TJvRichEditStrings(Lines).Format;
  Mode := TJvRichEditStrings(Lines).Mode;
  inherited CreateWnd;
{$IFNDEF VER90}
  if (SysLocale.FarEast) and not (SysLocale.PriLangID = LANG_JAPANESE) then
    Font.Charset := GetDefFontCharSet;
{$ENDIF}
  Mask := ENM_CHANGE or ENM_SELCHANGE or ENM_REQUESTRESIZE or ENM_PROTECTED;
  if RichEditVersion >= 2 then Mask := Mask or ENM_LINK;
  SendMessage(Handle, EM_SETEVENTMASK, 0, Mask);
  SendMessage(Handle, EM_SETBKGNDCOLOR, 0, ColorToRGB(Color));
{$IFDEF COMPILER3_UP}
  DoSetMaxLength(MaxLength);
{$ENDIF}
  SetWordSelection(FWordSelection);
  if RichEditVersion >= 2 then begin
    SendMessage(Handle, EM_AUTOURLDETECT, Longint(FAutoURLDetect), 0);
    FUndoLimit := SendMessage(Handle, EM_SETUNDOLIMIT, FUndoLimit, 0);
    UpdateTextModes(PlainText);
    SetLangOptions(FLangOptions);
  end;
  if FAllowObjects then begin
    SendMessage(Handle, EM_SETOLECALLBACK, 0,
      LPARAM(TRichEditOleCallback(FCallback) as IRichEditOleCallback));
    GetRichEditOle(Handle, FRichEditOle);
    UpdateHostNames;
  end;
  if FMemStream <> nil then begin
    FMemStream.ReadBuffer(DesignMode, SizeOf(DesignMode));
    if DesignMode then begin
      TJvRichEditStrings(Lines).Format := sfPlainText;
      TJvRichEditStrings(Lines).Mode := [];
    end;
    try
      Lines.LoadFromStream(FMemStream);
      FMemStream.Free;
      FMemStream := nil;
    finally
      TJvRichEditStrings(Lines).Format := StreamFmt;
      TJvRichEditStrings(Lines).Mode := Mode;
    end;
  end;
  if RichEditVersion < 2 then
    SendMessage(Handle, WM_SETFONT, 0, 0);
  Modified := FModified;
end;

procedure TJvCustomRichEdit.DestroyWnd;
var
  StreamFmt: TRichStreamFormat;
  Mode: TRichStreamModes;
  DesignMode: Boolean;
begin
  FModified := Modified;
  FMemStream := TMemoryStream.Create;
  StreamFmt := TJvRichEditStrings(Lines).Format;
  Mode := TJvRichEditStrings(Lines).Mode;
  DesignMode := (csDesigning in ComponentState);
  FMemStream.WriteBuffer(DesignMode, SizeOf(DesignMode));
  if DesignMode then begin
    TJvRichEditStrings(Lines).Format := sfPlainText;
    TJvRichEditStrings(Lines).Mode := [];
  end;
  try
    Lines.SaveToStream(FMemStream);
    FMemStream.Position := 0;
  finally
    TJvRichEditStrings(Lines).Format := StreamFmt;
    TJvRichEditStrings(Lines).Mode := Mode;
  end;
  inherited DestroyWnd;
end;

procedure TJvCustomRichEdit.SetAllowObjects(Value: Boolean);
begin
  if FAllowObjects <> Value then begin
    FAllowObjects := Value;
    RecreateWnd;    
  end;
end;

procedure TJvCustomRichEdit.UpdateHostNames;
var
  AppName: string;
begin
  if HandleAllocated and Assigned(FRichEditOle) then begin
    AppName := Application.Title;
    if Trim(AppName) = '' then
      AppName := ExtractFileName(Application.ExeName);
    if Trim(Title) = '' then
      IRichEditOle(FRichEditOle).SetHostNames(PChar(AppName), PChar(AppName))
    else
      IRichEditOle(FRichEditOle).SetHostNames(PChar(AppName), PChar(Title));
  end;
end;

procedure TJvCustomRichEdit.SetTitle(const Value: string);
begin
  if FTitle <> Value then begin
    FTitle := Value;
    UpdateHostNames;
  end;
end;

function TJvCustomRichEdit.GetPopupMenu: TPopupMenu;
var
  EnumOleVerb: IEnumOleVerb;
  OleVerb: TOleVerb;
  Item: TMenuItem;
  ReObject: TReObject;
begin
  FPopupVerbMenu.Free;
  FPopupVerbMenu := nil;
  Result := inherited GetPopupMenu;
  if FAutoVerbMenu and (SelectionType = [stObject]) and
    Assigned(FRichEditOle) then
  begin
    FillChar(ReObject, SizeOf(ReObject), 0);
    ReObject.cbStruct := SizeOf(ReObject);
    if Succeeded(IRichEditOle(FRichEditOle).GetObject(
      Longint(REO_IOB_SELECTION), ReObject, REO_GETOBJ_POLEOBJ)) then
    try
      if Assigned(ReObject.poleobj) and
        (ReObject.dwFlags and REO_INPLACEACTIVE = 0) then
      begin
        FPopupVerbMenu := TPopupMenu.Create(Self);
        if ReObject.poleobj.EnumVerbs(EnumOleVerb) = 0 then
        try
          while (EnumOleVerb.Next(1, OleVerb, nil) = 0) and
            (OleVerb.lVerb >= 0) and
            (OleVerb.grfAttribs and OLEVERBATTRIB_ONCONTAINERMENU <> 0) do
          begin
            Item := TMenuItem.Create(FPopupVerbMenu);
            Item.Caption := WideCharToString(OleVerb.lpszVerbName);
            Item.Tag := OleVerb.lVerb;
            Item.Default := (OleVerb.lVerb = OLEIVERB_PRIMARY);
            Item.OnClick := PopupVerbClick;
            FPopupVerbMenu.Items.Add(Item);
          end;
        finally
          ReleaseObject(EnumOleVerb);
        end;
        if (Result <> nil) and (Result.Items.Count > 0) then begin
          Item := TMenuItem.Create(FPopupVerbMenu);
          Item.Caption := '-';
          Result.Items.Add(Item);
          Item := TMenuItem.Create(FPopupVerbMenu);
          Item.Caption := Format(ResStr(SPropDlgCaption),
            [GetFullNameStr(ReObject.poleobj)]);
          Item.OnClick := ObjectPropsClick;
          Result.Items.Add(Item);
          if FPopupVerbMenu.Items.Count > 0 then begin
            FPopupVerbMenu.Items.Caption := GetFullNameStr(ReObject.poleobj);
            Result.Items.Add(FPopupVerbMenu.Items);
          end;
        end
        else if FPopupVerbMenu.Items.Count > 0 then begin
          Item := TMenuItem.Create(FPopupVerbMenu);
          Item.Caption := Format(ResStr(SPropDlgCaption),
            [GetFullNameStr(ReObject.poleobj)]);
          Item.OnClick := ObjectPropsClick;
          FPopupVerbMenu.Items.Insert(0, Item);
          Result := FPopupVerbMenu;
        end;
      end;
    finally
      ReleaseObject(ReObject.poleobj);
    end;
  end;
end;

procedure TJvCustomRichEdit.PopupVerbClick(Sender: TObject);
var
  ReObject: TReObject;
begin
  if Assigned(FRichEditOle) then begin
    FillChar(ReObject, SizeOf(ReObject), 0);
    ReObject.cbStruct := SizeOf(ReObject);
    if Succeeded(IRichEditOle(FRichEditOle).GetObject(
      Longint(REO_IOB_SELECTION), ReObject, REO_GETOBJ_POLEOBJ or
      REO_GETOBJ_POLESITE)) then
    try
      if ReObject.dwFlags and REO_INPLACEACTIVE = 0 then
        OleCheck(ReObject.poleobj.DoVerb((Sender as TMenuItem).Tag, nil,
          ReObject.polesite, 0, Handle, ClientRect));
    finally
      ReleaseObject(ReObject.polesite);
      ReleaseObject(ReObject.poleobj);
    end;
  end;
end;

procedure TJvCustomRichEdit.ObjectPropsClick(Sender: TObject);
begin
  ObjectPropertiesDialog;
end;

procedure TJvCustomRichEdit.WMSetFont(var Message: TWMSetFont);
begin
  FDefAttributes.Assign(Font);
end;

procedure TJvCustomRichEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  FDefAttributes.Assign(Font);
end;

procedure TJvCustomRichEdit.CreateWindowHandle(const Params: TCreateParams);
var
  Bounds: TRect;
begin
  Bounds := BoundsRect;
  inherited CreateWindowHandle(Params);
  if HandleAllocated then BoundsRect := Bounds;
end;

{$IFDEF COMPILER3_UP}
procedure TJvCustomRichEdit.DoSetMaxLength(Value: Integer);
begin
  { The rich edit control's default maximum amount of text is 32K }
  { Let's set it at 16M by default }
  if Value = 0 then Value := $FFFFFF;
  SendMessage(Handle, EM_EXLIMITTEXT, 0, Value);
end;
{$ENDIF}

function TJvCustomRichEdit.GetCaretPos: TPoint;
var
  CharRange: TCharRange;
begin
  SendMessage(Handle, EM_EXGETSEL, 0, Longint(@CharRange));
  Result.X := CharRange.cpMax;
  Result.Y := LineFromChar(Result.X);
  Dec(Result.X, GetLineIndex(-1));
end;

{$IFDEF COMPILER3_UP}
function TJvCustomRichEdit.GetSelLength: Integer;
begin
  with GetSelection do
    Result := cpMax - cpMin;
end;

function TJvCustomRichEdit.GetSelStart: Integer;
begin
  Result := GetSelection.cpMin;
end;

function TJvCustomRichEdit.GetSelText: string;
begin
  with GetSelection do
    Result := GetTextRange(cpMin, cpMax);
end;
{$ENDIF COMPILER3_UP}

function TJvCustomRichEdit.GetSelTextBuf(Buffer: PChar; BufSize: Integer): Integer;
var
  S: string;
begin
  S := SelText;
  Result := Length(S);
  if BufSize < Length(S) then Result := BufSize;
  StrPLCopy(Buffer, S, Result);
end;

{$IFDEF COMPILER4_UP}
procedure TJvCustomRichEdit.CMBiDiModeChanged(var Message: TMessage);
var
  AParagraph: TParaFormat2;
begin
  HandleNeeded; { we REALLY need the handle for BiDi }
  inherited;
  Paragraph.GetAttributes(AParagraph);
  AParagraph.dwMask := PFM_ALIGNMENT;
  AParagraph.wAlignment := Ord(Alignment) + 1;
  Paragraph.SetAttributes(AParagraph);
end;
{$ENDIF}

procedure TJvCustomRichEdit.SetHideScrollBars(Value: Boolean);
begin
  if HideScrollBars <> Value then begin
    FHideScrollBars := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomRichEdit.SetSelectionBar(Value: Boolean);
begin
  if FSelectionBar <> Value then begin
    FSelectionBar := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomRichEdit.SetHideSelection(Value: Boolean);
begin
  if HideSelection <> Value then begin
    FHideSelection := Value;
    SendMessage(Handle, EM_HIDESELECTION, Ord(HideSelection), LPARAM(True));
  end;
end;

function TJvCustomRichEdit.GetAutoURLDetect: Boolean;
begin
  Result := FAutoURLDetect;
  if HandleAllocated and not (csDesigning in ComponentState) then begin
    if RichEditVersion >= 2 then
      Result := Boolean(SendMessage(Handle, EM_GETAUTOURLDETECT, 0, 0));
  end;
end;

procedure TJvCustomRichEdit.SetAutoURLDetect(Value: Boolean);
begin
  if Value <> FAutoURLDetect then begin
    FAutoURLDetect := Value;
    if HandleAllocated and (RichEditVersion >= 2) then
      SendMessage(Handle, EM_AUTOURLDETECT, Longint(FAutoURLDetect), 0);
  end;
end;

function TJvCustomRichEdit.GetWordSelection: Boolean;
begin
  Result := FWordSelection;
  if HandleAllocated then
    Result := (SendMessage(Handle, EM_GETOPTIONS, 0, 0) and
      ECO_AUTOWORDSELECTION) <> 0;
end;

procedure TJvCustomRichEdit.SetWordSelection(Value: Boolean);
var
  Options: LPARAM;
begin
  FWordSelection := Value;
  if HandleAllocated then begin
    Options := SendMessage(Handle, EM_GETOPTIONS, 0, 0);
    if Value then Options := Options or ECO_AUTOWORDSELECTION
    else Options := Options and not ECO_AUTOWORDSELECTION;
    SendMessage(Handle, EM_SETOPTIONS, ECOOP_SET, Options);
  end;
end;

const
  RichLangOptions: array[TRichLangOption] of DWORD = (IMF_AUTOKEYBOARD,
    IMF_AUTOFONT, IMF_IMECANCELCOMPLETE, IMF_IMEALWAYSSENDNOTIFY);

function TJvCustomRichEdit.GetLangOptions: TRichLangOptions;
var
  Flags: Longint;
  I: TRichLangOption;
begin
  Result := FLangOptions;
  if HandleAllocated and not (csDesigning in ComponentState) and
    (RichEditVersion >= 2) then
  begin
    Result := [];
    Flags := SendMessage(Handle, EM_GETLANGOPTIONS, 0, 0);
    for I := Low(TRichLangOption) to High(TRichLangOption) do
      if Flags and RichLangOptions[I] <> 0 then Include(Result, I);
  end;
end;

procedure TJvCustomRichEdit.SetLangOptions(Value: TRichLangOptions);
var
  Flags: DWORD;
  I: TRichLangOption;
begin
  FLangOptions := Value;
  if HandleAllocated and (RichEditVersion >= 2) then begin
    Flags := 0;
    for I := Low(TRichLangOption) to High(TRichLangOption) do
      if I in Value then Flags := Flags or RichLangOptions[I];
    SendMessage(Handle, EM_SETLANGOPTIONS, 0, LPARAM(Flags));
  end;
end;

procedure TJvCustomRichEdit.SetSelAttributes(Value: TJvTextAttributes);
begin
  FSelAttributes.Assign(Value);
end;

function TJvCustomRichEdit.GetCanRedo: Boolean;
begin
  Result := False;
  if HandleAllocated and (RichEditVersion >= 2) then
    Result := SendMessage(Handle, EM_CANREDO, 0, 0) <> 0;
end;

function TJvCustomRichEdit.GetCanPaste: Boolean;
begin
  Result := False;
  if HandleAllocated then
    Result := SendMessage(Handle, EM_CANPASTE, 0, 0) <> 0;
end;

{$IFNDEF COMPILER35_UP}
function TJvCustomRichEdit.GetCanUndo: Boolean;
begin
  Result := False;
  if HandleAllocated then
    Result := SendMessage(Handle, EM_CANUNDO, 0, 0) <> 0;
end;
{$ENDIF}

function TJvCustomRichEdit.GetRedoName: TUndoName;
begin
  Result := unUnknown;
  if (RichEditVersion >= 2) and HandleAllocated then
    Result := TUndoName(SendMessage(Handle, EM_GETREDONAME, 0, 0));
end;

function TJvCustomRichEdit.GetUndoName: TUndoName;
begin
  Result := unUnknown;
  if (RichEditVersion >= 2) and HandleAllocated then
    Result := TUndoName(SendMessage(Handle, EM_GETUNDONAME, 0, 0));
end;

function TJvCustomRichEdit.GetSelectionType: TRichSelectionType;
const
  SelTypes: array[TRichSelection] of Integer = (
    SEL_TEXT, SEL_OBJECT, SEL_MULTICHAR, SEL_MULTIOBJECT);
var
  Selection: Integer;
  I: TRichSelection;
begin
  Result := [];
  if HandleAllocated then begin
    Selection := SendMessage(Handle, EM_SELECTIONTYPE, 0, 0);
    for I := Low(TRichSelection) to High(TRichSelection) do
      if SelTypes[I] and Selection <> 0 then Include(Result, I);
  end;
end;

function TJvCustomRichEdit.GetSelection: TCharRange;
begin
  SendMessage(Handle, EM_EXGETSEL, 0, Longint(@Result));
end;

procedure TJvCustomRichEdit.SetSelection(StartPos, EndPos: Longint;
  ScrollCaret: Boolean);
var
  CharRange: TCharRange;
begin
  with CharRange do begin
    cpMin := StartPos;
    cpMax := EndPos;
  end;
  SendMessage(Handle, EM_EXSETSEL, 0, Longint(@CharRange));
  if ScrollCaret then SendMessage(Handle, EM_SCROLLCARET, 0, 0);
end;

{$IFDEF COMPILER3_UP}
procedure TJvCustomRichEdit.SetSelLength(Value: Integer);
begin
  with GetSelection do SetSelection(cpMin, cpMin + Value, True);
end;

procedure TJvCustomRichEdit.SetSelStart(Value: Integer);
begin
  SetSelection(Value, Value, False);
end;
{$ENDIF COMPILER3_UP}

function TJvCustomRichEdit.GetCharPos(CharIndex: Integer): TPoint;
var
  Res: Longint;
begin
  Result.X := 0;
  Result.Y := 0;
//  FillChar(Result, SizeOf(Result), 0);
  if HandleAllocated then begin
    if RichEditVersion = 2 then begin
      Res := SendMessage(Handle, Messages.EM_POSFROMCHAR, CharIndex, 0);
      Result.X := LoWord(Res);
      Result.Y := HiWord(Res);
    end
    else { RichEdit 1.0 and 3.0 }
      SendMessage(Handle, Messages.EM_POSFROMCHAR, WPARAM(@Result), CharIndex);
  end;
end;

function TJvCustomRichEdit.GetTextRange(StartPos, EndPos: Longint): string;
var
  TextRange: TTextRange;
begin
  SetLength(Result, EndPos - StartPos + 1);
  TextRange.chrg.cpMin := StartPos;
  TextRange.chrg.cpMax := EndPos;
  TextRange.lpstrText := PAnsiChar(Result);
  SetLength(Result, SendMessage(Handle, EM_GETTEXTRANGE, 0, Longint(@TextRange)));
end;

function TJvCustomRichEdit.WordAtCursor: string;
var
  Range: TCharRange;
begin
  Result := '';
  if HandleAllocated then begin
    Range.cpMax := SelStart;
    if Range.cpMax = 0 then Range.cpMin := 0
    else if SendMessage(Handle, EM_FINDWORDBREAK, WB_ISDELIMITER, Range.cpMax) <> 0 then
      Range.cpMin := SendMessage(Handle, EM_FINDWORDBREAK, WB_MOVEWORDLEFT, Range.cpMax)
    else
      Range.cpMin := SendMessage(Handle, EM_FINDWORDBREAK, WB_LEFT, Range.cpMax);
    while SendMessage(Handle, EM_FINDWORDBREAK, WB_ISDELIMITER, Range.cpMin) <> 0 do
      Inc(Range.cpMin);
    Range.cpMax := SendMessage(Handle, EM_FINDWORDBREAK, WB_RIGHTBREAK, Range.cpMax);
    Result := Trim(GetTextRange(Range.cpMin, Range.cpMax));
  end;
end;

function TJvCustomRichEdit.LineFromChar(CharIndex: Integer): Integer;
begin
  Result := SendMessage(Handle, EM_EXLINEFROMCHAR, 0, CharIndex);
end;

function TJvCustomRichEdit.GetLineIndex(LineNo: Integer): Integer;
begin
  Result := SendMessage(Handle, EM_LINEINDEX, LineNo, 0);
end;

function TJvCustomRichEdit.GetLineLength(CharIndex: Integer): Integer;
begin
  Result := SendMessage(Handle, EM_LINELENGTH, CharIndex, 0);
end;

procedure TJvCustomRichEdit.SetUndoLimit(Value: Integer);
begin
  if (Value <> FUndoLimit) then begin
    FUndoLimit := Value;
    if (RichEditVersion >= 2) and HandleAllocated then
      FUndoLimit := SendMessage(Handle, EM_SETUNDOLIMIT, Value, 0);
  end;
end;

procedure TJvCustomRichEdit.SetDefAttributes(Value: TJvTextAttributes);
begin
  FDefAttributes.Assign(Value);
end;

procedure TJvCustomRichEdit.SetWordAttributes(Value: TJvTextAttributes);
begin
  FWordAttributes.Assign(Value);
end;

function TJvCustomRichEdit.GetStreamFormat: TRichStreamFormat;
begin
  Result := TJvRichEditStrings(Lines).Format;
end;

function TJvCustomRichEdit.GetStreamMode: TRichStreamModes;
begin
  Result := TJvRichEditStrings(Lines).Mode;
end;

procedure TJvCustomRichEdit.SetStreamFormat(Value: TRichStreamFormat);
begin
  TJvRichEditStrings(Lines).Format := Value;
end;

procedure TJvCustomRichEdit.SetStreamMode(Value: TRichStreamModes);
begin
  TJvRichEditStrings(Lines).Mode := Value;
end;

procedure TJvCustomRichEdit.SetPlainText(Value: Boolean);
var
  MemStream: TStream;
  StreamFmt: TRichStreamFormat;
  Mode: TRichStreamModes;
begin
  if PlainText <> Value then begin
    if HandleAllocated and (RichEditVersion >= 2) then begin
      MemStream := TMemoryStream.Create;
      try
        StreamFmt := TJvRichEditStrings(Lines).Format;
        Mode := TJvRichEditStrings(Lines).Mode;
        try
          if (csDesigning in ComponentState) or Value then
            TJvRichEditStrings(Lines).Format := sfPlainText
          else TJvRichEditStrings(Lines).Format := sfRichText;
          TJvRichEditStrings(Lines).Mode := [];
          Lines.SaveToStream(MemStream);
          MemStream.Position := 0;
          TJvRichEditStrings(Lines).EnableChange(False);
          try
            SendMessage(Handle, WM_SETTEXT, 0, 0);
            UpdateTextModes(Value);
            FPlainText := Value;
          finally
            TJvRichEditStrings(Lines).EnableChange(True);
          end;
          Lines.LoadFromStream(MemStream);
        finally
          TJvRichEditStrings(Lines).Format := StreamFmt;
          TJvRichEditStrings(Lines).Mode := Mode;
        end;
      finally
        MemStream.Free;
      end;
    end;
    FPlainText := Value;
  end;
end;

procedure TJvCustomRichEdit.UpdateTextModes(Plain: Boolean);
const
  TextModes: array[Boolean] of DWORD = (TM_RICHTEXT, TM_PLAINTEXT);
  UndoModes: array[Boolean] of DWORD = (TM_SINGLELEVELUNDO, TM_MULTILEVELUNDO);
begin
  if (RichEditVersion >= 2) and HandleAllocated then begin
    SendMessage(Handle, EM_SETTEXTMODE, TextModes[Plain] or
      UndoModes[FUndoLimit > 1], 0);
  end;
end;

procedure TJvCustomRichEdit.CMColorChanged(var Message: TMessage);
begin
  inherited;
  SendMessage(Handle, EM_SETBKGNDCOLOR, 0, ColorToRGB(Color))
end;

procedure TJvCustomRichEdit.EMReplaceSel(var Message: TMessage);
var
  CharRange: TCharRange;
begin
  Perform(EM_EXGETSEL, 0, Longint(@CharRange));
  with CharRange do
    cpMax := cpMin + Integer(StrLen(PChar(Message.lParam)));
  if (FUndoLimit > 1) and (RichEditVersion >= 2) and not FLinesUpdating then
    Message.wParam := 1; { allow Undo }
  inherited;
  if not FLinesUpdating then begin
    Perform(EM_EXSETSEL, 0, Longint(@CharRange));
    Perform(EM_SCROLLCARET, 0, 0);
  end;
end;

procedure TJvCustomRichEdit.SetRichEditStrings(Value: TStrings);
begin
  FRichEditStrings.Assign(Value);
end;

procedure TJvCustomRichEdit.CloseObjects;
var
  I: Integer;
  ReObject: TReObject;
begin
  if Assigned(FRichEditOle) then begin
    FillChar(ReObject, SizeOf(ReObject), 0);
    ReObject.cbStruct := SizeOf(ReObject);
    with IRichEditOle(FRichEditOle) do begin
      for I := GetObjectCount - 1 downto 0 do
        if Succeeded(GetObject(I, ReObject, REO_GETOBJ_POLEOBJ)) then begin
          if ReObject.dwFlags and REO_INPLACEACTIVE <> 0 then
            IRichEditOle(FRichEditOle).InPlaceDeactivate;
          ReObject.poleobj.Close(OLECLOSE_NOSAVE);
          ReleaseObject(ReObject.poleobj);
        end;
    end;
  end;
end;

function TJvCustomRichEdit.PasteSpecialDialog: Boolean;

  procedure SetPasteEntry(var Entry: TOleUIPasteEntry; Format: TClipFormat;
    tymed: DWORD; const FormatName, ResultText: string; Flags: DWORD);
  begin
    with Entry do begin
      fmtetc.cfFormat := Format;
      fmtetc.dwAspect := DVASPECT_CONTENT;
      fmtetc.lIndex := -1;
      fmtetc.tymed := tymed;
      if FormatName <> '' then lpstrFormatName := PChar(FormatName)
      else lpstrFormatName := '%s';
      if ResultText <> '' then lpstrResultText := PChar(ResultText)
      else lpstrResultText := '%s';
      dwFlags := Flags;
    end;
  end;

const
  PasteFormatCount = 6;
var
  Data: TOleUIPasteSpecial;
  PasteFormats: array[0..PasteFormatCount - 1] of TOleUIPasteEntry;
  Format: Integer;
  OleClientSite: IOleClientSite;
  Storage: IStorage;
  OleObject: IOleObject;
  ReObject: TReObject;
  Selection: TCharRange;
begin
  Result := False;
  if not CanPaste or not Assigned(FRichEditOle) then Exit;
  FillChar(Data, SizeOf(Data), 0);
  FillChar(PasteFormats, SizeOf(PasteFormats), 0);
  with Data do begin
    cbStruct := SizeOf(Data);
    hWndOwner := Handle;
    arrPasteEntries := @PasteFormats;
    cPasteEntries := PasteFormatCount;
    arrLinkTypes := @CFLinkSource;
    cLinkTypes := 1;
    dwFlags := PSF_SELECTPASTE;
  end;
  SetPasteEntry(PasteFormats[0], CFEmbeddedObject, TYMED_ISTORAGE, '', '',
    OLEUIPASTE_PASTE or OLEUIPASTE_ENABLEICON);
  SetPasteEntry(PasteFormats[1], CFLinkSource, TYMED_ISTREAM, '', '',
    OLEUIPASTE_LINKTYPE1 or OLEUIPASTE_ENABLEICON);
  SetPasteEntry(PasteFormats[2], CFRtf, TYMED_ISTORAGE,
    CF_RTF, CF_RTF, OLEUIPASTE_PASTE);
  SetPasteEntry(PasteFormats[3], CFRtfNoObjs, TYMED_ISTORAGE,
    CF_RTFNOOBJS, CF_RTFNOOBJS, OLEUIPASTE_PASTE);
  SetPasteEntry(PasteFormats[4], CF_TEXT, TYMED_HGLOBAL,
    'Unformatted text', 'text without any formatting', OLEUIPASTE_PASTE);
  SetPasteEntry(PasteFormats[5], CF_BITMAP, TYMED_GDI,
    'Windows Bitmap', 'bitmap image', OLEUIPASTE_PASTE);
  try
    if OleUIPasteSpecial(Data) = OLEUI_OK then begin
      Result := True;
      if Data.nSelectedIndex in [0, 1] then begin
        { CFEmbeddedObject, CFLinkSource }
        FillChar(ReObject, SizeOf(TReObject), 0);
        IRichEditOle(FRichEditOle).GetClientSite(OleClientSite);
        Storage := nil;
        try
          CreateStorage(Storage);
          case Data.nSelectedIndex of
            0: OleCheck(OleCreateFromData(Data.lpSrcDataObj,
               {$IFDEF COMPILER3_UP} IOleObject {$ELSE} IID_IOleObject {$ENDIF},
               OLERENDER_DRAW, nil, OleClientSite, Storage, OleObject));
            1: OleCheck(OleCreateLinkFromData(Data.lpSrcDataObj,
               {$IFDEF COMPILER3_UP} IOleObject {$ELSE} IID_IOleObject {$ENDIF},
               OLERENDER_DRAW, nil, OleClientSite, Storage, OleObject));
          end;
          try
            with ReObject do begin
              cbStruct := SizeOf(TReObject);
              cp := REO_CP_SELECTION;
              poleobj := OleObject;
              OleObject.GetUserClassID(clsid);
              pstg := Storage;
              polesite := OleClientSite;
              dvAspect := DVASPECT_CONTENT;
              dwFlags := REO_RESIZABLE;
              OleCheck(OleSetDrawAspect(OleObject,
                Data.dwFlags and PSF_CHECKDISPLAYASICON <> 0,
                Data.hMetaPict, dvAspect));
            end;
            SendMessage(Handle, EM_EXGETSEL, 0, Longint(@Selection));
            Selection.cpMax := Selection.cpMin + 1;
            OleCheck(IRichEditOle(FRichEditOle).InsertObject(ReObject));
            SendMessage(Handle, EM_EXSETSEL, 0, Longint(@Selection));
            IRichEditOle(FRichEditOle).SetDvaspect(
              Longint(REO_IOB_SELECTION), ReObject.dvAspect);
          finally
            ReleaseObject(OleObject);
          end;
        finally
          ReleaseObject(OleClientSite);
          ReleaseObject(Storage);
        end;
      end
      else begin
        Format := PasteFormats[Data.nSelectedIndex].fmtetc.cfFormat;
        OleCheck(IRichEditOle(FRichEditOle).ImportDataObject(
          Data.lpSrcDataObj, Format, Data.hMetaPict));
      end;
      SendMessage(Handle, EM_SCROLLCARET, 0, 0);
    end;
  finally
    DestroyMetaPict(Data.hMetaPict);
    ReleaseObject(Data.lpSrcDataObj);
  end;
end;

function TJvCustomRichEdit.InsertObjectDialog: Boolean;
var
  Data: TOleUIInsertObject;
  NameBuffer: array[0..255] of Char;
  OleClientSite: IOleClientSite;
  Storage: IStorage;
  OleObject: IOleObject;
  ReObject: TReObject;
  IsNewObject: Boolean;
  Selection: TCharRange;
begin
  FillChar(Data, SizeOf(Data), 0);
  FillChar(NameBuffer, SizeOf(NameBuffer), 0);
  FillChar(ReObject, SizeOf(TReObject), 0);
  if Assigned(FRichEditOle) then begin
    IRichEditOle(FRichEditOle).GetClientSite(OleClientSite);
    Storage := nil;
    try
      CreateStorage(Storage);
      with Data do begin
        cbStruct := SizeOf(Data);
        dwFlags := IOF_SELECTCREATENEW or IOF_VERIFYSERVERSEXIST or 
          IOF_CREATENEWOBJECT or IOF_CREATEFILEOBJECT or IOF_CREATELINKOBJECT;
        hWndOwner := Handle;
        lpszFile := NameBuffer;
        cchFile := SizeOf(NameBuffer);
        iid := {$IFDEF COMPILER3_UP} IOleObject {$ELSE} IID_IOleObject {$ENDIF};
        oleRender := OLERENDER_DRAW;
        lpIOleClientSite := OleClientSite;
        lpIStorage := Storage;
        ppvObj := @OleObject;
      end;
      try
        Result := OleUIInsertObject(Data) = OLEUI_OK;
        if Result then
        try
          IsNewObject := Data.dwFlags and IOF_SELECTCREATENEW = IOF_SELECTCREATENEW;
          with ReObject do begin
            cbStruct := SizeOf(TReObject);
            cp := REO_CP_SELECTION;
            clsid := Data.clsid;
            poleobj := OleObject;
            pstg := Storage;
            polesite := OleClientSite;
            dvAspect := DVASPECT_CONTENT;
            dwFlags := REO_RESIZABLE;
            if IsNewObject then dwFlags := dwFlags or REO_BLANK;
            OleCheck(OleSetDrawAspect(OleObject,
              Data.dwFlags and IOF_CHECKDISPLAYASICON <> 0,
              Data.hMetaPict, dvAspect));
          end;
          SendMessage(Handle, EM_EXGETSEL, 0, Longint(@Selection));
          Selection.cpMax := Selection.cpMin + 1;
          OleCheck(IRichEditOle(FRichEditOle).InsertObject(ReObject));
          SendMessage(Handle, EM_EXSETSEL, 0, Longint(@Selection));
          SendMessage(Handle, EM_SCROLLCARET, 0, 0);
          IRichEditOle(FRichEditOle).SetDvaspect(
            Longint(REO_IOB_SELECTION), ReObject.dvAspect);
          if IsNewObject then OleObject.DoVerb(OLEIVERB_SHOW, nil,
            OleClientSite, 0, Handle, ClientRect);
        finally
          ReleaseObject(OleObject);
        end;
      finally
        DestroyMetaPict(Data.hMetaPict);
      end;
    finally
      ReleaseObject(OleClientSite);
      ReleaseObject(Storage);
    end;
  end
  else Result := False;
end;

function TJvCustomRichEdit.ObjectPropertiesDialog: Boolean;
var
  ObjectProps: TOleUIObjectProps;
  PropSheet: TPropSheetHeader;
  GeneralProps: TOleUIGnrlProps;
  ViewProps: TOleUIViewProps;
  LinkProps: TOleUILinkProps;
  DialogCaption: string;
  ReObject: TReObject;
begin
  Result := False;
  if not Assigned(FRichEditOle) or (SelectionType <> [stObject]) then Exit;
  FillChar(ObjectProps, SizeOf(ObjectProps), 0);
  FillChar(PropSheet, SizeOf(PropSheet), 0);
  FillChar(GeneralProps, SizeOf(GeneralProps), 0);
  FillChar(ViewProps, SizeOf(ViewProps), 0);
  FillChar(LinkProps, SizeOf(LinkProps), 0);
  FillChar(ReObject, SizeOf(ReObject), 0);
  ReObject.cbStruct := SizeOf(ReObject);
  if Succeeded(IRichEditOle(FRichEditOle).GetObject(Longint(REO_IOB_SELECTION),
    ReObject, REO_GETOBJ_POLEOBJ or REO_GETOBJ_POLESITE)) then
  try
    if ReObject.dwFlags and REO_INPLACEACTIVE = 0 then begin
      ObjectProps.cbStruct := SizeOf(ObjectProps);
      ObjectProps.dwFlags := OPF_DISABLECONVERT;
      ObjectProps.lpPS := @PropSheet;
      ObjectProps.lpObjInfo := TOleUIObjInfo.Create(Self, ReObject);
      if (ReObject.dwFlags and REO_LINK) <> 0 then begin
        ObjectProps.dwFlags := ObjectProps.dwFlags or OPF_OBJECTISLINK;
        ObjectProps.lpLinkInfo := TOleUILinkInfo.Create(Self, ReObject);
      end;
      ObjectProps.lpGP := @GeneralProps;
      ObjectProps.lpVP := @ViewProps;
      ObjectProps.lpLP := @LinkProps;
      PropSheet.dwSize := SizeOf(PropSheet);
      PropSheet.hWndParent := Handle;
{$IFDEF COMPILER3_UP}
      PropSheet.hInstance := MainInstance;
{$ELSE}
      PropSheet.hInstance := HInstance;
{$ENDIF}
      DialogCaption := Format(ResStr(SPropDlgCaption),
        [GetFullNameStr(ReObject.poleobj)]);
      PropSheet.pszCaption := PChar(DialogCaption);
      GeneralProps.cbStruct := SizeOf(GeneralProps);
      ViewProps.cbStruct := SizeOf(ViewProps);
      ViewProps.dwFlags := VPF_DISABLESCALE;
      LinkProps.cbStruct := SizeOf(LinkProps);
      LinkProps.dwFlags := ELF_DISABLECANCELLINK;
      Result := OleUIObjectProperties(ObjectProps) = OLEUI_OK;
    end;
  finally
{$IFNDEF COMPILER3_UP}
    ObjectProps.lpLinkInfo.Free;
    ObjectProps.lpObjInfo.Free;
    ReleaseObject(ReObject.polesite);
    ReleaseObject(ReObject.poleobj);
{$ENDIF}
  end;
end;

procedure TJvCustomRichEdit.Print(const Caption: string);
var
  Range: TFormatRange;
  LastChar, MaxLen, LogX, LogY, OldMap: Integer;
  SaveRect: TRect;
  TextLenEx: TGetTextLengthEx;
begin
  FillChar(Range, SizeOf(TFormatRange), 0);
  with Printer, Range do begin
    Title := Caption;
    BeginDoc;
    hdc := Handle;
    hdcTarget := hdc;
    LogX := GetDeviceCaps(Handle, LOGPIXELSX);
    LogY := GetDeviceCaps(Handle, LOGPIXELSY);
    if IsRectEmpty(PageRect) then begin
      rc.right := PageWidth * 1440 div LogX;
      rc.bottom := PageHeight * 1440 div LogY;
    end
    else begin
      rc.left := PageRect.Left * 1440 div LogX;
      rc.top := PageRect.Top * 1440 div LogY;
      rc.right := PageRect.Right * 1440 div LogX;
      rc.bottom := PageRect.Bottom * 1440 div LogY;
    end;
    rcPage := rc;
    SaveRect := rc;
    LastChar := 0;
    if RichEditVersion >= 2 then begin
      with TextLenEx do begin
        flags := GTL_DEFAULT;
        codepage := CP_ACP;
      end;
      MaxLen := Perform(EM_GETTEXTLENGTHEX, WParam(@TextLenEx), 0);
    end
    else MaxLen := GetTextLen;
    chrg.cpMax := -1;
    { ensure printer DC is in text map mode }
    OldMap := SetMapMode(hdc, MM_TEXT);
    SendMessage(Self.Handle, EM_FORMATRANGE, 0, 0);    { flush buffer }
    try
      repeat
        rc := SaveRect;
        chrg.cpMin := LastChar;
        LastChar := SendMessage(Self.Handle, EM_FORMATRANGE, 1, Longint(@Range));
        if (LastChar < MaxLen) and (LastChar <> -1) then NewPage;
      until (LastChar >= MaxLen) or (LastChar = -1);
      EndDoc;
    finally
      SendMessage(Self.Handle, EM_FORMATRANGE, 0, 0);  { flush buffer }
      SetMapMode(hdc, OldMap);       { restore previous map mode }
    end;
  end;
end;

var
  Painting: Boolean = False;

procedure TJvCustomRichEdit.WMPaint(var Message: TWMPaint);
var
  R, R1: TRect;
begin
  if RichEditVersion >= 2 then
    inherited
  else begin
    if GetUpdateRect(Handle, R, True) then
    begin
      with ClientRect do R1 := Rect(Right - 3, Top, Right, Bottom);
      if IntersectRect(R, R, R1) then InvalidateRect(Handle, @R1, True);
    end;
    if Painting then
      Invalidate
    else begin
      Painting := True;
      try
        inherited;
      finally
        Painting := False;
      end;
    end;
  end;
end;

procedure TJvCustomRichEdit.WMDestroy(var Msg: TWMDestroy);
begin
  CloseObjects;
  ReleaseObject(FRichEditOle);
  inherited;
end;

procedure TJvCustomRichEdit.WMMouseMove(var Message: TMessage);
begin
  inherited;
end;

procedure TJvCustomRichEdit.WMSetCursor(var Message: TWMSetCursor);
begin
  inherited;
end;

{$IFDEF COMPILER5_UP}
procedure TJvCustomRichEdit.WMRButtonUp(var Message: TMessage);
begin
  { RichEd20 does not pass the WM_RBUTTONUP message to defwndproc, }
  { so we get no WM_CONTEXTMENU message. Simulate message here.    }
  if Win32MajorVersion < 5 then
    Perform(WM_CONTEXTMENU, Handle, LParam(PointToSmallPoint(
      ClientToScreen(SmallPointToPoint(TWMMouse(Message).Pos)))));
  inherited;
end;
{$ENDIF}

procedure TJvCustomRichEdit.CNNotify(var Message: TWMNotify);
var
  AMsg: TMessage;
begin
  with Message do
    case NMHdr^.code of
      EN_SELCHANGE: SelectionChange;
      EN_REQUESTRESIZE: RequestSize(PReqSize(NMHdr)^.rc);
      EN_SAVECLIPBOARD:
        with PENSaveClipboard(NMHdr)^ do
          if not SaveClipboard(cObjectCount, cch) then Result := 1;
      EN_PROTECTED:
        with PENProtected(NMHdr)^ do begin
          AMsg.Msg := Msg;
          AMsg.WParam := WParam;
          AMsg.LParam := LParam;
          AMsg.Result := 0;
          if not ProtectChange(AMsg, chrg.cpMin, chrg.cpMax) then
            Result := 1;
        end;
      EN_LINK:
        with PENLink(NMHdr)^ do begin
          case Msg of
            WM_RBUTTONDOWN:
              begin
                FClickRange := chrg;
                FClickBtn := mbRight;
              end;
            WM_RBUTTONUP:
              begin
                if (FClickBtn = mbRight) and (FClickRange.cpMin = chrg.cpMin) and
                  (FClickRange.cpMax = chrg.cpMax) then
                  URLClick(GetTextRange(chrg.cpMin, chrg.cpMax), mbRight);
                with FClickRange do begin
                  cpMin := -1;
                  cpMax := -1;
                end;
              end;
            WM_LBUTTONDOWN:
              begin
                FClickRange := chrg;
                FClickBtn := mbLeft;
              end;
            WM_LBUTTONUP:
              begin
                if (FClickBtn = mbLeft) and (FClickRange.cpMin = chrg.cpMin) and
                  (FClickRange.cpMax = chrg.cpMax) then
                  URLClick(GetTextRange(chrg.cpMin, chrg.cpMax), mbLeft);
                with FClickRange do begin
                  cpMin := -1;
                  cpMax := -1;
                end;
              end;
          end;
        end;
      EN_STOPNOUNDO:
        begin
          { cannot allocate enough memory to maintain the undo state }
        end;
    end;
end;

function TJvCustomRichEdit.SaveClipboard(NumObj, NumChars: Integer): Boolean;
begin
  Result := True;
  if Assigned(OnSaveClipboard) then
    OnSaveClipboard(Self, NumObj, NumChars, Result);
end;

function TJvCustomRichEdit.ProtectChange(const Message: TMessage; StartPos,
  EndPos: Integer): Boolean;
begin
  Result := False;
  if Assigned(OnProtectChangeEx) then
    OnProtectChangeEx(Self, Message, StartPos, EndPos, Result)
  else if Assigned(OnProtectChange) then
    OnProtectChange(Self, StartPos, EndPos, Result);
end;

procedure TJvCustomRichEdit.SelectionChange;
begin
  if Assigned(OnSelectionChange) then OnSelectionChange(Self);
end;

procedure TJvCustomRichEdit.RequestSize(const Rect: TRect);
begin
  if Assigned(OnResizeRequest) then OnResizeRequest(Self, Rect);
end;

procedure TJvCustomRichEdit.URLClick(const URLText: string; Button: TMouseButton);
begin
  if Assigned(OnURLClick) then OnURLClick(Self, URLText, Button);
end;

function TJvCustomRichEdit.FindText(const SearchStr: string;
  StartPos, Length: Integer; Options: TRichSearchTypes): Integer;
var
  Find: TFindTextEx;
  Flags: Integer;
begin
  with Find.chrg do begin
    cpMin := StartPos;
    cpMax := cpMin + Abs(Length);
  end;
  if RichEditVersion >= 2 then begin
    if not (stBackward in Options) then Flags := FT_DOWN
    else Flags := 0;
  end
  else begin
    Options := Options - [stBackward];
    Flags := 0;
  end;
  if stWholeWord in Options then Flags := Flags or FT_WHOLEWORD;
  if stMatchCase in Options then Flags := Flags or FT_MATCHCASE;
  Find.lpstrText := PChar(SearchStr);
  Result := SendMessage(Handle, EM_FINDTEXTEX, Flags, Longint(@Find));
  if (Result >= 0) and (stSetSelection in Options) then begin
    SendMessage(Handle, EM_EXSETSEL, 0, Longint(@Find.chrgText));
    SendMessage(Handle, EM_SCROLLCARET, 0, 0);
  end;
end;

procedure TJvCustomRichEdit.ClearUndo;
begin
  SendMessage(Handle, EM_EMPTYUNDOBUFFER, 0, 0);
end;

procedure TJvCustomRichEdit.Redo;
begin
  SendMessage(Handle, EM_REDO, 0, 0);
end;

{$IFNDEF COMPILER35_UP}
procedure TJvCustomRichEdit.Undo;
begin
  SendMessage(Handle, WM_UNDO, 0, 0);
end;
{$ENDIF}

procedure TJvCustomRichEdit.StopGroupTyping;
begin
  if (RichEditVersion >= 2) and HandleAllocated then
    SendMessage(Handle, EM_STOPGROUPTYPING, 0, 0);
end;

{$IFDEF COMPILER3_UP}
procedure TJvCustomRichEdit.SetUIActive(Active: Boolean);
var
  Form: TCustomForm;
begin
  try
    Form := GetParentForm(Self);
    if Form <> nil then
      if Active then begin
        if (Form.ActiveOleControl <> nil) and
          (Form.ActiveOleControl <> Self) then
          Form.ActiveOleControl.Perform(CM_UIDEACTIVATE, 0, 0);
        Form.ActiveOleControl := Self;
        if AllowInPlace and CanFocus then SetFocus;
      end
      else begin
        if Form.ActiveOleControl = Self then Form.ActiveOleControl := nil;
        if (Form.ActiveControl = Self) and AllowInPlace then begin
          Windows.SetFocus(Handle);
          SelectionChange;
        end;
      end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TJvCustomRichEdit.CMDocWindowActivate(var Message: TMessage);
begin
  if Assigned(FCallback) then
    with TRichEditOleCallback(FCallback) do
      if Assigned(FDocForm) and IsFormMDIChild(FDocForm.Form) then begin
        if Message.WParam = 0 then begin
          FFrameForm.SetMenu(0, 0, 0);
          FFrameForm.ClearBorderSpace;
        end;
      end;
end;

procedure TJvCustomRichEdit.CMUIDeactivate(var Message: TMessage);
begin
  if (GetParentForm(Self) <> nil) and Assigned(FRichEditOle) and
    (GetParentForm(Self).ActiveOleControl = Self) then
    {IRichEditOle(FRichEditOle).InPlaceDeactivate};
end;
{$ENDIF COMPILER3_UP}

{ Find & Replace Dialogs }

procedure TJvCustomRichEdit.SetupFindDialog(Dialog: TFindDialog;
  const SearchStr, ReplaceStr: string);
begin
  with Dialog do begin
    if SearchStr <> '' then FindText := SearchStr;
    if RichEditVersion = 1 then
      Options := Options + [frHideUpDown, frDown];
    OnFind := FindDialogFind;
{$IFDEF COMPILER3_UP}
    OnClose := FindDialogClose;
{$ENDIF}
  end;
  if Dialog is TReplaceDialog then
    with TReplaceDialog(Dialog) do begin
      if ReplaceStr <> '' then ReplaceText := ReplaceStr;
      OnReplace := ReplaceDialogReplace;
    end;
end;

function TJvCustomRichEdit.FindDialog(const SearchStr: string): TFindDialog;
begin
  if FFindDialog = nil then begin
    FFindDialog := TFindDialog.Create(Self);
    if FReplaceDialog <> nil then
      FFindDialog.FindText := FReplaceDialog.FindText;
  end;
  Result := FFindDialog;
  SetupFindDialog(FFindDialog, SearchStr, '');
  FFindDialog.Execute;
end;

function TJvCustomRichEdit.ReplaceDialog(const SearchStr,
  ReplaceStr: string): TReplaceDialog;
begin
  if FReplaceDialog = nil then begin
    FReplaceDialog := TReplaceDialog.Create(Self);
    if FFindDialog <> nil then
      FReplaceDialog.FindText := FFindDialog.FindText;
  end;
  Result := FReplaceDialog;
  SetupFindDialog(FReplaceDialog, SearchStr, ReplaceStr);
  FReplaceDialog.Execute;
end;

function TJvCustomRichEdit.GetCanFindNext: Boolean;
begin
  Result := HandleAllocated and (FLastFind <> nil) and
    (FLastFind.FindText <> '');
end;

function TJvCustomRichEdit.FindNext: Boolean;
begin
  if CanFindNext then Result := FindEditText(FLastFind, False, True)
  else Result := False;
end;

procedure TJvCustomRichEdit.AdjustFindDialogPosition(Dialog: TFindDialog);
var
  TextRect, R: TRect;
begin
  if Dialog.Handle = 0 then Exit;
  with TextRect do begin
    TopLeft := ClientToScreen(GetCharPos(SelStart));
    BottomRight := ClientToScreen(GetCharPos(SelStart + SelLength));
    Inc(Bottom, 20);
  end;
  with Dialog do begin
    GetWindowRect(Handle, R);
    if PtInRect(R, TextRect.TopLeft) or PtInRect(R, TextRect.BottomRight) then
    begin
      if TextRect.Top > R.Bottom - R.Top + 20 then
        OffsetRect(R, 0, TextRect.Top - R.Bottom - 20)
      else begin
        if TextRect.Top + R.Bottom - R.Top < GetSystemMetrics(SM_CYSCREEN) then
          OffsetRect(R, 0, 40 + TextRect.Top - R.Top);
      end;
      Position := R.TopLeft;
    end;
  end;
end;

function TJvCustomRichEdit.FindEditText(Dialog: TFindDialog; AdjustPos, Events: Boolean): Boolean;
var
  Length, StartPos: Integer;
  SrchOptions: TRichSearchTypes;
begin
  with TFindDialog(Dialog) do begin
    SrchOptions := [stSetSelection];
    if frDown in Options then begin
      StartPos := Max(SelStart, SelStart + SelLength);
      Length := System.Length(Text) - StartPos + 1;
    end
    else begin
      SrchOptions := SrchOptions + [stBackward];
      StartPos := Min(SelStart, SelStart + SelLength);
      Length := StartPos + 1;
    end;
    if frMatchCase in Options then
      SrchOptions := SrchOptions + [stMatchCase];
    if frWholeWord in Options then
      SrchOptions := SrchOptions + [stWholeWord];
    Result := Self.FindText(FindText, StartPos, Length, SrchOptions) >= 0;
    if FindText <> '' then FLastFind := Dialog;
    if Result then begin
      if AdjustPos then AdjustFindDialogPosition(Dialog);
    end
    else if Events then TextNotFound(Dialog);
  end;
end;

procedure TJvCustomRichEdit.TextNotFound(Dialog: TFindDialog);
begin
  with Dialog do
    if Assigned(FOnTextNotFound) then FOnTextNotFound(Self, FindText);
end;

procedure TJvCustomRichEdit.FindDialogFind(Sender: TObject);
begin
  FindEditText(TFindDialog(Sender), True, True);
end;

procedure TJvCustomRichEdit.ReplaceDialogReplace(Sender: TObject);
var
  Cnt: Integer;
  SaveSelChange: TNotifyEvent;
begin
  with TReplaceDialog(Sender) do begin
    if (frReplaceAll in Options) then begin
      Cnt := 0;
      SaveSelChange := FOnSelChange;
      TJvRichEditStrings(Lines).EnableChange(False);
      try
        FOnSelChange := nil;
        while FindEditText(TFindDialog(Sender), False, False) do begin
          SelText := ReplaceText;
          Inc(Cnt);
        end;
        if Cnt = 0 then TextNotFound(TFindDialog(Sender))
        else AdjustFindDialogPosition(TFindDialog(Sender));
      finally
        TJvRichEditStrings(Lines).EnableChange(True);
        FOnSelChange := SaveSelChange;
        if Cnt > 0 then begin
          Change;
          SelectionChange;
        end;
      end;
    end
    else if (frReplace in Options) then begin
      if FindEditText(TFindDialog(Sender), True, True) then
        SelText := ReplaceText;
    end;
  end;
end;

{$IFDEF COMPILER3_UP}
procedure TJvCustomRichEdit.FindDialogClose(Sender: TObject);
begin
  CloseFindDialog(Sender as TFindDialog);
end;

procedure TJvCustomRichEdit.CloseFindDialog(Dialog: TFindDialog);
begin
  if Assigned(FOnCloseFindDialog) then FOnCloseFindDialog(Self, Dialog);
end;
{$ENDIF COMPILER3_UP}

{ Conversion formats }

procedure AppendConversionFormat(const Ext: string; Plain: Boolean;
  AClass: TConversionClass);
var
  NewRec: PRichConversionFormat;
begin
  New(NewRec);
  with NewRec^ do begin
{$IFNDEF VER90}
    Extension := AnsiLowerCaseFileName(Ext);
{$ELSE}
    Extension := LowerCase(Ext);
{$ENDIF}
    PlainText := Plain;
    ConversionClass := AClass;
    Next := ConversionFormatList;
  end;
  ConversionFormatList := NewRec;
end;

class procedure TJvCustomRichEdit.RegisterConversionFormat(const AExtension: string;
  APlainText: Boolean; AConversionClass: TConversionClass);
begin
  AppendConversionFormat(AExtension, APlainText, AConversionClass);
end;

{ Initialization part }

var
  OldError: Longint;
  FLibHandle: THandle;
  Ver: TOsVersionInfo;

initialization
  RichEditVersion := 1;
  OldError := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  try
{$IFNDEF RICHEDIT_VER_10}
    FLibHandle := LoadLibrary(RichEdit20ModuleName);
    if (FLibHandle > 0) and (FLibHandle < HINSTANCE_ERROR) then FLibHandle := 0;
{$ELSE}
    FLibHandle := 0;
{$ENDIF}
    if FLibHandle = 0 then begin
      FLibHandle := LoadLibrary(RichEdit10ModuleName);
      if (FLibHandle > 0) and (FLibHandle < HINSTANCE_ERROR) then FLibHandle := 0;
    end
    else begin
      RichEditVersion := 2;
      Ver.dwOSVersionInfoSize := SizeOf(Ver);
      GetVersionEx(Ver);
      with Ver do begin
        if (dwPlatformId = VER_PLATFORM_WIN32_NT) and
          (dwMajorVersion >= 5) then
          RichEditVersion := 3;
      end;
    end;
  finally
    SetErrorMode(OldError);
  end;
  CFEmbeddedObject := RegisterClipboardFormat(CF_EMBEDDEDOBJECT);
  CFLinkSource := RegisterClipboardFormat(CF_LINKSOURCE);
  CFRtf := RegisterClipboardFormat(CF_RTF);
  CFRtfNoObjs := RegisterClipboardFormat(CF_RTFNOOBJS);
finalization
  if FLibHandle <> 0 then FreeLibrary(FLibHandle);
end.
