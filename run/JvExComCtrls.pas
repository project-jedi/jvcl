{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExComCtrls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas.Hausladen@gmx.de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Last Modified: 2004-01-13

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I jvcl.inc}

unit JvExComCtrls;
interface
uses
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, ComCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Qt, QGraphics, QControls, QForms, QComCtrls,
  {$ENDIF VisualCLX}
  Classes, SysUtils,
  JvThemes, JvExControls;

{$IFDEF VCL}
 {$DEFINE NeedMouseEnterLeave}
{$ELSE}
 {$IF not declared(PatchedVCLX)}
  {$DEFINE NeedMouseEnterLeave}
 {$IFEND}
{$ENDIF VCL}

{$DEFINE ANIMATE}
{$IFDEF COMPILER6_UP}
 {$IF not declared(TAnimate)}
  {$UNDEF ANIMATE}
 {$IFEND}
{$ENDIF COMPILER6_UP}

type
{$IFDEF VCL}
  TJvExProgressBar = class(TProgressBar, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExCustomTabControl = class(TCustomTabControl, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExTabControl = class(TTabControl, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExTabSheet = class(TTabSheet, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExPageControl = class(TPageControl, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExStatusBar = class(TStatusBar, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExToolBar = class(TToolBar, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExToolButton = class(TToolButton, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  
  end;
  
{$ELSE}
  TJvExProgressBar = class(TProgressBar, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  
  end;
  
  TJvExCustomTabControl = class(TCustomTabControl,  IJvWinControlEvents, IJvCustomControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  
  end;
  
  TJvExTabControl = class(TTabControl,  IJvWinControlEvents, IJvCustomControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  
  end;
  
  TJvExTabSheet = class(TTabSheet,  IJvWinControlEvents, IJvCustomControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  
  end;
  
  TJvExPageControl = class(TPageControl,  IJvWinControlEvents, IJvCustomControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  
  end;
  
  TJvExStatusBar = class(TStatusBar,  IJvWinControlEvents, IJvCustomControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  
  end;
  
  TJvExCustomViewControl = class(TCustomViewControl, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExToolBar = class(TToolBar,  IJvWinControlEvents, IJvCustomControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  
  end;
  
  TJvExToolButton = class(TToolButton,  IJvWinControlEvents, IJvCustomControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  
  end;
  
{$ENDIF VCL}
{$IFDEF COMPILER6_UP}
  TJvExCustomHeaderControl = class(TCustomHeaderControl, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
 {$IFDEF VCL}
  TJvExCustomStatusBar = class(TCustomStatusBar, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
 {$ENDIF VCL}
{$ENDIF COMPILER6_UP}
  TJvExHeaderControl = class(THeaderControl, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExCustomTreeView = class(TCustomTreeView, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExTreeView = class(TTreeView, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExCustomListView = class(TCustomListView, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExListView = class(TListView, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
{$IFDEF ANIMATE}
  {$IFDEF VCL}
  TJvExAnimate = class(TAnimate, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  {$ELSE}
  TJvExAnimate = class(TAnimate,  IJvWinControlEvents, IJvCustomControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  
  end;
  
  {$ENDIF VCL}
{$ENDIF ANIMATE}
{$IFDEF VCL}
  TJvExCustomHotKey = class(TCustomHotKey, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExCustomUpDown = class(TCustomUpDown, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExCoolBar = class(TCoolBar, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExMonthCalendar = class(TMonthCalendar, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExHotKey = class(THotKey, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExUpDown = class(TUpDown, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExCommonCalendar = class(TCommonCalendar, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExDateTimePicker = class(TDateTimePicker, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExPageScroller = class(TPageScroller, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExTrackBar = class(TTrackBar, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
 {$IFDEF COMPILER6_UP}
  TJvExCustomComboBoxEx = class(TCustomComboBoxEx, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
  TJvExComboBoxEx = class(TComboBoxEx, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // IJvControlEvents
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
    {$IFNDEF HASAUTOSIZE}
     {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
     {$ENDIF !COMPILER6_UP}
    {$ENDIF !HASAUTOSIZE}
  public
    procedure Dispatch(var Msg); override;
  protected
   // IJvWinControlEvents
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  protected
   {$IF not declared(PatchedVCLX)}
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
   {$IFEND}
  protected
    function NeedKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF NeedMouseEnterLeave}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF NeedMouseEnterLeave}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure DoGetDlgCode(var Code: TDlgCodes); virtual;
  {$IFDEF VisualCLX}
  private
    FCanvas: TCanvas;
  protected
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  {$ENDIF VisualCLX}
  end;
  
 {$ENDIF COMPILER6_UP}
{$ENDIF VCL}

implementation

{$IFDEF VCL}
{$IFDEF VCL}
procedure TJvExProgressBar.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExProgressBar.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExProgressBar.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExProgressBar.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExProgressBar.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExProgressBar.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExProgressBar.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExProgressBar.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExProgressBar.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExProgressBar.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExProgressBar.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExProgressBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExProgressBar.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExProgressBar.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExProgressBar.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExProgressBar.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExProgressBar.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExProgressBar.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExProgressBar.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExProgressBar.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExProgressBar.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExProgressBar.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExProgressBar.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExProgressBar.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExProgressBar.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExProgressBar.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExProgressBar.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExProgressBar.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExProgressBar.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExProgressBar.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomTabControl.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExCustomTabControl.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomTabControl.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomTabControl.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomTabControl.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomTabControl.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomTabControl.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomTabControl.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomTabControl.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomTabControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomTabControl.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomTabControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomTabControl.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomTabControl.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomTabControl.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExCustomTabControl.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomTabControl.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomTabControl.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomTabControl.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExCustomTabControl.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomTabControl.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomTabControl.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExCustomTabControl.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomTabControl.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExCustomTabControl.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExCustomTabControl.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExCustomTabControl.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExCustomTabControl.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExCustomTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExCustomTabControl.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExCustomTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExCustomTabControl.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExTabControl.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExTabControl.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExTabControl.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExTabControl.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExTabControl.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExTabControl.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExTabControl.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExTabControl.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExTabControl.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExTabControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExTabControl.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExTabControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExTabControl.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExTabControl.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExTabControl.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExTabControl.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExTabControl.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExTabControl.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExTabControl.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExTabControl.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExTabControl.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExTabControl.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExTabControl.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExTabControl.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExTabControl.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExTabControl.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExTabControl.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExTabControl.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExTabControl.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExTabControl.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExTabSheet.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExTabSheet.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExTabSheet.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExTabSheet.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExTabSheet.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExTabSheet.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExTabSheet.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExTabSheet.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExTabSheet.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExTabSheet.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExTabSheet.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExTabSheet.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExTabSheet.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExTabSheet.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExTabSheet.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExTabSheet.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExTabSheet.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExTabSheet.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExTabSheet.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExTabSheet.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExTabSheet.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExTabSheet.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExTabSheet.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExTabSheet.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExTabSheet.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExTabSheet.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExTabSheet.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExTabSheet.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExTabSheet.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExTabSheet.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExPageControl.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExPageControl.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExPageControl.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExPageControl.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExPageControl.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExPageControl.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExPageControl.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExPageControl.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExPageControl.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExPageControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExPageControl.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExPageControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExPageControl.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExPageControl.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExPageControl.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExPageControl.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExPageControl.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExPageControl.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExPageControl.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExPageControl.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExPageControl.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExPageControl.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExPageControl.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExPageControl.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExPageControl.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExPageControl.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExPageControl.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExPageControl.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExPageControl.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExPageControl.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExStatusBar.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExStatusBar.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExStatusBar.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExStatusBar.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExStatusBar.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExStatusBar.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExStatusBar.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExStatusBar.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExStatusBar.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExStatusBar.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExStatusBar.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExStatusBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExStatusBar.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExStatusBar.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExStatusBar.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExStatusBar.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExStatusBar.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExStatusBar.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExStatusBar.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExStatusBar.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExStatusBar.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExStatusBar.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExStatusBar.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExStatusBar.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExStatusBar.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExStatusBar.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExStatusBar.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExStatusBar.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExStatusBar.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExStatusBar.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExToolBar.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExToolBar.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExToolBar.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExToolBar.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExToolBar.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExToolBar.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExToolBar.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExToolBar.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExToolBar.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExToolBar.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExToolBar.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExToolBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExToolBar.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExToolBar.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExToolBar.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExToolBar.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExToolBar.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExToolBar.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExToolBar.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExToolBar.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExToolBar.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExToolBar.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExToolBar.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExToolBar.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExToolBar.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExToolBar.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExToolBar.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExToolBar.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExToolBar.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExToolBar.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExToolButton.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExToolButton.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExToolButton.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExToolButton.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExToolButton.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExToolButton.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExToolButton.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExToolButton.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExToolButton.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExToolButton.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExToolButton.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExToolButton.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExToolButton.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExToolButton.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExToolButton.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExToolButton.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExToolButton.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
function TJvExToolButton.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

constructor TJvExToolButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExToolButton.Destroy;
begin
  
  inherited Destroy;
end;
{$ELSE}
{$IFDEF VCL}
procedure TJvExProgressBar.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExProgressBar.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExProgressBar.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExProgressBar.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExProgressBar.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExProgressBar.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExProgressBar.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExProgressBar.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExProgressBar.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExProgressBar.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExProgressBar.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExProgressBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExProgressBar.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExProgressBar.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExProgressBar.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExProgressBar.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExProgressBar.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
function TJvExProgressBar.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

constructor TJvExProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExProgressBar.Destroy;
begin
  
  inherited Destroy;
end;
{$IFDEF VCL}
procedure TJvExCustomTabControl.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExCustomTabControl.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomTabControl.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomTabControl.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomTabControl.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomTabControl.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomTabControl.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomTabControl.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomTabControl.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomTabControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomTabControl.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomTabControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomTabControl.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomTabControl.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomTabControl.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExCustomTabControl.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomTabControl.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomTabControl.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomTabControl.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExCustomTabControl.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomTabControl.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomTabControl.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExCustomTabControl.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomTabControl.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExCustomTabControl.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExCustomTabControl.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExCustomTabControl.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExCustomTabControl.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

constructor TJvExCustomTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExCustomTabControl.Destroy;
begin
  
  inherited Destroy;
end;
{$IFDEF VCL}
procedure TJvExTabControl.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExTabControl.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExTabControl.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExTabControl.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExTabControl.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExTabControl.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExTabControl.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExTabControl.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExTabControl.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExTabControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExTabControl.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExTabControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExTabControl.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExTabControl.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExTabControl.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExTabControl.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExTabControl.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExTabControl.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExTabControl.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExTabControl.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExTabControl.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExTabControl.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExTabControl.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExTabControl.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExTabControl.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExTabControl.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExTabControl.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExTabControl.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

constructor TJvExTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExTabControl.Destroy;
begin
  
  inherited Destroy;
end;
{$IFDEF VCL}
procedure TJvExTabSheet.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExTabSheet.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExTabSheet.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExTabSheet.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExTabSheet.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExTabSheet.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExTabSheet.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExTabSheet.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExTabSheet.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExTabSheet.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExTabSheet.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExTabSheet.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExTabSheet.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExTabSheet.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExTabSheet.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExTabSheet.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExTabSheet.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExTabSheet.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExTabSheet.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExTabSheet.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExTabSheet.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExTabSheet.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExTabSheet.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExTabSheet.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExTabSheet.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExTabSheet.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExTabSheet.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExTabSheet.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

constructor TJvExTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExTabSheet.Destroy;
begin
  
  inherited Destroy;
end;
{$IFDEF VCL}
procedure TJvExPageControl.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExPageControl.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExPageControl.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExPageControl.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExPageControl.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExPageControl.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExPageControl.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExPageControl.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExPageControl.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExPageControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExPageControl.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExPageControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExPageControl.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExPageControl.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExPageControl.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExPageControl.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExPageControl.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExPageControl.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExPageControl.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExPageControl.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExPageControl.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExPageControl.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExPageControl.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExPageControl.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExPageControl.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExPageControl.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExPageControl.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExPageControl.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

constructor TJvExPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExPageControl.Destroy;
begin
  
  inherited Destroy;
end;
{$IFDEF VCL}
procedure TJvExStatusBar.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExStatusBar.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExStatusBar.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExStatusBar.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExStatusBar.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExStatusBar.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExStatusBar.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExStatusBar.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExStatusBar.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExStatusBar.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExStatusBar.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExStatusBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExStatusBar.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExStatusBar.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExStatusBar.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExStatusBar.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExStatusBar.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExStatusBar.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExStatusBar.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExStatusBar.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExStatusBar.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExStatusBar.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExStatusBar.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExStatusBar.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExStatusBar.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExStatusBar.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExStatusBar.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExStatusBar.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

constructor TJvExStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExStatusBar.Destroy;
begin
  
  inherited Destroy;
end;
{$IFDEF VCL}
procedure TJvExCustomViewControl.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExCustomViewControl.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomViewControl.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomViewControl.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomViewControl.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomViewControl.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomViewControl.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomViewControl.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomViewControl.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomViewControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomViewControl.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomViewControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomViewControl.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomViewControl.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomViewControl.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExCustomViewControl.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomViewControl.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomViewControl.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomViewControl.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExCustomViewControl.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomViewControl.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomViewControl.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExCustomViewControl.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomViewControl.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExCustomViewControl.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExCustomViewControl.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExCustomViewControl.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExCustomViewControl.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExCustomViewControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExCustomViewControl.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExCustomViewControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExCustomViewControl.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExToolBar.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExToolBar.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExToolBar.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExToolBar.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExToolBar.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExToolBar.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExToolBar.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExToolBar.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExToolBar.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExToolBar.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExToolBar.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExToolBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExToolBar.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExToolBar.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExToolBar.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExToolBar.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExToolBar.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExToolBar.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExToolBar.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExToolBar.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExToolBar.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExToolBar.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExToolBar.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExToolBar.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExToolBar.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExToolBar.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExToolBar.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExToolBar.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

constructor TJvExToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExToolBar.Destroy;
begin
  
  inherited Destroy;
end;
{$IFDEF VCL}
procedure TJvExToolButton.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExToolButton.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExToolButton.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExToolButton.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExToolButton.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExToolButton.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExToolButton.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExToolButton.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExToolButton.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExToolButton.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExToolButton.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExToolButton.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExToolButton.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExToolButton.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExToolButton.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExToolButton.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExToolButton.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExToolButton.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExToolButton.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExToolButton.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExToolButton.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExToolButton.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExToolButton.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExToolButton.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExToolButton.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExToolButton.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExToolButton.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExToolButton.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

constructor TJvExToolButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExToolButton.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF COMPILER6_UP}
{$IFDEF VCL}
procedure TJvExCustomHeaderControl.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExCustomHeaderControl.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomHeaderControl.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomHeaderControl.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomHeaderControl.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomHeaderControl.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomHeaderControl.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomHeaderControl.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomHeaderControl.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomHeaderControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomHeaderControl.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomHeaderControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomHeaderControl.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomHeaderControl.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomHeaderControl.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExCustomHeaderControl.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomHeaderControl.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomHeaderControl.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomHeaderControl.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExCustomHeaderControl.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomHeaderControl.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomHeaderControl.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExCustomHeaderControl.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomHeaderControl.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExCustomHeaderControl.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExCustomHeaderControl.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExCustomHeaderControl.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExCustomHeaderControl.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExCustomHeaderControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExCustomHeaderControl.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExCustomHeaderControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExCustomHeaderControl.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
 {$IFDEF VCL}
{$IFDEF VCL}
procedure TJvExCustomStatusBar.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExCustomStatusBar.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomStatusBar.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomStatusBar.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomStatusBar.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomStatusBar.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomStatusBar.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomStatusBar.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomStatusBar.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomStatusBar.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomStatusBar.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomStatusBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomStatusBar.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomStatusBar.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomStatusBar.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExCustomStatusBar.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomStatusBar.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomStatusBar.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomStatusBar.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExCustomStatusBar.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomStatusBar.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomStatusBar.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExCustomStatusBar.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomStatusBar.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExCustomStatusBar.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExCustomStatusBar.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExCustomStatusBar.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExCustomStatusBar.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExCustomStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExCustomStatusBar.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExCustomStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExCustomStatusBar.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
 {$ENDIF VCL}
{$ENDIF COMPILER6_UP}
{$IFDEF VCL}
procedure TJvExHeaderControl.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExHeaderControl.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExHeaderControl.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExHeaderControl.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExHeaderControl.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExHeaderControl.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExHeaderControl.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExHeaderControl.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExHeaderControl.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExHeaderControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExHeaderControl.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExHeaderControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExHeaderControl.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExHeaderControl.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExHeaderControl.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExHeaderControl.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExHeaderControl.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExHeaderControl.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExHeaderControl.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExHeaderControl.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExHeaderControl.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExHeaderControl.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExHeaderControl.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExHeaderControl.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExHeaderControl.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExHeaderControl.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExHeaderControl.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExHeaderControl.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExHeaderControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExHeaderControl.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExHeaderControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExHeaderControl.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomTreeView.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExCustomTreeView.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomTreeView.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomTreeView.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomTreeView.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomTreeView.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomTreeView.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomTreeView.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomTreeView.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomTreeView.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomTreeView.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomTreeView.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomTreeView.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomTreeView.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomTreeView.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExCustomTreeView.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomTreeView.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomTreeView.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomTreeView.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExCustomTreeView.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomTreeView.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomTreeView.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExCustomTreeView.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomTreeView.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExCustomTreeView.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExCustomTreeView.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExCustomTreeView.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExCustomTreeView.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExCustomTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExCustomTreeView.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExCustomTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExCustomTreeView.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExTreeView.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExTreeView.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExTreeView.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExTreeView.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExTreeView.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExTreeView.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExTreeView.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExTreeView.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExTreeView.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExTreeView.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExTreeView.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExTreeView.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExTreeView.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExTreeView.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExTreeView.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExTreeView.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExTreeView.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExTreeView.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExTreeView.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExTreeView.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExTreeView.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExTreeView.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExTreeView.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExTreeView.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExTreeView.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExTreeView.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExTreeView.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExTreeView.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExTreeView.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExTreeView.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomListView.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExCustomListView.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomListView.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomListView.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomListView.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomListView.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomListView.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomListView.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomListView.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomListView.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomListView.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomListView.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomListView.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomListView.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomListView.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExCustomListView.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomListView.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomListView.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomListView.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExCustomListView.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomListView.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomListView.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExCustomListView.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomListView.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExCustomListView.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExCustomListView.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExCustomListView.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExCustomListView.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExCustomListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExCustomListView.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExCustomListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExCustomListView.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExListView.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExListView.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExListView.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExListView.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExListView.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExListView.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExListView.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExListView.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExListView.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExListView.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExListView.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExListView.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExListView.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExListView.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExListView.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExListView.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExListView.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExListView.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExListView.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExListView.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExListView.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExListView.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExListView.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExListView.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExListView.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExListView.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExListView.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExListView.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExListView.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExListView.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF ANIMATE}
  {$IFDEF VCL}
{$IFDEF VCL}
procedure TJvExAnimate.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExAnimate.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExAnimate.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExAnimate.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExAnimate.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExAnimate.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExAnimate.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExAnimate.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExAnimate.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExAnimate.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExAnimate.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExAnimate.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExAnimate.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExAnimate.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExAnimate.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExAnimate.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExAnimate.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExAnimate.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExAnimate.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExAnimate.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExAnimate.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExAnimate.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExAnimate.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExAnimate.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExAnimate.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExAnimate.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExAnimate.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExAnimate.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExAnimate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExAnimate.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExAnimate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExAnimate.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
  {$ELSE}
{$IFDEF VCL}
procedure TJvExAnimate.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExAnimate.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExAnimate.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExAnimate.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExAnimate.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExAnimate.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExAnimate.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExAnimate.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExAnimate.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExAnimate.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExAnimate.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExAnimate.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExAnimate.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExAnimate.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExAnimate.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExAnimate.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExAnimate.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExAnimate.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExAnimate.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExAnimate.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExAnimate.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExAnimate.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExAnimate.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExAnimate.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExAnimate.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExAnimate.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExAnimate.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExAnimate.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

constructor TJvExAnimate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExAnimate.Destroy;
begin
  
  inherited Destroy;
end;
  {$ENDIF VCL}
{$ENDIF ANIMATE}
{$IFDEF VCL}
{$IFDEF VCL}
procedure TJvExCustomHotKey.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExCustomHotKey.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomHotKey.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomHotKey.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomHotKey.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomHotKey.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomHotKey.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomHotKey.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomHotKey.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomHotKey.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomHotKey.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomHotKey.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomHotKey.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomHotKey.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomHotKey.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExCustomHotKey.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomHotKey.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomHotKey.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomHotKey.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExCustomHotKey.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomHotKey.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomHotKey.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExCustomHotKey.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomHotKey.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExCustomHotKey.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExCustomHotKey.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExCustomHotKey.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExCustomHotKey.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExCustomHotKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExCustomHotKey.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExCustomHotKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExCustomHotKey.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExHotKey.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExHotKey.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExHotKey.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExHotKey.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExHotKey.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExHotKey.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExHotKey.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExHotKey.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExHotKey.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExHotKey.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExHotKey.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExHotKey.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExHotKey.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExHotKey.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExHotKey.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExHotKey.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExHotKey.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExHotKey.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExHotKey.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExHotKey.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExHotKey.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExHotKey.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExHotKey.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExHotKey.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExHotKey.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExHotKey.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExHotKey.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExHotKey.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExHotKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExHotKey.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExHotKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExHotKey.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomUpDown.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExCustomUpDown.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomUpDown.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomUpDown.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomUpDown.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomUpDown.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomUpDown.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomUpDown.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomUpDown.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomUpDown.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomUpDown.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomUpDown.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomUpDown.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomUpDown.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomUpDown.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExCustomUpDown.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomUpDown.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomUpDown.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomUpDown.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExCustomUpDown.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomUpDown.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomUpDown.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExCustomUpDown.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomUpDown.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExCustomUpDown.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExCustomUpDown.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExCustomUpDown.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExCustomUpDown.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExCustomUpDown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExCustomUpDown.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExCustomUpDown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExCustomUpDown.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExUpDown.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExUpDown.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExUpDown.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExUpDown.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExUpDown.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExUpDown.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExUpDown.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExUpDown.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExUpDown.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExUpDown.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExUpDown.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExUpDown.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExUpDown.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExUpDown.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExUpDown.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExUpDown.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExUpDown.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExUpDown.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExUpDown.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExUpDown.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExUpDown.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExUpDown.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExUpDown.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExUpDown.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExUpDown.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExUpDown.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExUpDown.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExUpDown.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExUpDown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExUpDown.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExUpDown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExUpDown.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCoolBar.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExCoolBar.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCoolBar.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCoolBar.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCoolBar.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCoolBar.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCoolBar.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCoolBar.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCoolBar.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCoolBar.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCoolBar.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCoolBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCoolBar.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCoolBar.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCoolBar.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExCoolBar.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCoolBar.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCoolBar.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCoolBar.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExCoolBar.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCoolBar.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCoolBar.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExCoolBar.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCoolBar.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExCoolBar.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExCoolBar.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExCoolBar.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExCoolBar.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExCoolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExCoolBar.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExCoolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExCoolBar.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCommonCalendar.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExCommonCalendar.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCommonCalendar.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCommonCalendar.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCommonCalendar.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCommonCalendar.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCommonCalendar.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCommonCalendar.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCommonCalendar.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCommonCalendar.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCommonCalendar.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCommonCalendar.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCommonCalendar.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCommonCalendar.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCommonCalendar.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExCommonCalendar.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCommonCalendar.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCommonCalendar.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCommonCalendar.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExCommonCalendar.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCommonCalendar.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCommonCalendar.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExCommonCalendar.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCommonCalendar.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExCommonCalendar.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExCommonCalendar.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExCommonCalendar.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExCommonCalendar.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExCommonCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExCommonCalendar.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExCommonCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExCommonCalendar.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExMonthCalendar.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExMonthCalendar.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExMonthCalendar.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExMonthCalendar.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExMonthCalendar.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExMonthCalendar.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExMonthCalendar.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExMonthCalendar.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExMonthCalendar.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExMonthCalendar.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExMonthCalendar.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExMonthCalendar.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExMonthCalendar.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExMonthCalendar.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExMonthCalendar.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExMonthCalendar.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExMonthCalendar.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExMonthCalendar.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExMonthCalendar.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExMonthCalendar.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExMonthCalendar.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExMonthCalendar.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExMonthCalendar.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExMonthCalendar.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExMonthCalendar.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExMonthCalendar.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExMonthCalendar.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExMonthCalendar.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExMonthCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExMonthCalendar.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExMonthCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExMonthCalendar.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExDateTimePicker.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExDateTimePicker.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExDateTimePicker.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExDateTimePicker.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExDateTimePicker.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExDateTimePicker.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExDateTimePicker.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExDateTimePicker.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExDateTimePicker.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExDateTimePicker.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExDateTimePicker.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExDateTimePicker.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExDateTimePicker.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExDateTimePicker.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExDateTimePicker.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExDateTimePicker.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExDateTimePicker.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExDateTimePicker.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExDateTimePicker.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExDateTimePicker.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExDateTimePicker.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExDateTimePicker.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExDateTimePicker.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExDateTimePicker.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExDateTimePicker.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExDateTimePicker.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExDateTimePicker.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExDateTimePicker.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExDateTimePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExDateTimePicker.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExDateTimePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExDateTimePicker.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExPageScroller.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExPageScroller.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExPageScroller.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExPageScroller.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExPageScroller.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExPageScroller.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExPageScroller.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExPageScroller.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExPageScroller.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExPageScroller.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExPageScroller.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExPageScroller.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExPageScroller.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExPageScroller.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExPageScroller.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExPageScroller.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExPageScroller.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExPageScroller.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExPageScroller.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExPageScroller.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExPageScroller.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExPageScroller.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExPageScroller.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExPageScroller.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExPageScroller.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExPageScroller.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExPageScroller.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExPageScroller.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExPageScroller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExPageScroller.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExPageScroller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExPageScroller.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExTrackBar.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExTrackBar.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExTrackBar.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExTrackBar.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExTrackBar.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExTrackBar.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExTrackBar.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExTrackBar.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExTrackBar.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExTrackBar.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExTrackBar.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExTrackBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExTrackBar.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExTrackBar.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExTrackBar.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExTrackBar.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExTrackBar.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExTrackBar.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExTrackBar.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExTrackBar.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExTrackBar.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExTrackBar.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExTrackBar.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExTrackBar.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExTrackBar.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExTrackBar.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExTrackBar.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExTrackBar.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExTrackBar.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExTrackBar.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
 {$IFDEF COMPILER6_UP}
{$IFDEF VCL}
procedure TJvExCustomComboBoxEx.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExCustomComboBoxEx.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomComboBoxEx.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomComboBoxEx.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomComboBoxEx.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomComboBoxEx.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomComboBoxEx.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomComboBoxEx.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomComboBoxEx.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomComboBoxEx.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomComboBoxEx.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomComboBoxEx.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomComboBoxEx.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomComboBoxEx.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomComboBoxEx.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExCustomComboBoxEx.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomComboBoxEx.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomComboBoxEx.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomComboBoxEx.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExCustomComboBoxEx.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomComboBoxEx.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomComboBoxEx.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExCustomComboBoxEx.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomComboBoxEx.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExCustomComboBoxEx.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExCustomComboBoxEx.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExCustomComboBoxEx.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExCustomComboBoxEx.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExCustomComboBoxEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExCustomComboBoxEx.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExCustomComboBoxEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExCustomComboBoxEx.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExComboBoxEx.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;

procedure TJvExComboBoxEx.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExComboBoxEx.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExComboBoxEx.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExComboBoxEx.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExComboBoxEx.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExComboBoxEx.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExComboBoxEx.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExComboBoxEx.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExComboBoxEx.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExComboBoxEx.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExComboBoxEx.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExComboBoxEx.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExComboBoxEx.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExComboBoxEx.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value);
end;
 {$ENDIF !COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}
procedure TJvExComboBoxEx.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExComboBoxEx.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExComboBoxEx.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExComboBoxEx.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanging(Self, Control, Inserting);
end;

procedure TJvExComboBoxEx.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  Control_ControlsListChanged(Self, Control, Inserting);
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExComboBoxEx.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExComboBoxEx.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$IF not declared(PatchedVCLX)}
procedure TJvExComboBoxEx.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExComboBoxEx.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
procedure TJvExComboBoxEx.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if WidgetControl_Painting(Self, Canvas, EventRegion) <> nil then
  begin // returns an interface
    DoPaintBackground(Canvas, 0);
    Paint;
  end;
end;

procedure TJvExComboBoxEx.Paint;
begin
  WidgetControl_DefaultPaint(Self, Canvas);
end;

function TWidgetControl.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := TWidgetControl_NeedKey(Self, Key, Shift, KeyText,
    inherited NeedKey(Key, Shift, KeyText));
end;
{$ENDIF VisualCLX}
function TJvExComboBoxEx.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;
procedure TJvExComboBoxEx.DoGetDlgCode(var Code: TDlgCodes);
begin
end;

{$IFDEF VCL}
constructor TJvExComboBoxEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
end;

destructor TJvExComboBoxEx.Destroy;
begin
  
  inherited Destroy;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExComboBoxEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  
end;

destructor TJvExComboBoxEx.Destroy;
begin
  
  FCanvas.Free;
  inherited Destroy;
end;
{$ENDIF VisualCLX}
 {$ENDIF COMPILER6_UP}
{$ENDIF VCL}

end.
