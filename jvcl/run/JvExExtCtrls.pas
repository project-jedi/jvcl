{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExExtCtrls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas.Hausladen@gmx.de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Last Modified: 2004-01-12

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I jvcl.inc}

unit JvExExtCtrls;
interface
uses
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, ExtCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Qt, QGraphics, QControls, QForms, QExtCtrls,
  {$ENDIF VisualCLX}
  Classes, SysUtils,
  JvThemes, JvExControls;

type
  TJvExShape = class(TShape, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
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
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF VisualCLX}
  end;
  
  TJvExPaintBox = class(TPaintBox, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
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
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF VisualCLX}
  end;
  
  TJvExImage = class(TImage, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
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
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF VisualCLX}
  end;
  
  TJvExBevel = class(TBevel, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
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
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF VisualCLX}
  end;
  
  TJvExCustomPanel = class(TCustomPanel, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
   {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
   {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
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
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  end;
  
  TJvExPanel = class(TPanel, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
   {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
   {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
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
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  end;
  
  TJvExCustomRadioGroup = class(TCustomRadioGroup, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
   {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
   {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
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
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  private
    FCanvas: TCanvas;
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$ENDIF VisualCLX}
  end;
  
  TJvExRadioGroup = class(TRadioGroup, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
   {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
   {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
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
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  private
    FCanvas: TCanvas;
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$ENDIF VisualCLX}
  end;
  
  TJvExSplitter = class(TSplitter, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
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
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF VisualCLX}
  end;
  

  TJvExCustomControlBar = class(TCustomControlBar, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
   {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
   {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
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
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF VisualCLX}
  protected
    function HitTest(X, Y: Integer): Boolean; overload; dynamic;
  {$ENDIF VisualCLX}
  end;
  

  TJvExControlBar = class(TControlBar, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
   {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
   {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
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
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  {$IFDEF VisualCLX}
  protected
    function HitTest(X, Y: Integer): Boolean; overload; dynamic;
  {$ENDIF VisualCLX}
  end;
  
  
{$IFDEF VCL}
  TJvExPage = class(TPage, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
   {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
   {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
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
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  end;
  
  TJvExNotebook = class(TNotebook, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
   {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
   {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
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
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  end;
  
  TJvExHeader = class(THeader, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
   {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
   {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
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
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  end;
  
  {$IFDEF COMPILER6_UP}
  TJvExBoundLabel = class(TBoundLabel, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
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
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF VisualCLX}
  end;
  
  TJvExCustomLabeledEdit = class(TCustomLabeledEdit, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
   {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
   {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
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
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  private
    FCanvas: TCanvas;
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$ENDIF VisualCLX}
  end;
  
  TJvExLabeledEdit = class(TLabeledEdit, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
   {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
   {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
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
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  private
    FCanvas: TCanvas;
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$ENDIF VisualCLX}
  end;
  
  TJvExCustomColorBox = class(TCustomColorBox, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
   {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
   {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
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
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  private
    FCanvas: TCanvas;
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$ENDIF VisualCLX}
  end;
  
  TJvExColorBox = class(TColorBox, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
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
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
   {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
   {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
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
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  private
    FCanvas: TCanvas;
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$ENDIF VisualCLX}
  end;
  
  {$ENDIF COMPILER6_UP}
{$ENDIF VCL}

implementation


{$IFDEF VCL}
procedure TJvExShape.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExShape.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExShape.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExShape.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExShape.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExShape.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExShape.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExShape.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExShape.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExShape.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExShape.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExShape.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExShape.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExShape.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExShape.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExShape.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExShape.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExShape.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}

{$IFDEF VCL}
procedure TJvExPaintBox.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExPaintBox.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExPaintBox.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExPaintBox.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExPaintBox.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExPaintBox.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExPaintBox.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExPaintBox.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExPaintBox.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExPaintBox.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExPaintBox.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExPaintBox.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExPaintBox.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExPaintBox.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExPaintBox.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExPaintBox.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExPaintBox.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExPaintBox.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}

{$IFDEF VCL}
procedure TJvExImage.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExImage.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExImage.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExImage.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExImage.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExImage.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExImage.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExImage.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExImage.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExImage.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExImage.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExImage.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExImage.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExImage.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExImage.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExImage.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExImage.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExImage.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}

{$IFDEF VCL}
procedure TJvExBevel.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExBevel.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExBevel.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExBevel.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExBevel.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExBevel.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExBevel.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExBevel.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExBevel.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExBevel.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExBevel.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExBevel.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExBevel.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExBevel.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExBevel.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExBevel.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExBevel.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExBevel.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}

{$IFDEF VCL}
procedure TJvExCustomPanel.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomPanel.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomPanel.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomPanel.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomPanel.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomPanel.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomPanel.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomPanel.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomPanel.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomPanel.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomPanel.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomPanel.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomPanel.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomPanel.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExCustomPanel.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExCustomPanel.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomPanel.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExCustomPanel.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomPanel.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomPanel.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomPanel.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomPanel.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExCustomPanel.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomPanel.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomPanel.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
procedure TJvExCustomPanel.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}
  

{$IFDEF VCL}
procedure TJvExPanel.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExPanel.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExPanel.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExPanel.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExPanel.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExPanel.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExPanel.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExPanel.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExPanel.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExPanel.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExPanel.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExPanel.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExPanel.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExPanel.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExPanel.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExPanel.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExPanel.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExPanel.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExPanel.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExPanel.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExPanel.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExPanel.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExPanel.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExPanel.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExPanel.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
procedure TJvExPanel.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}
  

{$IFDEF VCL}
procedure TJvExCustomRadioGroup.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomRadioGroup.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomRadioGroup.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomRadioGroup.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomRadioGroup.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomRadioGroup.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomRadioGroup.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomRadioGroup.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomRadioGroup.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomRadioGroup.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomRadioGroup.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomRadioGroup.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomRadioGroup.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomRadioGroup.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExCustomRadioGroup.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExCustomRadioGroup.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomRadioGroup.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExCustomRadioGroup.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomRadioGroup.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomRadioGroup.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomRadioGroup.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomRadioGroup.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExCustomRadioGroup.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomRadioGroup.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomRadioGroup.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExCustomRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TJvExCustomRadioGroup.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvExCustomRadioGroup.Paint;
var
  Event: QPaintEventH;
begin
  Event := QPaintEvent_create(QPainter_clipRegion(FCanvas.Handle), False);
  try
    QObject_event(Handle, Event);
  finally
    QPaintEvent_destroy(Event);
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomRadioGroup.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}
  

{$IFDEF VCL}
procedure TJvExRadioGroup.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExRadioGroup.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExRadioGroup.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExRadioGroup.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExRadioGroup.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExRadioGroup.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExRadioGroup.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExRadioGroup.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExRadioGroup.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExRadioGroup.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExRadioGroup.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExRadioGroup.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExRadioGroup.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExRadioGroup.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExRadioGroup.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExRadioGroup.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExRadioGroup.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExRadioGroup.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExRadioGroup.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExRadioGroup.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExRadioGroup.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExRadioGroup.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExRadioGroup.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExRadioGroup.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExRadioGroup.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TJvExRadioGroup.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvExRadioGroup.Paint;
var
  Event: QPaintEventH;
begin
  Event := QPaintEvent_create(QPainter_clipRegion(FCanvas.Handle), False);
  try
    QObject_event(Handle, Event);
  finally
    QPaintEvent_destroy(Event);
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExRadioGroup.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}
  

{$IFDEF VCL}
procedure TJvExSplitter.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExSplitter.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExSplitter.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExSplitter.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExSplitter.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExSplitter.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExSplitter.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExSplitter.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExSplitter.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExSplitter.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExSplitter.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExSplitter.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExSplitter.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExSplitter.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExSplitter.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExSplitter.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExSplitter.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExSplitter.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}


{$IFDEF VCL}
procedure TJvExCustomControlBar.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomControlBar.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomControlBar.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomControlBar.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomControlBar.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomControlBar.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomControlBar.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomControlBar.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomControlBar.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomControlBar.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomControlBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomControlBar.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomControlBar.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomControlBar.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExCustomControlBar.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExCustomControlBar.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomControlBar.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExCustomControlBar.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomControlBar.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomControlBar.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomControlBar.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomControlBar.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExCustomControlBar.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomControlBar.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomControlBar.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
procedure TJvExCustomControlBar.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}
  
{$IFDEF VisualCLX}
function TJvExCustomControlBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height);
end;
{$ENDIF VisualCLX}



{$IFDEF VCL}
procedure TJvExControlBar.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExControlBar.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExControlBar.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExControlBar.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExControlBar.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExControlBar.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExControlBar.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExControlBar.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExControlBar.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExControlBar.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExControlBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExControlBar.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExControlBar.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExControlBar.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExControlBar.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExControlBar.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExControlBar.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExControlBar.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExControlBar.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExControlBar.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExControlBar.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExControlBar.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExControlBar.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExControlBar.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExControlBar.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
procedure TJvExControlBar.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}
  
{$IFDEF VisualCLX}
function TJvExControlBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height);
end;
{$ENDIF VisualCLX}


{$IFDEF VCL}

{$IFDEF VCL}
procedure TJvExPage.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExPage.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExPage.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExPage.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExPage.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExPage.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExPage.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExPage.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExPage.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExPage.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExPage.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExPage.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExPage.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExPage.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExPage.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExPage.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExPage.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExPage.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExPage.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExPage.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExPage.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExPage.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExPage.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExPage.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExPage.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
procedure TJvExPage.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}
  

{$IFDEF VCL}
procedure TJvExNotebook.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExNotebook.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExNotebook.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExNotebook.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExNotebook.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExNotebook.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExNotebook.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExNotebook.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExNotebook.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExNotebook.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExNotebook.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExNotebook.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExNotebook.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExNotebook.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExNotebook.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExNotebook.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExNotebook.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExNotebook.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExNotebook.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExNotebook.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExNotebook.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExNotebook.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExNotebook.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExNotebook.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExNotebook.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
procedure TJvExNotebook.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}
  

{$IFDEF VCL}
procedure TJvExHeader.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExHeader.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExHeader.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExHeader.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExHeader.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExHeader.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExHeader.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExHeader.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExHeader.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExHeader.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExHeader.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExHeader.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExHeader.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExHeader.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExHeader.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExHeader.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExHeader.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExHeader.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExHeader.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExHeader.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExHeader.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExHeader.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExHeader.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExHeader.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExHeader.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
procedure TJvExHeader.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}
  
 {$IFDEF COMPILER6_UP}

{$IFDEF VCL}
procedure TJvExBoundLabel.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExBoundLabel.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExBoundLabel.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExBoundLabel.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExBoundLabel.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExBoundLabel.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExBoundLabel.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExBoundLabel.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExBoundLabel.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExBoundLabel.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExBoundLabel.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExBoundLabel.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExBoundLabel.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExBoundLabel.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExBoundLabel.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExBoundLabel.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExBoundLabel.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExBoundLabel.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}

{$IFDEF VCL}
procedure TJvExCustomLabeledEdit.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomLabeledEdit.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomLabeledEdit.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomLabeledEdit.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomLabeledEdit.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomLabeledEdit.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomLabeledEdit.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomLabeledEdit.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomLabeledEdit.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomLabeledEdit.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomLabeledEdit.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomLabeledEdit.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomLabeledEdit.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomLabeledEdit.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExCustomLabeledEdit.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExCustomLabeledEdit.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomLabeledEdit.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExCustomLabeledEdit.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomLabeledEdit.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomLabeledEdit.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomLabeledEdit.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomLabeledEdit.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExCustomLabeledEdit.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomLabeledEdit.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomLabeledEdit.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExCustomLabeledEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TJvExCustomLabeledEdit.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvExCustomLabeledEdit.Paint;
var
  Event: QPaintEventH;
begin
  Event := QPaintEvent_create(QPainter_clipRegion(FCanvas.Handle), False);
  try
    QObject_event(Handle, Event);
  finally
    QPaintEvent_destroy(Event);
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomLabeledEdit.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}
  

{$IFDEF VCL}
procedure TJvExLabeledEdit.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExLabeledEdit.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExLabeledEdit.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExLabeledEdit.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExLabeledEdit.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExLabeledEdit.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExLabeledEdit.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExLabeledEdit.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExLabeledEdit.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExLabeledEdit.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExLabeledEdit.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExLabeledEdit.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExLabeledEdit.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExLabeledEdit.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExLabeledEdit.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExLabeledEdit.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExLabeledEdit.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExLabeledEdit.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExLabeledEdit.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExLabeledEdit.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExLabeledEdit.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExLabeledEdit.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExLabeledEdit.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExLabeledEdit.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExLabeledEdit.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExLabeledEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TJvExLabeledEdit.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvExLabeledEdit.Paint;
var
  Event: QPaintEventH;
begin
  Event := QPaintEvent_create(QPainter_clipRegion(FCanvas.Handle), False);
  try
    QObject_event(Handle, Event);
  finally
    QPaintEvent_destroy(Event);
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExLabeledEdit.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}
  

{$IFDEF VCL}
procedure TJvExCustomColorBox.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomColorBox.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomColorBox.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomColorBox.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomColorBox.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomColorBox.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomColorBox.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomColorBox.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomColorBox.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomColorBox.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomColorBox.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomColorBox.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomColorBox.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomColorBox.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExCustomColorBox.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExCustomColorBox.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomColorBox.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExCustomColorBox.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomColorBox.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomColorBox.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomColorBox.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomColorBox.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExCustomColorBox.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomColorBox.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomColorBox.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExCustomColorBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TJvExCustomColorBox.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvExCustomColorBox.Paint;
var
  Event: QPaintEventH;
begin
  Event := QPaintEvent_create(QPainter_clipRegion(FCanvas.Handle), False);
  try
    QObject_event(Handle, Event);
  finally
    QPaintEvent_destroy(Event);
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomColorBox.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}
  

{$IFDEF VCL}
procedure TJvExColorBox.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExColorBox.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExColorBox.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExColorBox.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExColorBox.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExColorBox.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExColorBox.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExColorBox.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExColorBox.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExColorBox.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExColorBox.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExColorBox.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExColorBox.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExColorBox.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExColorBox.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}

 {$IF not declared(PatchedVCLX)}
procedure TJvExColorBox.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExColorBox.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExColorBox.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExColorBox.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExColorBox.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExColorBox.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExColorBox.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExColorBox.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExColorBox.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExColorBox.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExColorBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TJvExColorBox.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvExColorBox.Paint;
var
  Event: QPaintEventH;
begin
  Event := QPaintEvent_create(QPainter_clipRegion(FCanvas.Handle), False);
  try
    QObject_event(Handle, Event);
  finally
    QPaintEvent_destroy(Event);
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExColorBox.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}
  
 {$ENDIF COMPILER6_UP}
{$ENDIF VCL}

end.
