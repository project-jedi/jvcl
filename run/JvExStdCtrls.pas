{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExStdCtrls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas.Hausladen@gmx.de]
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Last Modified: 2004-01-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I jvcl.inc}

unit JvExStdCtrls;
interface
uses
  {$IFDEF VCL}
  Windows, Messages, Controls, Forms, StdCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls, QForms, QStdCtrls,
  {$ENDIF VisualCLX}
  Classes, SysUtils,
  JvExControls;

type
  //
  // --------------------------------------
  TJvExCustomGroupBox = class(TCustomGroupBox, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
  //
  // --------------------------------------
  TJvExGroupBox = class(TGroupBox, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
  //
  // --------------------------------------
  TJvExCustomLabel = class(TCustomLabel, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
  //
  // --------------------------------------
  TJvExLabel = class(TLabel, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
  //
  // --------------------------------------
  TJvExCustomEdit = class(TCustomEdit, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
  //
  // --------------------------------------
  TJvExEdit = class(TEdit, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
  //
  // --------------------------------------
  TJvExCustomMemo = class(TCustomMemo, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
  //
  // --------------------------------------
  TJvExMemo = class(TMemo, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
{$IFDEF COMPILER6_UP}
 {$IFDEF VCL}
  //
  // --------------------------------------
  TJvExCustomCombo = class(TCustomCombo, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
 {$ENDIF VCL}
{$ENDIF}
  //
  // --------------------------------------
  TJvExCustomComboBox = class(TCustomComboBox, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
  //
  // --------------------------------------
  TJvExComboBox = class(TComboBox, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
  //
  // --------------------------------------
  TJvExButtonControl = class(TButtonControl, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
  //
  // --------------------------------------
  TJvExButton = class(TButton, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
  //
  // --------------------------------------
  TJvExCustomCheckBox = class(TCustomCheckBox, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
  //
  // --------------------------------------
  TJvExCheckBox = class(TCheckBox, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
  //
  // --------------------------------------
  TJvExRadioButton = class(TRadioButton, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
  //
  // --------------------------------------
  TJvExCustomListBox = class(TCustomListBox, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
  //
  // --------------------------------------
  TJvExListBox = class(TListBox, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
  //
  // --------------------------------------
  TJvExScrollBar = class(TScrollBar, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
{$IFDEF VCL}
  //
  // --------------------------------------
  TJvExCustomStaticText = class(TCustomStaticText, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
  //
  // --------------------------------------
  TJvExStaticText = class(TStaticText, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
   // TControl
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
  protected
   // TWinControl
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  public
    procedure Dispatch(var Message); override;
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  {$ENDIF}
  end;
{$ENDIF VCL}

implementation

//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExCustomGroupBox.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomGroupBox.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomGroupBox.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomGroupBox.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomGroupBox.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomGroupBox.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomGroupBox.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomGroupBox.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomGroupBox.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomGroupBox.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomGroupBox.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomGroupBox.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomGroupBox.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExCustomGroupBox.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomGroupBox.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomGroupBox.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomGroupBox.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomGroupBox.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomGroupBox.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExCustomGroupBox.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExCustomGroupBox.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExGroupBox.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExGroupBox.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExGroupBox.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExGroupBox.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExGroupBox.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExGroupBox.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExGroupBox.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExGroupBox.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExGroupBox.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExGroupBox.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExGroupBox.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExGroupBox.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExGroupBox.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExGroupBox.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExGroupBox.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExGroupBox.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExGroupBox.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExGroupBox.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExGroupBox.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExGroupBox.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExGroupBox.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExCustomLabel.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomLabel.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomLabel.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomLabel.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomLabel.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomLabel.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomLabel.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomLabel.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomLabel.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomLabel.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomLabel.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomLabel.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomLabel.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExCustomLabel.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomLabel.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomLabel.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExLabel.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExLabel.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExLabel.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExLabel.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExLabel.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExLabel.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExLabel.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExLabel.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExLabel.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExLabel.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExLabel.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExLabel.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExLabel.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExLabel.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExLabel.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExLabel.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExCustomEdit.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomEdit.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomEdit.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomEdit.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomEdit.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomEdit.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomEdit.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomEdit.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomEdit.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomEdit.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomEdit.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomEdit.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomEdit.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExCustomEdit.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomEdit.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomEdit.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomEdit.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomEdit.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomEdit.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExCustomEdit.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExCustomEdit.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExEdit.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExEdit.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExEdit.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExEdit.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExEdit.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExEdit.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExEdit.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExEdit.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExEdit.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExEdit.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExEdit.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExEdit.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExEdit.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExEdit.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExEdit.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExEdit.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExEdit.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExEdit.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExEdit.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExEdit.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExEdit.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExCustomMemo.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomMemo.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomMemo.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomMemo.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomMemo.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomMemo.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomMemo.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomMemo.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomMemo.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomMemo.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomMemo.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomMemo.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomMemo.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExCustomMemo.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomMemo.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomMemo.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomMemo.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomMemo.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomMemo.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExCustomMemo.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExCustomMemo.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExMemo.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExMemo.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExMemo.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExMemo.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExMemo.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExMemo.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExMemo.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExMemo.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExMemo.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExMemo.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExMemo.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExMemo.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExMemo.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExMemo.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExMemo.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExMemo.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExMemo.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExMemo.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExMemo.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExMemo.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExMemo.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
{$IFDEF COMPILER6_UP}
 {$IFDEF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExCustomCombo.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomCombo.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomCombo.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomCombo.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomCombo.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomCombo.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomCombo.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomCombo.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomCombo.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomCombo.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomCombo.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomCombo.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomCombo.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExCustomCombo.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomCombo.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomCombo.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomCombo.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomCombo.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomCombo.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExCustomCombo.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExCustomCombo.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
 {$ENDIF VCL}
{$ENDIF}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExCustomComboBox.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomComboBox.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomComboBox.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomComboBox.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomComboBox.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomComboBox.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomComboBox.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomComboBox.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomComboBox.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomComboBox.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomComboBox.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomComboBox.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomComboBox.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExCustomComboBox.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomComboBox.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomComboBox.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomComboBox.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomComboBox.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomComboBox.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExCustomComboBox.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExCustomComboBox.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExComboBox.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExComboBox.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExComboBox.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExComboBox.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExComboBox.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExComboBox.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExComboBox.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExComboBox.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExComboBox.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExComboBox.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExComboBox.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExComboBox.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExComboBox.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExComboBox.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExComboBox.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExComboBox.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExComboBox.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExComboBox.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExComboBox.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExComboBox.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExComboBox.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExButtonControl.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExButtonControl.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExButtonControl.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExButtonControl.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExButtonControl.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExButtonControl.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExButtonControl.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExButtonControl.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExButtonControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExButtonControl.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExButtonControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExButtonControl.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExButtonControl.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExButtonControl.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExButtonControl.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExButtonControl.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExButtonControl.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExButtonControl.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExButtonControl.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExButtonControl.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExButtonControl.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExButton.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExButton.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExButton.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExButton.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExButton.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExButton.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExButton.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExButton.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExButton.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExButton.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExButton.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExButton.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExButton.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExButton.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExButton.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExButton.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExButton.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExButton.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExButton.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExButton.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExButton.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExCustomCheckBox.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomCheckBox.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomCheckBox.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomCheckBox.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomCheckBox.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomCheckBox.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomCheckBox.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomCheckBox.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomCheckBox.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomCheckBox.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomCheckBox.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomCheckBox.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomCheckBox.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExCustomCheckBox.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomCheckBox.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomCheckBox.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomCheckBox.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomCheckBox.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomCheckBox.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExCustomCheckBox.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExCustomCheckBox.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExCheckBox.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCheckBox.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCheckBox.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCheckBox.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCheckBox.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCheckBox.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCheckBox.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCheckBox.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCheckBox.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCheckBox.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCheckBox.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCheckBox.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCheckBox.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExCheckBox.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCheckBox.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCheckBox.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCheckBox.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCheckBox.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCheckBox.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExCheckBox.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExCheckBox.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExRadioButton.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExRadioButton.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExRadioButton.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExRadioButton.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExRadioButton.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExRadioButton.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExRadioButton.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExRadioButton.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExRadioButton.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExRadioButton.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExRadioButton.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExRadioButton.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExRadioButton.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExRadioButton.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExRadioButton.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExRadioButton.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExRadioButton.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExRadioButton.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExRadioButton.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExRadioButton.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExRadioButton.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExCustomListBox.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomListBox.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomListBox.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomListBox.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomListBox.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomListBox.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomListBox.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomListBox.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomListBox.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomListBox.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomListBox.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomListBox.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomListBox.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExCustomListBox.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomListBox.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomListBox.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomListBox.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomListBox.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomListBox.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExCustomListBox.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExCustomListBox.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExListBox.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExListBox.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExListBox.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExListBox.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExListBox.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExListBox.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExListBox.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExListBox.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExListBox.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExListBox.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExListBox.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExListBox.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExListBox.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExListBox.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExListBox.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExListBox.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExListBox.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExListBox.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExListBox.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExListBox.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExListBox.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExScrollBar.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExScrollBar.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExScrollBar.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExScrollBar.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExScrollBar.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExScrollBar.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExScrollBar.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExScrollBar.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExScrollBar.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExScrollBar.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExScrollBar.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExScrollBar.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExScrollBar.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExScrollBar.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExScrollBar.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExScrollBar.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExScrollBar.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExScrollBar.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExScrollBar.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExScrollBar.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExScrollBar.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
{$IFDEF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExCustomStaticText.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomStaticText.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomStaticText.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomStaticText.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomStaticText.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomStaticText.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomStaticText.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomStaticText.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomStaticText.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomStaticText.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomStaticText.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomStaticText.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomStaticText.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExCustomStaticText.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomStaticText.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomStaticText.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomStaticText.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomStaticText.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomStaticText.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExCustomStaticText.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExCustomStaticText.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
//
// -----------------------------------------------------------------------------
{$IFDEF VCL}
procedure TJvExStaticText.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExStaticText.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExStaticText.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExStaticText.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExStaticText.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExStaticText.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExStaticText.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExStaticText.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExStaticText.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExStaticText.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExStaticText.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExStaticText.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExStaticText.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
procedure TJvExStaticText.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExStaticText.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExStaticText.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExStaticText.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExStaticText.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExStaticText.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExStaticText.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;
{$ENDIF VCL}
{$IFDEF VCL}
procedure TJvExStaticText.Dispatch(var Message);
begin
  if not DispatchMsg(Self, Message) then
    inherited Dispatch(Message);
end;
{$ENDIF VCL}
{$ENDIF VCL}

end.
