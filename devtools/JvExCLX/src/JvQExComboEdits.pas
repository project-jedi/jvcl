{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExComboEdits.pas, released on 2004-01-19

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQExComboEdits;

{$I jvcl.inc}
{MACROINCLUDE JvQExControls.macros}

WARNINGHEADER

interface

uses
  QGraphics, QControls, QForms, QExtCtrls, QComboEdits,
  Qt, QWindows, QMessages,
  Classes, SysUtils,
  JvQTypes, JvQThemes, JVCLXVer, JvQExControls;


type
  JV_EDITCONTROL(CustomComboEdit)

  JV_EDITCONTROL_BEGIN(CustomComboMaskEdit)
  private
    FBeepOnError: Boolean;
  protected
    procedure DoBeepOnError; dynamic;
    procedure SetBeepOnError(Value: Boolean); virtual;
    property BeepOnError: Boolean read FBeepOnError write SetBeepOnError default True;
  JV_EDITCONTROL_END(CustomComboMaskEdit)

  TJvExComboEdit = class(TJvExCustomComboEdit)
  published
    property Flat default False;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property ClientColor default clBase;
    property EditClientColor default True;
  public
    property CursorPos;
  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property Alignment;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyString;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnReturnPressed;
    property OnStartDrag;
  end;

  TJvExComboMaskEdit = class(TJvExCustomComboMaskEdit)
  published
    property Flat default False;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property ClientColor default clBase;
    property EditClientColor default True;
  published
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BeepOnError;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property EditMask;
    property Font;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyString;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;


implementation

{$UNDEF CREATE_CUSTOMCODE}
{$DEFINE CREATE_CUSTOMCODE
  FClipboardCommands := [caCopy..caUndo];
}
JV_EDITCONTROL_IMPL(CustomComboEdit)

{ The CREATE_CUSTOMCODE macro is used to extend the constructor by the macro
  content. }
{$UNDEF CREATE_CUSTOMCODE}
{$DEFINE CREATE_CUSTOMCODE
  FBeepOnError := True;
  FClipboardCommands := [caCopy..caUndo];
}
procedure TJvExCustomComboMaskEdit.DoBeepOnError;
begin
  if BeepOnError then
    SysUtils.Beep;
end;

procedure TJvExCustomComboMaskEdit.SetBeepOnError(Value: Boolean);
begin
  FBeepOnError := Value;
end;

JV_EDITCONTROL_IMPL(CustomComboMaskEdit)

{$UNDEF CREATE_CUSTOMCODE} // undefine at file end
end.
