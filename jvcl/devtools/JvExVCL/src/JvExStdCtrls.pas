{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExStdCtrls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExStdCtrls;

{$I jvcl.inc}
{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  {$IFDEF VCL}
  Windows, Messages,
  {$ENDIF VCL}
  Graphics, Controls, Forms, StdCtrls,
  {$IFDEF VisualCLX}
  Qt, QWindows,
  {$ENDIF VisualCLX}
  Classes, SysUtils,
  JvTypes, JvThemes, JVCLVer, JvExControls;

{$IFDEF VCL}
 {$DEFINE NeedMouseEnterLeave}
{$ENDIF VCL}
{$IFDEF VisualCLX}
 {$IF not declared(PatchedVCLX)}
  {$DEFINE NeedMouseEnterLeave}
 {$IFEND}
{$ENDIF VisualCLX}

type
  JV_WINCONTROL_EVENTS(CustomGroupBox)
{$DEFINE HASAUTOSIZE}
  {$IFDEF VCL}
  JV_CONTROL_EVENTS(CustomLabel)
  JV_CONTROL_EVENTS(Label)
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  JV_WINCONTROL_EVENTS(CustomLabel)
  {$ENDIF VisualCLX}
{$UNDEF HASAUTOSIZE}
  JV_EDITCONTROL_EVENTS(CustomEdit)
  JV_EDITCONTROL_EVENTS(CustomMemo)
  {$IFDEF VCL}
  {$IFDEF COMPILER6_UP}
  JV_WINCONTROL_EVENTS(CustomCombo)
  {$ENDIF COMPILER6_UP}
  {$ENDIF VCL}
  JV_WINCONTROL_EVENTS(CustomComboBox)
  JV_WINCONTROL_EVENTS(ButtonControl)
  JV_WINCONTROL_EVENTS(Button)
  JV_WINCONTROL_EVENTS(CustomCheckBox)
  JV_WINCONTROL_EVENTS(RadioButton)
  JV_WINCONTROL_EVENTS(CustomListBox)
  JV_WINCONTROL_EVENTS(ScrollBar)
  {$IFDEF VCL}
  JV_WINCONTROL_EVENTS(GroupBox)
  JV_WINCONTROL_EVENTS(CheckBox)
  JV_WINCONTROL_EVENTS(CustomStaticText)
  JV_WINCONTROL_EVENTS(StaticText)
  {$ENDIF VCL}
  
  {$IFDEF VisualCLX}

  TJvExCheckBox = class(TJvExCustomCheckBox)
  published
    property Action;
    property AllowGrayed;
    property Anchors;
    property Bitmap;
    property Caption;
    property Checked;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property Masked default False;
    property ParentColor default True;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
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

  TJvExGroupBox = class(TJvExCustomGroupBox)
  published
    property Align default alNone;
    property Alignment;
    property Anchors;
    property Bitmap;
    property BorderStyle default bsEtched;
    property Caption;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property Masked default False;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default False;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TJvExLabel = class(TJvExCustomLabel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property Bitmap;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property Masked;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TJvExListBox = class(TJvExCustomListBox)
  published
    property Style; { Must be published before Items }
    property RowLayout; { Must be published before Rows }
    property ColumnLayout; { Must be published before Columns }
    property Align;
    property Anchors;
    property BorderStyle;
    property Color default clBase;
    property Columns;
    property Constraints;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Rows;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnDrawItem;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyString;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TJvExMemo = class(TJvExCustomMemo)
  published
    property Align;
    property Alignment;
    property Anchors;
    property BorderStyle;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property HMargin;
    property Lines;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UndoLevels;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;
    property WrapAtValue;
    property WrapBreak;
    property WrapMode;
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

  {$ENDIF VisualCLX}

implementation

JV_WINCONTROL_EVENTS_IMPL(CustomGroupBox)
{$DEFINE HASAUTOSIZE}
{$IFDEF VCL}
JV_CONTROL_EVENTS_IMPL(CustomLabel)
JV_CONTROL_EVENTS_IMPL(Label)
{$ENDIF VCL}
{$IFDEF VisualCLX}
JV_WINCONTROL_EVENTS_IMPL(CustomLabel)
{$ENDIF VisualCLX}
{$UNDEF HASAUTOSIZE}

{$UNDEF CONSTRUCTOR_CODE}
{$DEFINE CONSTRUCTOR_CODE
  FClipboardCommands := [caCopy..caUndo];
}
JV_EDITCONTROL_EVENTS_IMPL(CustomEdit)
JV_EDITCONTROL_EVENTS_IMPL(CustomMemo)
{$UNDEF CONSTRUCTOR_CODE}
{$DEFINE CONSTRUCTOR_CODE}

{$IFDEF VCL}
{$IFDEF COMPILER6_UP}
JV_WINCONTROL_EVENTS_IMPL(CustomCombo)
{$ENDIF COMPILER6_UP}
{$ENDIF VCL}
JV_WINCONTROL_EVENTS_IMPL(CustomComboBox)
JV_WINCONTROL_EVENTS_IMPL(ButtonControl)
JV_WINCONTROL_EVENTS_IMPL(Button)
JV_WINCONTROL_EVENTS_IMPL(CustomCheckBox)
JV_WINCONTROL_EVENTS_IMPL(RadioButton)
JV_WINCONTROL_EVENTS_IMPL(CustomListBox)
JV_WINCONTROL_EVENTS_IMPL(ScrollBar)
{$IFDEF VCL}
JV_WINCONTROL_EVENTS_IMPL(GroupBox)
JV_WINCONTROL_EVENTS_IMPL(CheckBox)
JV_WINCONTROL_EVENTS_IMPL(CustomStaticText)
JV_WINCONTROL_EVENTS_IMPL(StaticText)
{$ENDIF VCL}

{$UNDEF CONSTRUCTOR_CODE} // undefine at file end

end.
