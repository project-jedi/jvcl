{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExComCtrls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvExComCtrls;

{$I jvcl.inc}
{MACROINCLUDE JvExControls.macros}

WARNINGHEADER

interface

uses
  {$IFDEF VCL}
  Windows, Messages,
  {$ENDIF VCL}
  Graphics, Controls, Forms, ComCtrls,
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

{$DEFINE ANIMATE}
{$IFDEF COMPILER6_UP}
 {$IF not declared(TAnimate)}
  {$UNDEF ANIMATE}
 {$IFEND}
{$ENDIF COMPILER6_UP}

type
  {$IFDEF COMPILER6_UP}
  JV_WINCONTROL_EVENTS(CustomHeaderControl)
  {$ENDIF COMPILER6_UP}

  JV_WINCONTROL_EVENTS(CustomTreeView)
  JV_WINCONTROL_EVENTS(CustomListView)
  JV_CUSTOMCONTROL_EVENTS(CustomTabControl)
  JV_WINCONTROL_EVENTS(HeaderControl)
  JV_CUSTOMCONTROL_EVENTS(PageControl)
  JV_WINCONTROL_EVENTS(TrackBar)

{$IFDEF VCL}
  {$IFDEF ANIMATE}
  JV_WINCONTROL_EVENTS(Animate)
  {$ENDIF ANIMATE}
  {$IFDEF COMPILER6_UP}
  JV_WINCONTROL_EVENTS(CustomComboBoxEx)
  JV_WINCONTROL_EVENTS(CustomStatusBar)
  JV_WINCONTROL_EVENTS(ComboBoxEx)
  {$ENDIF COMPILER6_UP}
  JV_WINCONTROL_EVENTS(CoolBar)

  JV_WINCONTROL_EVENTS(CommonCalendar)
  JV_WINCONTROL_EVENTS(MonthCalendar)

  JV_CUSTOMCONTROL_EVENTS(TabControl)

  JV_WINCONTROL_EVENTS(CustomHotKey)
  JV_WINCONTROL_EVENTS(HotKey)

  JV_WINCONTROL_EVENTS(CustomUpDown)
  JV_WINCONTROL_EVENTS(UpDown)

  JV_WINCONTROL_EVENTS(DateTimePicker)
  JV_WINCONTROL_EVENTS(ListView)
  JV_WINCONTROL_EVENTS(PageScroller)
  JV_WINCONTROL_EVENTS(ProgressBar)
  JV_WINCONTROL_EVENTS(StatusBar)
  JV_WINCONTROL_EVENTS(TabSheet)
  JV_WINCONTROL_EVENTS(ToolBar)
  JV_CONTROL_EVENTS(ToolButton)
  JV_WINCONTROL_EVENTS(TreeView)
{$ENDIF VCL}

{$IFDEF VisualCLX}
  {$IFDEF ANIMATE}
  JV_CUSTOMCONTROL_EVENTS(Animate)
  {$ENDIF ANIMATE}
  JV_WINCONTROL_EVENTS(CustomIconView)
  JV_WINCONTROL_EVENTS(CustomSpinEdit)
  JV_WINCONTROL_EVENTS(CustomViewControl)
  JV_CONTROL_EVENTS(ProgressBar)
  JV_CUSTOMCONTROL_EVENTS(TabSheet)
  JV_CUSTOMCONTROL_EVENTS(StatusBar)
  JV_CUSTOMCONTROL_EVENTS(ToolBar)
  JV_CUSTOMCONTROL_EVENTS(ToolButton)

  TJvExIconView = class(TJvExCustomIconView)
  public
    property SelCount;
    property Selected;
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property Height;
    property Hint;
    property IconOptions;
    property Images;
    property Items;
    property ItemWidth;
    property MultiSelect;
    property OwnerDraw;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property ShowToolTips;
    property Sort;
    property SortDirection;
    property Spacing;
    property TabOrder;
    property TabStop;
    property TextPosition;
    property Width;
    property Visible;
    property OnContextPopup;
    property OnCustomDrawItem;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnItemDoubleClick;
    property OnItemExitViewportEnter;
    property OnItemEnter;
    property OnEdited;
    property OnEditing;
    property OnItemClicked;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyString;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectItem;
    property OnStartDrag;
  end;

  TJvExSpinEdit = class(TJvExCustomSpinEdit)
  public
    property CleanText;
    property Text;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property ButtonStyle;
    property Constraints;
    property Color;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property Max;
    property Min;
    property Increment;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property Value;
    property Visible;
    property ParentFont;
    property ParentShowHint;
    property Prefix;
    property ShowHint;
    property SpecialText;
    property Suffix;
    property Wrap;
    property OnChanged;
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

  TJvExTabControl = class(TJvExCustomTabControl)
  published
    property Anchors;
    property Align;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property HotImages;
    property HotTrack;
    property HotTrackColor;
    property Images;
    property MultiLine;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RaggedRight;
    property ShowFrame;
    property ShowHint;
    property Style;
    property MultiSelect; { must be after Style due to streaming order }
    property TabHeight;
    property TabOrder;
    property TabStop;
    property Tabs;
    property TabIndex; { must be after Tabs due to streaming order }
    property TabWidth;
    property Visible;
    property OnChange;
    property OnChanged;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

  TJvExTreeView = class(TJvExCustomTreeView)
  public
    property SelCount;
    property SortColumn;
  published
    property Align;
    property Anchors;
    property AutoExpand default False;
    property BorderStyle default bsSunken3d;
    property Color;
    property ColumnClick default True;
    property ColumnMove default True;
    property ColumnResize default True;
    property Columns;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property Height;
    property Hint;
    property Images;
    property ItemMargin;
    property Items;
    property Indent;
    property MultiSelect default False;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RowSelect;
    property ShowColumnHeaders;
    property ShowColumnSortIndicators;
    property ShowButtons;
    property ShowHint;
    property Sorted default False;
    property SortType;
    property TabOrder;
    property TabStop;
    property Visible;
    property Width;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnResize;
    property OnContextPopup;
    property OnCustomDrawBranch;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanding;
    property OnExpanded;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnInsert;
    property OnItemClick;
    property OnItemDoubleClick;
    property OnItemEnter;
    property OnItemExitViewportEnter;
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
    property OnItemMouseDown;
    property OnViewPortMouseDown;
  end;
{$ENDIF VisualCLX}

implementation


JV_WINCONTROL_EVENTS_IMPL(CustomTreeView)
JV_WINCONTROL_EVENTS_IMPL(CustomListView)
JV_WINCONTROL_EVENTS_IMPL(HeaderControl)
JV_WINCONTROL_EVENTS_IMPL(TrackBar)

{$IFDEF COMPILER6_UP}
JV_WINCONTROL_EVENTS_IMPL(CustomHeaderControl)
{$ENDIF COMPILER6_UP}

{$IFDEF VCL}
{$IFDEF ANIMATE}
JV_WINCONTROL_EVENTS_IMPL(Animate)
{$ENDIF ANIMATE}
{$IFDEF COMPILER6_UP}
JV_WINCONTROL_EVENTS_IMPL(CustomComboBoxEx)
JV_WINCONTROL_EVENTS_IMPL(CustomStatusBar)
JV_WINCONTROL_EVENTS_IMPL(ComboBoxEx)
{$ENDIF COMPILER6_UP}
JV_WINCONTROL_EVENTS_IMPL(CoolBar)

JV_WINCONTROL_EVENTS_IMPL(CommonCalendar)
JV_WINCONTROL_EVENTS_IMPL(MonthCalendar)

JV_WINCONTROL_EVENTS_IMPL(CustomHotKey)
JV_WINCONTROL_EVENTS_IMPL(HotKey)

JV_WINCONTROL_EVENTS_IMPL(CustomUpDown)
JV_WINCONTROL_EVENTS_IMPL(UpDown)

JV_WINCONTROL_EVENTS_IMPL(CustomTabControl)
JV_WINCONTROL_EVENTS_IMPL(DateTimePicker)

JV_WINCONTROL_EVENTS_IMPL(ListView)
JV_WINCONTROL_EVENTS_IMPL(ProgressBar)
JV_WINCONTROL_EVENTS_IMPL(PageControl)
JV_WINCONTROL_EVENTS_IMPL(PageScroller)
JV_WINCONTROL_EVENTS_IMPL(TabControl)
JV_WINCONTROL_EVENTS_IMPL(TabSheet)
JV_WINCONTROL_EVENTS_IMPL(ToolBar)
JV_WINCONTROL_EVENTS_IMPL(TreeView)
JV_WINCONTROL_EVENTS_IMPL(StatusBar)
JV_CONTROL_EVENTS_IMPL(ToolButton)
{$ENDIF VCL}

{$IFDEF VisualCLX}
{$IFDEF ANIMATE}
JV_CUSTOMCONTROL_EVENTS_IMPL(Animate)
{$ENDIF ANIMATE}
JV_CONTROL_EVENTS_IMPL(ProgressBar)
JV_CUSTOMCONTROL_EVENTS_IMPL(CustomTabControl)
JV_CUSTOMCONTROL_EVENTS_IMPL(TabSheet)
JV_CUSTOMCONTROL_EVENTS_IMPL(PageControl)
JV_CUSTOMCONTROL_EVENTS_IMPL(StatusBar)
JV_WINCONTROL_EVENTS_IMPL(CustomIconView)
JV_WINCONTROL_EVENTS_IMPL(CustomViewControl)
JV_CUSTOMCONTROL_EVENTS_IMPL(ToolBar)
JV_CUSTOMCONTROL_EVENTS_IMPL(ToolButton)
JV_WINCONTROL_EVENTS_IMPL(CustomSpinEdit)
{$ENDIF VisualCLX}


end.
