{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_ComCtrls.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQInterpreter_ComCtrls;

interface

uses
  JvQInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  QWindows, Classes, QControls, QGraphics, QComCtrls,
  JvQInterpreter_Windows;

{ TTabControl }

{ constructor Create(AOwner: TComponent) }

procedure TTabControl_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTabControl.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TTabSheet }

{ constructor Create(AOwner: TComponent) }

procedure TTabSheet_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTabSheet.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read PageControl: TPageControl }

procedure TTabSheet_Read_PageControl(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTabSheet(Args.Obj).PageControl);
end;

{ property Write PageControl(Value: TPageControl) }

procedure TTabSheet_Write_PageControl(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTabSheet(Args.Obj).PageControl := V2O(Value) as TPageControl;
end;

{ property Read TabIndex: Integer }

procedure TTabSheet_Read_TabIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTabSheet(Args.Obj).TabIndex;
end;

{ TPageControl }

{ constructor Create(AOwner: TComponent) }

procedure TPageControl_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TPageControl.Create(V2O(Args.Values[0]) as TComponent));
end;

{ function FindNextPage(CurPage: TTabSheet; GoForward, CheckTabVisible: Boolean): TTabSheet; }

procedure TPageControl_FindNextPage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TPageControl(Args.Obj).FindNextPage(V2O(Args.Values[0]) as TTabSheet, Args.Values[1], Args.Values[2]));
end;

{ procedure SelectNextPage(GoForward: Boolean); }

procedure TPageControl_SelectNextPage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TPageControl(Args.Obj).SelectNextPage(Args.Values[0]);
end;

{ property Read PageCount: Integer }

procedure TPageControl_Read_PageCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TPageControl(Args.Obj).PageCount;
end;

{ property Read Pages[Integer]: TTabSheet }

procedure TPageControl_Read_Pages(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TPageControl(Args.Obj).Pages[Args.Values[0]]);
end;

{ TStatusPanel }

{ constructor Create(Collection: TCollection) }

procedure TStatusPanel_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStatusPanel.Create(V2O(Args.Values[0]) as TCollection));
end;

{ procedure Assign(Source: TPersistent); }

procedure TStatusPanel_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TStatusPanel(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ TStatusPanels }

{ constructor Create(StatusBar: TStatusBar) }

procedure TStatusPanels_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStatusPanels.Create(V2O(Args.Values[0]) as TStatusBar));
end;

{ function Add: TStatusPanel; }

procedure TStatusPanels_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStatusPanels(Args.Obj).Add);
end;

{ property Read Items[Integer]: TStatusPanel }

procedure TStatusPanels_Read_Items(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStatusPanels(Args.Obj).Items[Args.Values[0]]);
end;

{ property Write Items[Integer]: TStatusPanel }

procedure TStatusPanels_Write_Items(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TStatusPanels(Args.Obj).Items[Args.Values[0]] := V2O(Value) as TStatusPanel;
end;

{ TStatusBar }

{ constructor Create(AOwner: TComponent) }

procedure TStatusBar_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStatusBar.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Canvas: TCanvas }

procedure TStatusBar_Read_Canvas(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStatusBar(Args.Obj).Canvas);
end;

{ THeaderSection }

{ constructor Create(Collection: TCollection) }

procedure THeaderSection_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(THeaderSection.Create(V2O(Args.Values[0]) as TCollection));
end;

{ procedure Assign(Source: TPersistent); }

procedure THeaderSection_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  THeaderSection(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ property Read Left: Integer }

procedure THeaderSection_Read_Left(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THeaderSection(Args.Obj).Left;
end;

{ property Read Right: Integer }

procedure THeaderSection_Read_Right(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THeaderSection(Args.Obj).Right;
end;

{ THeaderSections }

{ constructor Create(HeaderControl: THeaderControl) }

procedure THeaderSections_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(THeaderSections.Create(V2O(Args.Values[0]) as THeaderControl));
end;

{ function Add: THeaderSection; }

procedure THeaderSections_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(THeaderSections(Args.Obj).Add);
end;

{ property Read Items[Integer]: THeaderSection }

procedure THeaderSections_Read_Items(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(THeaderSections(Args.Obj).Items[Args.Values[0]]);
end;

{ property Write Items[Integer]: THeaderSection }

procedure THeaderSections_Write_Items(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THeaderSections(Args.Obj).Items[Args.Values[0]] := V2O(Value) as THeaderSection;
end;

{ THeaderControl }

{ constructor Create(AOwner: TComponent) }

procedure THeaderControl_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(THeaderControl.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Canvas: TCanvas }

procedure THeaderControl_Read_Canvas(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(THeaderControl(Args.Obj).Canvas);
end;

{ TTreeNode }

{ constructor Create(AOwner: TTreeNodes) }

procedure TTreeNode_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNode.Create(V2O(Args.Values[0]) as TTreeNodes));
end;

{ function AlphaSort: Boolean; }

procedure TTreeNode_AlphaSort(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).AlphaSort;
end;

{ procedure Assign(Source: TPersistent); }

procedure TTreeNode_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ procedure Collapse(Recurse: Boolean); }

procedure TTreeNode_Collapse(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).Collapse(Args.Values[0]);
end;

{ function CustomSort(SortProc: TTVCompare; Data: Longint): Boolean; }

procedure TTreeNode_CustomSort(var Value: Variant; Args: TJvInterpreterArgs);
begin
//  Value := TTreeNode(Args.Obj).CustomSort(Args.Values[0], Args.Values[1]);
  NotImplemented('TTreeNode.CustomSort');
end;

{ procedure Delete; }

procedure TTreeNode_Delete(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).Delete;
end;

{ procedure DeleteChildren; }

procedure TTreeNode_DeleteChildren(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).DeleteChildren;
end;

{ function DisplayRect(TextOnly: Boolean): TRect; }

procedure TTreeNode_DisplayRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Rect2Var(TTreeNode(Args.Obj).DisplayRect(Args.Values[0]));
end;

{ function EditText: Boolean; }

procedure TTreeNode_EditText(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).EditText;
end;

{ procedure EndEdit(Cancel: Boolean); }

procedure TTreeNode_EndEdit(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).EndEdit(Args.Values[0]);
end;

{ procedure Expand(Recurse: Boolean); }

procedure TTreeNode_Expand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).Expand(Args.Values[0]);
end;

{ function GetFirstChild: TTreeNode; }

procedure TTreeNode_GetFirstChild(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNode(Args.Obj).GetFirstChild);
end;

{ function GetHandle: HWND; }

procedure TTreeNode_GetHandle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TTreeNode(Args.Obj).GetHandle);
end;

{ function GetLastChild: TTreeNode; }

procedure TTreeNode_GetLastChild(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNode(Args.Obj).GetLastChild);
end;

{ function GetNext: TTreeNode; }

procedure TTreeNode_GetNext(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNode(Args.Obj).GetNext);
end;

{ function GetNextChild(Value: TTreeNode): TTreeNode; }

procedure TTreeNode_GetNextChild(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNode(Args.Obj).GetNextChild(V2O(Args.Values[0]) as TTreeNode));
end;

{ function GetNextSibling: TTreeNode; }

procedure TTreeNode_GetNextSibling(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNode(Args.Obj).GetNextSibling);
end;

{ function GetNextVisible: TTreeNode; }

procedure TTreeNode_GetNextVisible(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNode(Args.Obj).GetNextVisible);
end;

{ function GetPrev: TTreeNode; }

procedure TTreeNode_GetPrev(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNode(Args.Obj).GetPrev);
end;

{ function GetPrevChild(Value: TTreeNode): TTreeNode; }

procedure TTreeNode_GetPrevChild(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNode(Args.Obj).GetPrevChild(V2O(Args.Values[0]) as TTreeNode));
end;

{ function GetPrevSibling: TTreeNode; }

procedure TTreeNode_GetPrevSibling(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNode(Args.Obj).GetPrevSibling);
end;

{ function GetPrevVisible: TTreeNode; }

procedure TTreeNode_GetPrevVisible(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNode(Args.Obj).GetPrevVisible);
end;

{ function HasAsParent(Value: TTreeNode): Boolean; }

procedure TTreeNode_HasAsParent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).HasAsParent(V2O(Args.Values[0]) as TTreeNode);
end;

{ function IndexOf(Value: TTreeNode): Integer; }

procedure TTreeNode_IndexOf(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).IndexOf(V2O(Args.Values[0]) as TTreeNode);
end;

{ procedure MakeVisible; }

procedure TTreeNode_MakeVisible(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).MakeVisible;
end;

{ procedure MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode); }

procedure TTreeNode_MoveTo(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).MoveTo(V2O(Args.Values[0]) as TTreeNode, Args.Values[1]);
end;

{ property Read AbsoluteIndex: Integer }

procedure TTreeNode_Read_AbsoluteIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).AbsoluteIndex;
end;

{ property Read Count: Integer }

procedure TTreeNode_Read_Count(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).Count;
end;

{ property Read Cut: Boolean }

procedure TTreeNode_Read_Cut(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).Cut;
end;

{ property Write Cut(Value: Boolean) }

procedure TTreeNode_Write_Cut(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).Cut := Value;
end;

{ property Read Data: Pointer }

procedure TTreeNode_Read_Data(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TTreeNode(Args.Obj).Data);
end;

{ property Write Data(Value: Pointer) }

procedure TTreeNode_Write_Data(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).Data := V2P(Value);
end;

{ property Read Deleting: Boolean }

procedure TTreeNode_Read_Deleting(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).Deleting;
end;

{ property Read Focused: Boolean }

procedure TTreeNode_Read_Focused(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).Focused;
end;

{ property Write Focused(Value: Boolean) }

procedure TTreeNode_Write_Focused(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).Focused := Value;
end;

{ property Read DropTarget: Boolean }

procedure TTreeNode_Read_DropTarget(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).DropTarget;
end;

{ property Write DropTarget(Value: Boolean) }

procedure TTreeNode_Write_DropTarget(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).DropTarget := Value;
end;

{ property Read Selected: Boolean }

procedure TTreeNode_Read_Selected(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).Selected;
end;

{ property Write Selected(Value: Boolean) }

procedure TTreeNode_Write_Selected(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).Selected := Value;
end;

{ property Read Expanded: Boolean }

procedure TTreeNode_Read_Expanded(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).Expanded;
end;

{ property Write Expanded(Value: Boolean) }

procedure TTreeNode_Write_Expanded(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).Expanded := Value;
end;

{ property Read Handle: HWND }

procedure TTreeNode_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TTreeNode(Args.Obj).Handle);
end;

{ property Read HasChildren: Boolean }

procedure TTreeNode_Read_HasChildren(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).HasChildren;
end;

{ property Write HasChildren(Value: Boolean) }

procedure TTreeNode_Write_HasChildren(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).HasChildren := Value;
end;

{ property Read ImageIndex: Integer }

procedure TTreeNode_Read_ImageIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).ImageIndex;
end;

{ property Write ImageIndex(Value: Integer) }

procedure TTreeNode_Write_ImageIndex(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).ImageIndex := Value;
end;

{ property Read Index: Integer }

procedure TTreeNode_Read_Index(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).Index;
end;

{ property Read IsVisible: Boolean }

procedure TTreeNode_Read_IsVisible(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).IsVisible;
end;

{ property Read Item[Integer]: TTreeNode }

procedure TTreeNode_Read_Item(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNode(Args.Obj).Item[Args.Values[0]]);
end;

{ property Write Item[Integer]: TTreeNode }

procedure TTreeNode_Write_Item(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).Item[Args.Values[0]] := V2O(Value) as TTreeNode;
end;

{ property Read ItemId: HTreeItem }

procedure TTreeNode_Read_ItemId(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TTreeNode(Args.Obj).ItemId);
end;

{ property Read Level: Integer }

procedure TTreeNode_Read_Level(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).Level;
end;

{ property Read OverlayIndex: Integer }

procedure TTreeNode_Read_OverlayIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).OverlayIndex;
end;

{ property Write OverlayIndex(Value: Integer) }

procedure TTreeNode_Write_OverlayIndex(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).OverlayIndex := Value;
end;

{ property Read Owner: TTreeNodes }

procedure TTreeNode_Read_Owner(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNode(Args.Obj).Owner);
end;

{ property Read Parent: TTreeNode }

procedure TTreeNode_Read_Parent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNode(Args.Obj).Parent);
end;

{ property Read SelectedIndex: Integer }

procedure TTreeNode_Read_SelectedIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).SelectedIndex;
end;

{ property Write SelectedIndex(Value: Integer) }

procedure TTreeNode_Write_SelectedIndex(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).SelectedIndex := Value;
end;

{ property Read StateIndex: Integer }

procedure TTreeNode_Read_StateIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).StateIndex;
end;

{ property Write StateIndex(Value: Integer) }

procedure TTreeNode_Write_StateIndex(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).StateIndex := Value;
end;

{ property Read Text: string }

procedure TTreeNode_Read_Text(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNode(Args.Obj).Text;
end;

{ property Write Text(Value: string) }

procedure TTreeNode_Write_Text(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNode(Args.Obj).Text := Value;
end;

{ property Read TreeView: TCustomTreeView }

procedure TTreeNode_Read_TreeView(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNode(Args.Obj).TreeView);
end;

{ TTreeNodes }

{ constructor Create(AOwner: TCustomTreeView) }

procedure TTreeNodes_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNodes.Create(V2O(Args.Values[0]) as TCustomTreeView));
end;

{ function AddChildFirst(Node: TTreeNode; const S: string): TTreeNode; }

procedure TTreeNodes_AddChildFirst(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNodes(Args.Obj).AddChildFirst(V2O(Args.Values[0]) as TTreeNode, Args.Values[1]));
end;

{ function AddChild(Node: TTreeNode; const S: string): TTreeNode; }

procedure TTreeNodes_AddChild(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNodes(Args.Obj).AddChild(V2O(Args.Values[0]) as TTreeNode, Args.Values[1]));
end;

{ function AddChildObjectFirst(Node: TTreeNode; const S: string; Ptr: Pointer): TTreeNode; }

procedure TTreeNodes_AddChildObjectFirst(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNodes(Args.Obj).AddChildObjectFirst(V2O(Args.Values[0]) as TTreeNode, Args.Values[1],
    V2P(Args.Values[2])));
end;

{ function AddChildObject(Node: TTreeNode; const S: string; Ptr: Pointer): TTreeNode; }

procedure TTreeNodes_AddChildObject(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNodes(Args.Obj).AddChildObject(V2O(Args.Values[0]) as TTreeNode, Args.Values[1],
    V2P(Args.Values[2])));
end;

{ function AddFirst(Node: TTreeNode; const S: string): TTreeNode; }

procedure TTreeNodes_AddFirst(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNodes(Args.Obj).AddFirst(V2O(Args.Values[0]) as TTreeNode, Args.Values[1]));
end;

{ function Add(Node: TTreeNode; const S: string): TTreeNode; }

procedure TTreeNodes_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNodes(Args.Obj).Add(V2O(Args.Values[0]) as TTreeNode, Args.Values[1]));
end;

{ function AddObjectFirst(Node: TTreeNode; const S: string; Ptr: Pointer): TTreeNode; }

procedure TTreeNodes_AddObjectFirst(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNodes(Args.Obj).AddObjectFirst(V2O(Args.Values[0]) as TTreeNode, Args.Values[1],
    V2P(Args.Values[2])));
end;

{ function AddObject(Node: TTreeNode; const S: string; Ptr: Pointer): TTreeNode; }

procedure TTreeNodes_AddObject(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNodes(Args.Obj).AddObject(V2O(Args.Values[0]) as TTreeNode, Args.Values[1], V2P(Args.Values[2])));
end;

{ procedure Assign(Source: TPersistent); }

procedure TTreeNodes_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNodes(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ procedure BeginUpdate; }

procedure TTreeNodes_BeginUpdate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNodes(Args.Obj).BeginUpdate;
end;

{ procedure Clear; }

procedure TTreeNodes_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNodes(Args.Obj).Clear;
end;

{ procedure Delete(Node: TTreeNode); }

procedure TTreeNodes_Delete(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNodes(Args.Obj).Delete(V2O(Args.Values[0]) as TTreeNode);
end;

{ procedure EndUpdate; }

procedure TTreeNodes_EndUpdate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTreeNodes(Args.Obj).EndUpdate;
end;

{ function GetFirstNode: TTreeNode; }

procedure TTreeNodes_GetFirstNode(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNodes(Args.Obj).GetFirstNode);
end;

{ function GetNode(ItemId: HTreeItem): TTreeNode; }

procedure TTreeNodes_GetNode(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNodes(Args.Obj).GetNode(V2P(Args.Values[0])));
end;

{ function Insert(Node: TTreeNode; const S: string): TTreeNode; }

procedure TTreeNodes_Insert(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNodes(Args.Obj).Insert(V2O(Args.Values[0]) as TTreeNode, Args.Values[1]));
end;

{ function InsertObject(Node: TTreeNode; const S: string; Ptr: Pointer): TTreeNode; }

procedure TTreeNodes_InsertObject(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNodes(Args.Obj).InsertObject(V2O(Args.Values[0]) as TTreeNode, Args.Values[1],
    V2P(Args.Values[2])));
end;

{ property Read Count: Integer }

procedure TTreeNodes_Read_Count(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTreeNodes(Args.Obj).Count;
end;

{ property Read Handle: HWND }

procedure TTreeNodes_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TTreeNodes(Args.Obj).Handle);
end;

{ property Read Item[Integer]: TTreeNode }

procedure TTreeNodes_Read_Item(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNodes(Args.Obj).Item[Args.Values[0]]);
end;

{ property Read Owner: TCustomTreeView }

procedure TTreeNodes_Read_Owner(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeNodes(Args.Obj).Owner);
end;

{ TCustomTreeView }

{ function AlphaSort: Boolean; }

procedure TCustomTreeView_AlphaSort(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomTreeView(Args.Obj).AlphaSort;
end;

{ function CustomSort(SortProc: TTVCompare; Data: Longint): Boolean; }

procedure TCustomTreeView_CustomSort(var Value: Variant; Args: TJvInterpreterArgs);
begin
//  Value := TCustomTreeView(Args.Obj).CustomSort(Args.Values[0], Args.Values[1]);
  NotImplemented('TCustomTreeView.CustomSort');
end;

{ procedure FullCollapse; }

procedure TCustomTreeView_FullCollapse(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomTreeView(Args.Obj).FullCollapse;
end;

{ procedure FullExpand; }

procedure TCustomTreeView_FullExpand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomTreeView(Args.Obj).FullExpand;
end;

{ function GetHitTestInfoAt(X, Y: Integer): THitTests; }

procedure TCustomTreeView_GetHitTestInfoAt(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Word(TCustomTreeView(Args.Obj).GetHitTestInfoAt(Args.Values[0], Args.Values[1])));
end;

{ function GetNodeAt(X, Y: Integer): TTreeNode; }

procedure TCustomTreeView_GetNodeAt(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomTreeView(Args.Obj).GetNodeAt(Args.Values[0], Args.Values[1]));
end;

{ function IsEditing: Boolean; }

procedure TCustomTreeView_IsEditing(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomTreeView(Args.Obj).IsEditing;
end;

{ procedure LoadFromFile(const FileName: string); }

procedure TCustomTreeView_LoadFromFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomTreeView(Args.Obj).LoadFromFile(Args.Values[0]);
end;

{ procedure LoadFromStream(Stream: TStream); }

procedure TCustomTreeView_LoadFromStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomTreeView(Args.Obj).LoadFromStream(V2O(Args.Values[0]) as TStream);
end;

{ procedure SaveToFile(const FileName: string); }

procedure TCustomTreeView_SaveToFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomTreeView(Args.Obj).SaveToFile(Args.Values[0]);
end;

{ procedure SaveToStream(Stream: TStream); }

procedure TCustomTreeView_SaveToStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomTreeView(Args.Obj).SaveToStream(V2O(Args.Values[0]) as TStream);
end;

{ property Read DropTarget: TTreeNode }

procedure TCustomTreeView_Read_DropTarget(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomTreeView(Args.Obj).DropTarget);
end;

{ property Write DropTarget(Value: TTreeNode) }

procedure TCustomTreeView_Write_DropTarget(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomTreeView(Args.Obj).DropTarget := V2O(Value) as TTreeNode;
end;

{ property Read Selected: TTreeNode }

procedure TCustomTreeView_Read_Selected(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomTreeView(Args.Obj).Selected);
end;

{ property Write Selected(Value: TTreeNode) }

procedure TCustomTreeView_Write_Selected(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomTreeView(Args.Obj).Selected := V2O(Value) as TTreeNode;
end;

{ property Read TopItem: TTreeNode }

procedure TCustomTreeView_Read_TopItem(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomTreeView(Args.Obj).TopItem);
end;

{ property Write TopItem(Value: TTreeNode) }

procedure TCustomTreeView_Write_TopItem(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomTreeView(Args.Obj).TopItem := V2O(Value) as TTreeNode;
end;

{ TTreeView }

{ constructor Create(AOwner: TComponent) }

procedure TTreeView_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTreeView.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TTrackBar }

{ constructor Create(AOwner: TComponent) }

procedure TTrackBar_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTrackBar.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure SetTick(Value: Integer); }

procedure TTrackBar_SetTick(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTrackBar(Args.Obj).SetTick(Args.Values[0]);
end;

{ TProgressBar }

{ constructor Create(AOwner: TComponent) }

procedure TProgressBar_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TProgressBar.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure StepIt; }

procedure TProgressBar_StepIt(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TProgressBar(Args.Obj).StepIt;
end;

{ procedure StepBy(Delta: Integer); }

procedure TProgressBar_StepBy(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TProgressBar(Args.Obj).StepBy(Args.Values[0]);
end;

{ TTextAttributes }

{ constructor Create(AOwner: TCustomRichEdit; AttributeType: TAttributeType) }

procedure TTextAttributes_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTextAttributes.Create(V2O(Args.Values[0]) as TCustomRichEdit, Args.Values[1]));
end;

{ procedure Assign(Source: TPersistent); }

procedure TTextAttributes_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TTextAttributes(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ property Read Charset: TFontCharset }

procedure TTextAttributes_Read_Charset(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTextAttributes(Args.Obj).Charset;
end;

{ property Write Charset(Value: TFontCharset) }

procedure TTextAttributes_Write_Charset(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTextAttributes(Args.Obj).Charset := Value;
end;

{ property Read Color: TColor }

procedure TTextAttributes_Read_Color(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTextAttributes(Args.Obj).Color;
end;

{ property Write Color(Value: TColor) }

procedure TTextAttributes_Write_Color(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTextAttributes(Args.Obj).Color := Value;
end;

{ property Read ConsistentAttributes: TConsistentAttributes }

procedure TTextAttributes_Read_ConsistentAttributes(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Byte(TTextAttributes(Args.Obj).ConsistentAttributes));
end;

{ property Read Name: TFontName }

procedure TTextAttributes_Read_Name(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTextAttributes(Args.Obj).Name;
end;

{ property Write Name(Value: TFontName) }

procedure TTextAttributes_Write_Name(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTextAttributes(Args.Obj).Name := Value;
end;

{ property Read Pitch: TFontPitch }

procedure TTextAttributes_Read_Pitch(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTextAttributes(Args.Obj).Pitch;
end;

{ property Write Pitch(Value: TFontPitch) }

procedure TTextAttributes_Write_Pitch(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTextAttributes(Args.Obj).Pitch := Value;
end;

{ property Read Protected: Boolean }

procedure TTextAttributes_Read_Protected(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTextAttributes(Args.Obj).Protected;
end;

{ property Write Protected(Value: Boolean) }

procedure TTextAttributes_Write_Protected(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTextAttributes(Args.Obj).Protected := Value;
end;

{ property Read Size: Integer }

procedure TTextAttributes_Read_Size(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTextAttributes(Args.Obj).Size;
end;

{ property Write Size(Value: Integer) }

procedure TTextAttributes_Write_Size(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTextAttributes(Args.Obj).Size := Value;
end;

{ property Read Style: TFontStyles }

procedure TTextAttributes_Read_Style(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Byte(TTextAttributes(Args.Obj).Style));
end;

{ property Write Style(Value: TFontStyles) }

procedure TTextAttributes_Write_Style(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTextAttributes(Args.Obj).Style := TFontStyles(Byte(V2S(Value)));
end;

{ property Read Height: Integer }

procedure TTextAttributes_Read_Height(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTextAttributes(Args.Obj).Height;
end;

{ property Write Height(Value: Integer) }

procedure TTextAttributes_Write_Height(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTextAttributes(Args.Obj).Height := Value;
end;

{ TParaAttributes }

{ constructor Create(AOwner: TCustomRichEdit) }

procedure TParaAttributes_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TParaAttributes.Create(V2O(Args.Values[0]) as TCustomRichEdit));
end;

{ procedure Assign(Source: TPersistent); }

procedure TParaAttributes_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TParaAttributes(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ property Read Alignment: TAlignment }

procedure TParaAttributes_Read_Alignment(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParaAttributes(Args.Obj).Alignment;
end;

{ property Write Alignment(Value: TAlignment) }

procedure TParaAttributes_Write_Alignment(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParaAttributes(Args.Obj).Alignment := Value;
end;

{ property Read FirstIndent: Longint }

procedure TParaAttributes_Read_FirstIndent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParaAttributes(Args.Obj).FirstIndent;
end;

{ property Write FirstIndent(Value: Longint) }

procedure TParaAttributes_Write_FirstIndent(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParaAttributes(Args.Obj).FirstIndent := Value;
end;

{ property Read LeftIndent: Longint }

procedure TParaAttributes_Read_LeftIndent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParaAttributes(Args.Obj).LeftIndent;
end;

{ property Write LeftIndent(Value: Longint) }

procedure TParaAttributes_Write_LeftIndent(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParaAttributes(Args.Obj).LeftIndent := Value;
end;

{ property Read Numbering: TNumberingStyle }

procedure TParaAttributes_Read_Numbering(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParaAttributes(Args.Obj).Numbering;
end;

{ property Write Numbering(Value: TNumberingStyle) }

procedure TParaAttributes_Write_Numbering(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParaAttributes(Args.Obj).Numbering := Value;
end;

{ property Read RightIndent: Longint }

procedure TParaAttributes_Read_RightIndent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParaAttributes(Args.Obj).RightIndent;
end;

{ property Write RightIndent(Value: Longint) }

procedure TParaAttributes_Write_RightIndent(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParaAttributes(Args.Obj).RightIndent := Value;
end;

{ property Read Tab[Byte]: Longint }

procedure TParaAttributes_Read_Tab(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParaAttributes(Args.Obj).Tab[Args.Values[0]];
end;

{ property Write Tab[Byte]: Longint }

procedure TParaAttributes_Write_Tab(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParaAttributes(Args.Obj).Tab[Args.Values[0]] := Value;
end;

{ property Read TabCount: Integer }

procedure TParaAttributes_Read_TabCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TParaAttributes(Args.Obj).TabCount;
end;

{ property Write TabCount(Value: Integer) }

procedure TParaAttributes_Write_TabCount(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TParaAttributes(Args.Obj).TabCount := Value;
end;

{ TCustomRichEdit }

{ procedure Clear; }

procedure TCustomRichEdit_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomRichEdit(Args.Obj).Clear;
end;

{ function FindText(const SearchStr: string; StartPos, Length: Integer; Options: TSearchTypes): Integer; }

procedure TCustomRichEdit_FindText(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomRichEdit(Args.Obj).FindText(Args.Values[0], Args.Values[1], Args.Values[2],
    TSearchTypes(Byte(V2S(Args.Values[3]))));
end;

{ function GetSelTextBuf(Buffer: PChar; BufSize: Integer): Integer; }

procedure TCustomRichEdit_GetSelTextBuf(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomRichEdit(Args.Obj).GetSelTextBuf(PChar(string(Args.Values[0])), Args.Values[1]);
end;

{ procedure Print(const Caption: string); }

procedure TCustomRichEdit_Print(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomRichEdit(Args.Obj).Print(Args.Values[0]);
end;

{ property Read DefAttributes: TTextAttributes }

procedure TCustomRichEdit_Read_DefAttributes(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomRichEdit(Args.Obj).DefAttributes);
end;

{ property Write DefAttributes(Value: TTextAttributes) }

procedure TCustomRichEdit_Write_DefAttributes(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomRichEdit(Args.Obj).DefAttributes := V2O(Value) as TTextAttributes;
end;

{ property Read SelAttributes: TTextAttributes }

procedure TCustomRichEdit_Read_SelAttributes(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomRichEdit(Args.Obj).SelAttributes);
end;

{ property Write SelAttributes(Value: TTextAttributes) }

procedure TCustomRichEdit_Write_SelAttributes(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomRichEdit(Args.Obj).SelAttributes := V2O(Value) as TTextAttributes;
end;

{ property Read PageRect: TRect }

procedure TCustomRichEdit_Read_PageRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Rect2Var(TCustomRichEdit(Args.Obj).PageRect);
end;

{ property Write PageRect(Value: TRect) }

procedure TCustomRichEdit_Write_PageRect(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomRichEdit(Args.Obj).PageRect := Var2Rect(Value);
end;

{ property Read Paragraph: TParaAttributes }

procedure TCustomRichEdit_Read_Paragraph(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomRichEdit(Args.Obj).Paragraph);
end;

{ TRichEdit }

{ constructor Create(AOwner: TComponent) }

procedure TRichEdit_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TRichEdit.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TUpDown }

{ constructor Create(AOwner: TComponent) }

procedure TUpDown_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TUpDown.Create(V2O(Args.Values[0]) as TComponent));
end;

{ THotKey }

{ constructor Create(AOwner: TComponent) }

procedure THotKey_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(THotKey.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TListColumn }

{ constructor Create(Collection: TCollection) }

procedure TListColumn_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TListColumn.Create(V2O(Args.Values[0]) as TCollection));
end;

{ procedure Assign(Source: TPersistent); }

procedure TListColumn_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TListColumn(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ property Read WidthType: TWidth }

procedure TListColumn_Read_WidthType(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TListColumn(Args.Obj).WidthType;
end;

{ TListColumns }

{ constructor Create(AOwner: TCustomListView) }

procedure TListColumns_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TListColumns.Create(V2O(Args.Values[0]) as TCustomListView));
end;

{ function Add: TListColumn; }

procedure TListColumns_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TListColumns(Args.Obj).Add);
end;

{ property Read Owner: TCustomListView }

procedure TListColumns_Read_Owner(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TListColumns(Args.Obj).Owner);
end;

{ property Read Items[Integer]: TListColumn }

procedure TListColumns_Read_Items(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TListColumns(Args.Obj).Items[Args.Values[0]]);
end;

{ property Write Items[Integer]: TListColumn }

procedure TListColumns_Write_Items(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TListColumns(Args.Obj).Items[Args.Values[0]] := V2O(Value) as TListColumn;
end;

{ TListItem }

{ constructor Create(AOwner: TListItems) }

procedure TListItem_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TListItem.Create(V2O(Args.Values[0]) as TListItems));
end;

{ procedure CancelEdit; }

procedure TListItem_CancelEdit(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).CancelEdit;
end;

{ procedure Delete; }

procedure TListItem_Delete(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).Delete;
end;

{ function DisplayRect(Code: TDisplayCode): TRect; }

procedure TListItem_DisplayRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Rect2Var(TListItem(Args.Obj).DisplayRect(Args.Values[0]));
end;

{ function EditCaption: Boolean; }

procedure TListItem_EditCaption(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TListItem(Args.Obj).EditCaption;
end;

{ function GetPosition: TPoint; }

procedure TListItem_GetPosition(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Point2Var(TListItem(Args.Obj).GetPosition);
end;

{ procedure MakeVisible(PartialOK: Boolean); }

procedure TListItem_MakeVisible(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).MakeVisible(Args.Values[0]);
end;

{ procedure Update; }

procedure TListItem_Update(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).Update;
end;

{ procedure SetPosition(const Value: TPoint); }

procedure TListItem_SetPosition(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).SetPosition(Var2Point(Args.Values[0]));
end;

{ property Read Caption: string }

procedure TListItem_Read_Caption(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TListItem(Args.Obj).Caption;
end;

{ property Write Caption(Value: string) }

procedure TListItem_Write_Caption(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).Caption := Value;
end;

{ property Read Checked: Boolean }

procedure TListItem_Read_Checked(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TListItem(Args.Obj).Checked;
end;

{ property Write Checked(Value: Boolean) }

procedure TListItem_Write_Checked(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).Checked := Value;
end;

{ property Read Cut: Boolean }

procedure TListItem_Read_Cut(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TListItem(Args.Obj).Cut;
end;

{ property Write Cut(Value: Boolean) }

procedure TListItem_Write_Cut(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).Cut := Value;
end;

{ property Read Data: Pointer }

procedure TListItem_Read_Data(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := P2V(TListItem(Args.Obj).Data);
end;

{ property Write Data(Value: Pointer) }

procedure TListItem_Write_Data(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).Data := V2P(Value);
end;

{ property Read DropTarget: Boolean }

procedure TListItem_Read_DropTarget(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TListItem(Args.Obj).DropTarget;
end;

{ property Write DropTarget(Value: Boolean) }

procedure TListItem_Write_DropTarget(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).DropTarget := Value;
end;

{ property Read Focused: Boolean }

procedure TListItem_Read_Focused(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TListItem(Args.Obj).Focused;
end;

{ property Write Focused(Value: Boolean) }

procedure TListItem_Write_Focused(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).Focused := Value;
end;

{ property Read Handle: HWND }

procedure TListItem_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TListItem(Args.Obj).Handle);
end;

{ property Read ImageIndex: Integer }

procedure TListItem_Read_ImageIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TListItem(Args.Obj).ImageIndex;
end;

{ property Write ImageIndex(Value: Integer) }

procedure TListItem_Write_ImageIndex(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).ImageIndex := Value;
end;

{ property Read Index: Integer }

procedure TListItem_Read_Index(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TListItem(Args.Obj).Index;
end;

{ property Read Left: Integer }

procedure TListItem_Read_Left(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TListItem(Args.Obj).Left;
end;

{ property Write Left(Value: Integer) }

procedure TListItem_Write_Left(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).Left := Value;
end;

{ property Read ListView: TCustomListView }

procedure TListItem_Read_ListView(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TListItem(Args.Obj).ListView);
end;

{ property Read Owner: TListItems }

procedure TListItem_Read_Owner(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TListItem(Args.Obj).Owner);
end;

{ property Read OverlayIndex: Integer }

procedure TListItem_Read_OverlayIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TListItem(Args.Obj).OverlayIndex;
end;

{ property Write OverlayIndex(Value: Integer) }

procedure TListItem_Write_OverlayIndex(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).OverlayIndex := Value;
end;

{ property Read Selected: Boolean }

procedure TListItem_Read_Selected(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TListItem(Args.Obj).Selected;
end;

{ property Write Selected(Value: Boolean) }

procedure TListItem_Write_Selected(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).Selected := Value;
end;

{ property Read StateIndex: Integer }

procedure TListItem_Read_StateIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TListItem(Args.Obj).StateIndex;
end;

{ property Write StateIndex(Value: Integer) }

procedure TListItem_Write_StateIndex(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).StateIndex := Value;
end;

{ property Read SubItems: TStrings }

procedure TListItem_Read_SubItems(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TListItem(Args.Obj).SubItems);
end;

{ property Write SubItems(Value: TStrings) }

procedure TListItem_Write_SubItems(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).SubItems := V2O(Value) as TStrings;
end;

{ property Read Top: Integer }

procedure TListItem_Read_Top(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TListItem(Args.Obj).Top;
end;

{ property Write Top(Value: Integer) }

procedure TListItem_Write_Top(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItem(Args.Obj).Top := Value;
end;

{ TListItems }

{ constructor Create(AOwner: TCustomListView) }

procedure TListItems_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TListItems.Create(V2O(Args.Values[0]) as TCustomListView));
end;

{ function Add: TListItem; }

procedure TListItems_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TListItems(Args.Obj).Add);
end;

{ procedure Assign(Source: TPersistent); }

procedure TListItems_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItems(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ procedure BeginUpdate; }

procedure TListItems_BeginUpdate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItems(Args.Obj).BeginUpdate;
end;

{ procedure Clear; }

procedure TListItems_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItems(Args.Obj).Clear;
end;

{ procedure Delete(Index: Integer); }

procedure TListItems_Delete(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItems(Args.Obj).Delete(Args.Values[0]);
end;

{ procedure EndUpdate; }

procedure TListItems_EndUpdate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItems(Args.Obj).EndUpdate;
end;

{ function IndexOf(Value: TListItem): Integer; }

procedure TListItems_IndexOf(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TListItems(Args.Obj).IndexOf(V2O(Args.Values[0]) as TListItem);
end;

{ function Insert(Index: Integer): TListItem; }

procedure TListItems_Insert(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TListItems(Args.Obj).Insert(Args.Values[0]));
end;

{ property Read Count: Integer }

procedure TListItems_Read_Count(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TListItems(Args.Obj).Count;
end;

{ property Read Handle: HWND }

procedure TListItems_Read_Handle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TListItems(Args.Obj).Handle);
end;

{ property Read Item[Integer]: TListItem }

procedure TListItems_Read_Item(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TListItems(Args.Obj).Item[Args.Values[0]]);
end;

{ property Write Item[Integer]: TListItem }

procedure TListItems_Write_Item(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TListItems(Args.Obj).Item[Args.Values[0]] := V2O(Value) as TListItem;
end;

{ property Read Owner: TCustomListView }

procedure TListItems_Read_Owner(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TListItems(Args.Obj).Owner);
end;

{ TCustomListView }

{ function AlphaSort: Boolean; }

procedure TCustomListView_AlphaSort(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomListView(Args.Obj).AlphaSort;
end;

{ procedure Arrange(Code: TListArrangement); }

procedure TCustomListView_Arrange(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomListView(Args.Obj).Arrange(Args.Values[0]);
end;

{ function FindCaption(StartIndex: Integer; Value: string; Partial, Inclusive, Wrap: Boolean): TListItem; }

procedure TCustomListView_FindCaption(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomListView(Args.Obj).FindCaption(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3],
    Args.Values[4]));
end;

{ function FindData(StartIndex: Integer; Value: Pointer; Inclusive, Wrap: Boolean): TListItem; }

procedure TCustomListView_FindData(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomListView(Args.Obj).FindData(Args.Values[0], V2P(Args.Values[1]), Args.Values[2], Args.Values[3]));
end;

{ function GetItemAt(X, Y: Integer): TListItem; }

procedure TCustomListView_GetItemAt(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomListView(Args.Obj).GetItemAt(Args.Values[0], Args.Values[1]));
end;

{ function GetNearestItem(Point: TPoint; Direction: TSearchDirection): TListItem; }

procedure TCustomListView_GetNearestItem(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomListView(Args.Obj).GetNearestItem(Var2Point(Args.Values[0]), Args.Values[1]));
end;

{ function GetNextItem(StartItem: TListItem; Direction: TSearchDirection; States: TItemStates): TListItem; }

procedure TCustomListView_GetNextItem(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomListView(Args.Obj).GetNextItem(V2O(Args.Values[0]) as TListItem, Args.Values[1],
    TItemStates(Byte(V2S(Args.Values[2])))));
end;

{ function GetSearchString: string; }

procedure TCustomListView_GetSearchString(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomListView(Args.Obj).GetSearchString;
end;

{ function IsEditing: Boolean; }

procedure TCustomListView_IsEditing(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomListView(Args.Obj).IsEditing;
end;

{ procedure Scroll(DX, DY: Integer); }

procedure TCustomListView_Scroll(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomListView(Args.Obj).Scroll(Args.Values[0], Args.Values[1]);
end;

{ property Read Checkboxes: Boolean }

procedure TCustomListView_Read_Checkboxes(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomListView(Args.Obj).Checkboxes;
end;

{ property Write Checkboxes(Value: Boolean) }

procedure TCustomListView_Write_Checkboxes(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomListView(Args.Obj).Checkboxes := Value;
end;

{ property Read Column[Integer]: TListColumn }

procedure TCustomListView_Read_Column(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomListView(Args.Obj).Column[Args.Values[0]]);
end;

{ property Read DropTarget: TListItem }

procedure TCustomListView_Read_DropTarget(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomListView(Args.Obj).DropTarget);
end;

{ property Write DropTarget(Value: TListItem) }

procedure TCustomListView_Write_DropTarget(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomListView(Args.Obj).DropTarget := V2O(Value) as TListItem;
end;

{ property Read GridLines: Boolean }

procedure TCustomListView_Read_GridLines(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomListView(Args.Obj).GridLines;
end;

{ property Write GridLines(Value: Boolean) }

procedure TCustomListView_Write_GridLines(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomListView(Args.Obj).GridLines := Value;
end;

{ property Read HotTrack: Boolean }

procedure TCustomListView_Read_HotTrack(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomListView(Args.Obj).HotTrack;
end;

{ property Write HotTrack(Value: Boolean) }

procedure TCustomListView_Write_HotTrack(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomListView(Args.Obj).HotTrack := Value;
end;

{ property Read ItemFocused: TListItem }

procedure TCustomListView_Read_ItemFocused(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomListView(Args.Obj).ItemFocused);
end;

{ property Write ItemFocused(Value: TListItem) }

procedure TCustomListView_Write_ItemFocused(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomListView(Args.Obj).ItemFocused := V2O(Value) as TListItem;
end;

{ property Read RowSelect: Boolean }

procedure TCustomListView_Read_RowSelect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomListView(Args.Obj).RowSelect;
end;

{ property Write RowSelect(Value: Boolean) }

procedure TCustomListView_Write_RowSelect(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomListView(Args.Obj).RowSelect := Value;
end;

{ property Read SelCount: Integer }

procedure TCustomListView_Read_SelCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomListView(Args.Obj).SelCount;
end;

{ property Read Selected: TListItem }

procedure TCustomListView_Read_Selected(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomListView(Args.Obj).Selected);
end;

{ property Write Selected(Value: TListItem) }

procedure TCustomListView_Write_Selected(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomListView(Args.Obj).Selected := V2O(Value) as TListItem;
end;

{ function CustomSort(SortProc: TLVCompare; lParam: Longint): Boolean; }

procedure TCustomListView_CustomSort(var Value: Variant; Args: TJvInterpreterArgs);
begin
//  Value := TCustomListView(Args.Obj).CustomSort(Args.Values[0], Args.Values[1]);
  NotImplemented('TCustomListView.CustomSort');
end;

{ function StringWidth(S: string): Integer; }

procedure TCustomListView_StringWidth(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomListView(Args.Obj).StringWidth(Args.Values[0]);
end;

{ procedure UpdateItems(FirstIndex, LastIndex: Integer); }

procedure TCustomListView_UpdateItems(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomListView(Args.Obj).UpdateItems(Args.Values[0], Args.Values[1]);
end;

{ property Read TopItem: TListItem }

procedure TCustomListView_Read_TopItem(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomListView(Args.Obj).TopItem);
end;

{ property Read ViewOrigin: TPoint }

procedure TCustomListView_Read_ViewOrigin(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Point2Var(TCustomListView(Args.Obj).ViewOrigin);
end;

{ property Read VisibleRowCount: Integer }

procedure TCustomListView_Read_VisibleRowCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomListView(Args.Obj).VisibleRowCount;
end;

{ property Read BoundingRect: TRect }

procedure TCustomListView_Read_BoundingRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Rect2Var(TCustomListView(Args.Obj).BoundingRect);
end;

{ TListView }

{ constructor Create(AOwner: TComponent) }

procedure TListView_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TListView.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TAnimate }

{ constructor Create(AOwner: TComponent) }

procedure TAnimate_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TAnimate.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read FrameCount: Integer }

procedure TAnimate_Read_FrameCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TAnimate(Args.Obj).FrameCount;
end;

{ property Read FrameHeight: Integer }

procedure TAnimate_Read_FrameHeight(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TAnimate(Args.Obj).FrameHeight;
end;

{ property Read FrameWidth: Integer }

procedure TAnimate_Read_FrameWidth(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TAnimate(Args.Obj).FrameWidth;
end;

{ property Read Open: Boolean }

procedure TAnimate_Read_Open(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TAnimate(Args.Obj).Open;
end;

{ property Write Open(Value: Boolean) }

procedure TAnimate_Write_Open(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TAnimate(Args.Obj).Open := Value;
end;

{ procedure Play(FromFrame, ToFrame: Word; Count: Integer); }

procedure TAnimate_Play(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TAnimate(Args.Obj).Play(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{ procedure Reset; }

procedure TAnimate_Reset(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TAnimate(Args.Obj).Reset;
end;

{ procedure Seek(Frame: Smallint); }

procedure TAnimate_Seek(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TAnimate(Args.Obj).Seek(Args.Values[0]);
end;

{ procedure Stop; }

procedure TAnimate_Stop(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TAnimate(Args.Obj).Stop;
end;

{ property Read ResHandle: THandle }

procedure TAnimate_Read_ResHandle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TAnimate(Args.Obj).ResHandle);
end;

{ property Write ResHandle(Value: THandle) }

procedure TAnimate_Write_ResHandle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TAnimate(Args.Obj).ResHandle := Value;
end;

{ property Read ResId: Integer }

procedure TAnimate_Read_ResId(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TAnimate(Args.Obj).ResId;
end;

{ property Write ResId(Value: Integer) }

procedure TAnimate_Write_ResId(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TAnimate(Args.Obj).ResId := Value;
end;

{ property Read ResName: string }

procedure TAnimate_Read_ResName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TAnimate(Args.Obj).ResName;
end;

{ property Write ResName(Value: string) }

procedure TAnimate_Write_ResName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TAnimate(Args.Obj).ResName := Value;
end;

type
  TJvInterpreterComCtrlsEvent = class(TJvInterpreterEvent)
  private
    procedure TabChangingEvent(Sender: TObject; var AllowChange: Boolean);
    procedure DrawPanelEvent(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure DrawSectionEvent(HeaderControl: THeaderControl; Section: THeaderSection; const Rect: TRect; Pressed:
      Boolean);
    procedure SectionNotifyEvent(HeaderControl: THeaderControl; Section: THeaderSection);
    procedure SectionTrackEvent(HeaderControl: THeaderControl; Section: THeaderSection; Width: Integer; State:
      TSectionTrackState);
    procedure TVChangingEvent(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure TVChangedEvent(Sender: TObject; Node: TTreeNode);
    procedure TVEditingEvent(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure TVEditedEvent(Sender: TObject; Node: TTreeNode; var S: string);
    procedure TVExpandingEvent(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure TVCollapsingEvent(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
    procedure TVExpandedEvent(Sender: TObject; Node: TTreeNode);
    procedure TVCompareEvent(Sender: TObject; Node1, Node2: TTreeNode; Data: Integer; var Compare: Integer);
    procedure RichEditResizeEvent(Sender: TObject; Rect: TRect);
    procedure RichEditProtectChange(Sender: TObject; StartPos, EndPos: Integer; var AllowChange: Boolean);
    procedure RichEditSaveClipboard(Sender: TObject; NumObjects, NumChars: Integer; var SaveClipboard: Boolean);
    procedure UDClickEvent(Sender: TObject; Button: TUDBtnType);
    procedure UDChangingEvent(Sender: TObject; var AllowChange: Boolean);
    procedure LVDeletedEvent(Sender: TObject; Item: TListItem);
    procedure LVEditingEvent(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
    procedure LVEditedEvent(Sender: TObject; Item: TListItem; var S: string);
    procedure LVChangeEvent(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure LVChangingEvent(Sender: TObject; Item: TListItem; Change: TItemChange; var AllowChange: Boolean);
    procedure LVColumnClickEvent(Sender: TObject; Column: TListColumn);
    procedure LVCompareEvent(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
  end;

procedure TJvInterpreterComCtrlsEvent.TabChangingEvent(Sender: TObject; var AllowChange: Boolean);
begin
  CallFunction(nil, [O2V(Sender), AllowChange]);
  AllowChange := Args.Values[1];
end;

procedure TJvInterpreterComCtrlsEvent.DrawPanelEvent(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
begin
  CallFunction(nil, [O2V(StatusBar), O2V(Panel), Rect2Var(Rect)]);
end;

procedure TJvInterpreterComCtrlsEvent.DrawSectionEvent(HeaderControl: THeaderControl; Section: THeaderSection; const
  Rect: TRect; Pressed: Boolean);
begin
  CallFunction(nil, [O2V(HeaderControl), O2V(Section), Rect2Var(Rect), Pressed]);
end;

procedure TJvInterpreterComCtrlsEvent.SectionNotifyEvent(HeaderControl: THeaderControl; Section: THeaderSection);
begin
  CallFunction(nil, [O2V(HeaderControl), O2V(Section)]);
end;

procedure TJvInterpreterComCtrlsEvent.SectionTrackEvent(HeaderControl: THeaderControl; Section: THeaderSection; Width:
  Integer; State: TSectionTrackState);
begin
  CallFunction(nil, [O2V(HeaderControl), O2V(Section), Width, V2S(Byte(State))]);
end;

procedure TJvInterpreterComCtrlsEvent.TVChangingEvent(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  CallFunction(nil, [O2V(Sender), O2V(Node), AllowChange]);
  AllowChange := Args.Values[2];
end;

procedure TJvInterpreterComCtrlsEvent.TVChangedEvent(Sender: TObject; Node: TTreeNode);
begin
  CallFunction(nil, [O2V(Sender), O2V(Node)]);
end;

procedure TJvInterpreterComCtrlsEvent.TVEditingEvent(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
begin
  CallFunction(nil, [O2V(Sender), O2V(Node), AllowEdit]);
  AllowEdit := Args.Values[2];
end;

procedure TJvInterpreterComCtrlsEvent.TVEditedEvent(Sender: TObject; Node: TTreeNode; var S: string);
begin
  CallFunction(nil, [O2V(Sender), O2V(Node), S]);
  S := Args.Values[2];
end;

procedure TJvInterpreterComCtrlsEvent.TVExpandingEvent(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
begin
  CallFunction(nil, [O2V(Sender), O2V(Node), AllowExpansion]);
  AllowExpansion := Args.Values[2];
end;

procedure TJvInterpreterComCtrlsEvent.TVCollapsingEvent(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
begin
  CallFunction(nil, [O2V(Sender), O2V(Node), AllowCollapse]);
  AllowCollapse := Args.Values[2];
end;

procedure TJvInterpreterComCtrlsEvent.TVExpandedEvent(Sender: TObject; Node: TTreeNode);
begin
  CallFunction(nil, [O2V(Sender), O2V(Node)]);
end;

procedure TJvInterpreterComCtrlsEvent.TVCompareEvent(Sender: TObject; Node1, Node2: TTreeNode; Data: Integer;
  var Compare: Integer);
begin
  CallFunction(nil, [O2V(Sender), O2V(Node1), O2V(Node2), Data, Compare]);
  Compare := Args.Values[4];
end;

procedure TJvInterpreterComCtrlsEvent.RichEditResizeEvent(Sender: TObject; Rect: TRect);
begin
  CallFunction(nil, [O2V(Sender), Rect2Var(Rect)]);
end;

procedure TJvInterpreterComCtrlsEvent.RichEditProtectChange(Sender: TObject; StartPos, EndPos: Integer;
  var AllowChange: Boolean);
begin
  CallFunction(nil, [O2V(Sender), StartPos, EndPos, AllowChange]);
  AllowChange := Args.Values[3];
end;

procedure TJvInterpreterComCtrlsEvent.RichEditSaveClipboard(Sender: TObject; NumObjects, NumChars: Integer;
  var SaveClipboard: Boolean);
begin
  CallFunction(nil, [O2V(Sender), NumObjects, NumChars, SaveClipboard]);
  SaveClipboard := Args.Values[3];
end;

procedure TJvInterpreterComCtrlsEvent.UDClickEvent(Sender: TObject; Button: TUDBtnType);
begin
  CallFunction(nil, [O2V(Sender), Button]);
end;

procedure TJvInterpreterComCtrlsEvent.UDChangingEvent(Sender: TObject; var AllowChange: Boolean);
begin
  CallFunction(nil, [O2V(Sender), AllowChange]);
  AllowChange := Args.Values[1];
end;

procedure TJvInterpreterComCtrlsEvent.LVDeletedEvent(Sender: TObject; Item: TListItem);
begin
  CallFunction(nil, [O2V(Sender), O2V(Item)]);
end;

procedure TJvInterpreterComCtrlsEvent.LVEditingEvent(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
begin
  CallFunction(nil, [O2V(Sender), O2V(Item), AllowEdit]);
  AllowEdit := Args.Values[2];
end;

procedure TJvInterpreterComCtrlsEvent.LVEditedEvent(Sender: TObject; Item: TListItem; var S: string);
begin
  CallFunction(nil, [O2V(Sender), O2V(Item), S]);
  S := Args.Values[2];
end;

procedure TJvInterpreterComCtrlsEvent.LVChangeEvent(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  CallFunction(nil, [O2V(Sender), O2V(Item), Change]);
end;

procedure TJvInterpreterComCtrlsEvent.LVChangingEvent(Sender: TObject; Item: TListItem; Change: TItemChange;
  var AllowChange: Boolean);
begin
  CallFunction(nil, [O2V(Sender), O2V(Item), Change, AllowChange]);
  AllowChange := Args.Values[3];
end;

procedure TJvInterpreterComCtrlsEvent.LVColumnClickEvent(Sender: TObject; Column: TListColumn);
begin
  CallFunction(nil, [O2V(Sender), O2V(Column)]);
end;

procedure TJvInterpreterComCtrlsEvent.LVCompareEvent(Sender: TObject; Item1, Item2: TListItem; Data: Integer;
  var Compare: Integer);
begin
  CallFunction(nil, [O2V(Sender), O2V(Item1), O2V(Item2), Data, Compare]);
  Compare := Args.Values[4];
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cComCtrls = 'ComCtrls';
begin
  with JvInterpreterAdapter do
  begin
    { TTabPosition }
    AddConst(cComCtrls, 'tpTop', Ord(tpTop));
    AddConst(cComCtrls, 'tpBottom', Ord(tpBottom));
    { TCustomTabControl }
    AddClass(cComCtrls, TCustomTabControl, 'TCustomTabControl');
    { TTabControl }
    AddClass(cComCtrls, TTabControl, 'TTabControl');
    AddGet(TTabControl, 'Create', TTabControl_Create, 1, [varEmpty], varEmpty);
    { TTabSheet }
    AddClass(cComCtrls, TTabSheet, 'TTabSheet');
    AddGet(TTabSheet, 'Create', TTabSheet_Create, 1, [varEmpty], varEmpty);
    AddGet(TTabSheet, 'PageControl', TTabSheet_Read_PageControl, 0, [varEmpty], varEmpty);
    AddSet(TTabSheet, 'PageControl', TTabSheet_Write_PageControl, 0, [varEmpty]);
    AddGet(TTabSheet, 'TabIndex', TTabSheet_Read_TabIndex, 0, [varEmpty], varEmpty);
    { TPageControl }
    AddClass(cComCtrls, TPageControl, 'TPageControl');
    AddGet(TPageControl, 'Create', TPageControl_Create, 1, [varEmpty], varEmpty);
    AddGet(TPageControl, 'FindNextPage', TPageControl_FindNextPage, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TPageControl, 'SelectNextPage', TPageControl_SelectNextPage, 1, [varEmpty], varEmpty);
    AddGet(TPageControl, 'PageCount', TPageControl_Read_PageCount, 0, [varEmpty], varEmpty);
    AddGet(TPageControl, 'Pages', TPageControl_Read_Pages, 1, [varEmpty], varEmpty);
    { TStatusPanelStyle }
    AddConst(cComCtrls, 'psText', Ord(psText));
    AddConst(cComCtrls, 'psOwnerDraw', Ord(psOwnerDraw));
    { TStatusPanelBevel }
    AddConst(cComCtrls, 'pbNone', Ord(pbNone));
    AddConst(cComCtrls, 'pbLowered', Ord(pbLowered));
    AddConst(cComCtrls, 'pbRaised', Ord(pbRaised));
    { TStatusPanel }
    AddClass(cComCtrls, TStatusPanel, 'TStatusPanel');
    AddGet(TStatusPanel, 'Create', TStatusPanel_Create, 1, [varEmpty], varEmpty);
    AddGet(TStatusPanel, 'Assign', TStatusPanel_Assign, 1, [varEmpty], varEmpty);
    { TStatusPanels }
    AddClass(cComCtrls, TStatusPanels, 'TStatusPanels');
    AddGet(TStatusPanels, 'Create', TStatusPanels_Create, 1, [varEmpty], varEmpty);
    AddGet(TStatusPanels, 'Add', TStatusPanels_Add, 0, [varEmpty], varEmpty);
    AddIGet(TStatusPanels, 'Items', TStatusPanels_Read_Items, 1, [varEmpty], varEmpty);
    AddISet(TStatusPanels, 'Items', TStatusPanels_Write_Items, 1, [varNull]);
    { TStatusBar }
    AddClass(cComCtrls, TStatusBar, 'TStatusBar');
    AddGet(TStatusBar, 'Create', TStatusBar_Create, 1, [varEmpty], varEmpty);
    AddGet(TStatusBar, 'Canvas', TStatusBar_Read_Canvas, 0, [varEmpty], varEmpty);
    { THeaderSectionStyle }
    AddConst(cComCtrls, 'hsText', Ord(hsText));
    AddConst(cComCtrls, 'hsOwnerDraw', Ord(hsOwnerDraw));
    { THeaderSection }
    AddClass(cComCtrls, THeaderSection, 'THeaderSection');
    AddGet(THeaderSection, 'Create', THeaderSection_Create, 1, [varEmpty], varEmpty);
    AddGet(THeaderSection, 'Assign', THeaderSection_Assign, 1, [varEmpty], varEmpty);
    AddGet(THeaderSection, 'Left', THeaderSection_Read_Left, 0, [varEmpty], varEmpty);
    AddGet(THeaderSection, 'Right', THeaderSection_Read_Right, 0, [varEmpty], varEmpty);
    { THeaderSections }
    AddClass(cComCtrls, THeaderSections, 'THeaderSections');
    AddGet(THeaderSections, 'Create', THeaderSections_Create, 1, [varEmpty], varEmpty);
    AddGet(THeaderSections, 'Add', THeaderSections_Add, 0, [varEmpty], varEmpty);
    AddIGet(THeaderSections, 'Items', THeaderSections_Read_Items, 1, [varEmpty], varEmpty);
    AddISet(THeaderSections, 'Items', THeaderSections_Write_Items, 1, [varNull]);
    { TSectionTrackState }
    AddConst(cComCtrls, 'tsTrackBegin', Ord(tsTrackBegin));
    AddConst(cComCtrls, 'tsTrackMove', Ord(tsTrackMove));
    AddConst(cComCtrls, 'tsTrackEnd', Ord(tsTrackEnd));
    { THeaderControl }
    AddClass(cComCtrls, THeaderControl, 'THeaderControl');
    AddGet(THeaderControl, 'Create', THeaderControl_Create, 1, [varEmpty], varEmpty);
    AddGet(THeaderControl, 'Canvas', THeaderControl_Read_Canvas, 0, [varEmpty], varEmpty);
    { TNodeState }
    AddConst(cComCtrls, 'nsCut', Ord(nsCut));
    AddConst(cComCtrls, 'nsDropHilited', Ord(nsDropHilited));
    AddConst(cComCtrls, 'nsFocused', Ord(nsFocused));
    AddConst(cComCtrls, 'nsSelected', Ord(nsSelected));
    AddConst(cComCtrls, 'nsExpanded', Ord(nsExpanded));
    { TNodeAttachMode }
    AddConst(cComCtrls, 'naAdd', Ord(naAdd));
    AddConst(cComCtrls, 'naAddFirst', Ord(naAddFirst));
    AddConst(cComCtrls, 'naAddChild', Ord(naAddChild));
    AddConst(cComCtrls, 'naAddChildFirst', Ord(naAddChildFirst));
    AddConst(cComCtrls, 'naInsert', Ord(naInsert));
    { TAddMode }
    AddConst(cComCtrls, 'taAddFirst', Ord(taAddFirst));
    AddConst(cComCtrls, 'taAdd', Ord(taAdd));
    AddConst(cComCtrls, 'taInsert', Ord(taInsert));
    { TTreeNode }
    AddClass(cComCtrls, TTreeNode, 'TTreeNode');
    AddGet(TTreeNode, 'Create', TTreeNode_Create, 1, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'AlphaSort', TTreeNode_AlphaSort, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'Assign', TTreeNode_Assign, 1, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'Collapse', TTreeNode_Collapse, 1, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'CustomSort', TTreeNode_CustomSort, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TTreeNode, 'Delete', TTreeNode_Delete, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'DeleteChildren', TTreeNode_DeleteChildren, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'DisplayRect', TTreeNode_DisplayRect, 1, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'EditText', TTreeNode_EditText, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'EndEdit', TTreeNode_EndEdit, 1, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'Expand', TTreeNode_Expand, 1, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'GetFirstChild', TTreeNode_GetFirstChild, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'GetHandle', TTreeNode_GetHandle, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'GetLastChild', TTreeNode_GetLastChild, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'GetNext', TTreeNode_GetNext, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'GetNextChild', TTreeNode_GetNextChild, 1, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'GetNextSibling', TTreeNode_GetNextSibling, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'GetNextVisible', TTreeNode_GetNextVisible, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'GetPrev', TTreeNode_GetPrev, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'GetPrevChild', TTreeNode_GetPrevChild, 1, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'GetPrevSibling', TTreeNode_GetPrevSibling, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'GetPrevVisible', TTreeNode_GetPrevVisible, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'HasAsParent', TTreeNode_HasAsParent, 1, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'IndexOf', TTreeNode_IndexOf, 1, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'MakeVisible', TTreeNode_MakeVisible, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'MoveTo', TTreeNode_MoveTo, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TTreeNode, 'AbsoluteIndex', TTreeNode_Read_AbsoluteIndex, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'Count', TTreeNode_Read_Count, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'Cut', TTreeNode_Read_Cut, 0, [varEmpty], varEmpty);
    AddSet(TTreeNode, 'Cut', TTreeNode_Write_Cut, 0, [varEmpty]);
    AddGet(TTreeNode, 'Data', TTreeNode_Read_Data, 0, [varEmpty], varEmpty);
    AddSet(TTreeNode, 'Data', TTreeNode_Write_Data, 0, [varEmpty]);
    AddGet(TTreeNode, 'Deleting', TTreeNode_Read_Deleting, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'Focused', TTreeNode_Read_Focused, 0, [varEmpty], varEmpty);
    AddSet(TTreeNode, 'Focused', TTreeNode_Write_Focused, 0, [varEmpty]);
    AddGet(TTreeNode, 'DropTarget', TTreeNode_Read_DropTarget, 0, [varEmpty], varEmpty);
    AddSet(TTreeNode, 'DropTarget', TTreeNode_Write_DropTarget, 0, [varEmpty]);
    AddGet(TTreeNode, 'Selected', TTreeNode_Read_Selected, 0, [varEmpty], varEmpty);
    AddSet(TTreeNode, 'Selected', TTreeNode_Write_Selected, 0, [varEmpty]);
    AddGet(TTreeNode, 'Expanded', TTreeNode_Read_Expanded, 0, [varEmpty], varEmpty);
    AddSet(TTreeNode, 'Expanded', TTreeNode_Write_Expanded, 0, [varEmpty]);
    AddGet(TTreeNode, 'Handle', TTreeNode_Read_Handle, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'HasChildren', TTreeNode_Read_HasChildren, 0, [varEmpty], varEmpty);
    AddSet(TTreeNode, 'HasChildren', TTreeNode_Write_HasChildren, 0, [varEmpty]);
    AddGet(TTreeNode, 'ImageIndex', TTreeNode_Read_ImageIndex, 0, [varEmpty], varEmpty);
    AddSet(TTreeNode, 'ImageIndex', TTreeNode_Write_ImageIndex, 0, [varEmpty]);
    AddGet(TTreeNode, 'Index', TTreeNode_Read_Index, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'IsVisible', TTreeNode_Read_IsVisible, 0, [varEmpty], varEmpty);
    AddIGet(TTreeNode, 'Item', TTreeNode_Read_Item, 1, [varEmpty], varEmpty);
    AddISet(TTreeNode, 'Item', TTreeNode_Write_Item, 1, [varNull]);
    AddGet(TTreeNode, 'ItemId', TTreeNode_Read_ItemId, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'Level', TTreeNode_Read_Level, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'OverlayIndex', TTreeNode_Read_OverlayIndex, 0, [varEmpty], varEmpty);
    AddSet(TTreeNode, 'OverlayIndex', TTreeNode_Write_OverlayIndex, 0, [varEmpty]);
    AddGet(TTreeNode, 'Owner', TTreeNode_Read_Owner, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'Parent', TTreeNode_Read_Parent, 0, [varEmpty], varEmpty);
    AddGet(TTreeNode, 'SelectedIndex', TTreeNode_Read_SelectedIndex, 0, [varEmpty], varEmpty);
    AddSet(TTreeNode, 'SelectedIndex', TTreeNode_Write_SelectedIndex, 0, [varEmpty]);
    AddGet(TTreeNode, 'StateIndex', TTreeNode_Read_StateIndex, 0, [varEmpty], varEmpty);
    AddSet(TTreeNode, 'StateIndex', TTreeNode_Write_StateIndex, 0, [varEmpty]);
    AddGet(TTreeNode, 'Text', TTreeNode_Read_Text, 0, [varEmpty], varEmpty);
    AddSet(TTreeNode, 'Text', TTreeNode_Write_Text, 0, [varEmpty]);
    AddGet(TTreeNode, 'TreeView', TTreeNode_Read_TreeView, 0, [varEmpty], varEmpty);
    { TTreeNodes }
    AddClass(cComCtrls, TTreeNodes, 'TTreeNodes');
    AddGet(TTreeNodes, 'Create', TTreeNodes_Create, 1, [varEmpty], varEmpty);
    AddGet(TTreeNodes, 'AddChildFirst', TTreeNodes_AddChildFirst, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TTreeNodes, 'AddChild', TTreeNodes_AddChild, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TTreeNodes, 'AddChildObjectFirst', TTreeNodes_AddChildObjectFirst, 3, [varEmpty, varEmpty, varEmpty],
      varEmpty);
    AddGet(TTreeNodes, 'AddChildObject', TTreeNodes_AddChildObject, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TTreeNodes, 'AddFirst', TTreeNodes_AddFirst, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TTreeNodes, 'Add', TTreeNodes_Add, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TTreeNodes, 'AddObjectFirst', TTreeNodes_AddObjectFirst, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TTreeNodes, 'AddObject', TTreeNodes_AddObject, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TTreeNodes, 'Assign', TTreeNodes_Assign, 1, [varEmpty], varEmpty);
    AddGet(TTreeNodes, 'BeginUpdate', TTreeNodes_BeginUpdate, 0, [varEmpty], varEmpty);
    AddGet(TTreeNodes, 'Clear', TTreeNodes_Clear, 0, [varEmpty], varEmpty);
    AddGet(TTreeNodes, 'Delete', TTreeNodes_Delete, 1, [varEmpty], varEmpty);
    AddGet(TTreeNodes, 'EndUpdate', TTreeNodes_EndUpdate, 0, [varEmpty], varEmpty);
    AddGet(TTreeNodes, 'GetFirstNode', TTreeNodes_GetFirstNode, 0, [varEmpty], varEmpty);
    AddGet(TTreeNodes, 'GetNode', TTreeNodes_GetNode, 1, [varEmpty], varEmpty);
    AddGet(TTreeNodes, 'Insert', TTreeNodes_Insert, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TTreeNodes, 'InsertObject', TTreeNodes_InsertObject, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TTreeNodes, 'Count', TTreeNodes_Read_Count, 0, [varEmpty], varEmpty);
    AddGet(TTreeNodes, 'Handle', TTreeNodes_Read_Handle, 0, [varEmpty], varEmpty);
    AddGet(TTreeNodes, 'Item', TTreeNodes_Read_Item, 1, [varEmpty], varEmpty);
    AddGet(TTreeNodes, 'Owner', TTreeNodes_Read_Owner, 0, [varEmpty], varEmpty);
    { THitTest }
    AddConst(cComCtrls, 'htAbove', Ord(htAbove));
    AddConst(cComCtrls, 'htBelow', Ord(htBelow));
    AddConst(cComCtrls, 'htNowhere', Ord(htNowhere));
    AddConst(cComCtrls, 'htOnItem', Ord(htOnItem));
    AddConst(cComCtrls, 'htOnButton', Ord(htOnButton));
    AddConst(cComCtrls, 'htOnIcon', Ord(htOnIcon));
    AddConst(cComCtrls, 'htOnIndent', Ord(htOnIndent));
    AddConst(cComCtrls, 'htOnLabel', Ord(htOnLabel));
    AddConst(cComCtrls, 'htOnRight', Ord(htOnRight));
    AddConst(cComCtrls, 'htOnStateIcon', Ord(htOnStateIcon));
    AddConst(cComCtrls, 'htToLeft', Ord(htToLeft));
    AddConst(cComCtrls, 'htToRight', Ord(htToRight));
    { TSortType }
    AddConst(cComCtrls, 'stNone', Ord(stNone));
    AddConst(cComCtrls, 'stData', Ord(stData));
    AddConst(cComCtrls, 'stText', Ord(stText));
    AddConst(cComCtrls, 'stBoth', Ord(stBoth));
    { TCustomTreeView }
    AddClass(cComCtrls, TCustomTreeView, 'TCustomTreeView');
    AddGet(TCustomTreeView, 'AlphaSort', TCustomTreeView_AlphaSort, 0, [varEmpty], varEmpty);
    AddGet(TCustomTreeView, 'CustomSort', TCustomTreeView_CustomSort, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomTreeView, 'FullCollapse', TCustomTreeView_FullCollapse, 0, [varEmpty], varEmpty);
    AddGet(TCustomTreeView, 'FullExpand', TCustomTreeView_FullExpand, 0, [varEmpty], varEmpty);
    AddGet(TCustomTreeView, 'GetHitTestInfoAt', TCustomTreeView_GetHitTestInfoAt, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomTreeView, 'GetNodeAt', TCustomTreeView_GetNodeAt, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomTreeView, 'IsEditing', TCustomTreeView_IsEditing, 0, [varEmpty], varEmpty);
    AddGet(TCustomTreeView, 'LoadFromFile', TCustomTreeView_LoadFromFile, 1, [varEmpty], varEmpty);
    AddGet(TCustomTreeView, 'LoadFromStream', TCustomTreeView_LoadFromStream, 1, [varEmpty], varEmpty);
    AddGet(TCustomTreeView, 'SaveToFile', TCustomTreeView_SaveToFile, 1, [varEmpty], varEmpty);
    AddGet(TCustomTreeView, 'SaveToStream', TCustomTreeView_SaveToStream, 1, [varEmpty], varEmpty);
    AddGet(TCustomTreeView, 'DropTarget', TCustomTreeView_Read_DropTarget, 0, [varEmpty], varEmpty);
    AddSet(TCustomTreeView, 'DropTarget', TCustomTreeView_Write_DropTarget, 0, [varEmpty]);
    AddGet(TCustomTreeView, 'Selected', TCustomTreeView_Read_Selected, 0, [varEmpty], varEmpty);
    AddSet(TCustomTreeView, 'Selected', TCustomTreeView_Write_Selected, 0, [varEmpty]);
    AddGet(TCustomTreeView, 'TopItem', TCustomTreeView_Read_TopItem, 0, [varEmpty], varEmpty);
    AddSet(TCustomTreeView, 'TopItem', TCustomTreeView_Write_TopItem, 0, [varEmpty]);
    { TTreeView }
    AddClass(cComCtrls, TTreeView, 'TTreeView');
    AddGet(TTreeView, 'Create', TTreeView_Create, 1, [varEmpty], varEmpty);
    { TTrackBarOrientation }
    AddConst(cComCtrls, 'trHorizontal', Ord(trHorizontal));
    AddConst(cComCtrls, 'trVertical', Ord(trVertical));
    { TTickMark }
    AddConst(cComCtrls, 'tmBottomRight', Ord(tmBottomRight));
    AddConst(cComCtrls, 'tmTopLeft', Ord(tmTopLeft));
    AddConst(cComCtrls, 'tmBoth', Ord(tmBoth));
    { TTickStyle }
    AddConst(cComCtrls, 'tsNone', Ord(tsNone));
    AddConst(cComCtrls, 'tsAuto', Ord(tsAuto));
    AddConst(cComCtrls, 'tsManual', Ord(tsManual));
    { TTrackBar }
    AddClass(cComCtrls, TTrackBar, 'TTrackBar');
    AddGet(TTrackBar, 'Create', TTrackBar_Create, 1, [varEmpty], varEmpty);
    AddGet(TTrackBar, 'SetTick', TTrackBar_SetTick, 1, [varEmpty], varEmpty);
    { TProgressBar }
    AddClass(cComCtrls, TProgressBar, 'TProgressBar');
    AddGet(TProgressBar, 'Create', TProgressBar_Create, 1, [varEmpty], varEmpty);
    AddGet(TProgressBar, 'StepIt', TProgressBar_StepIt, 0, [varEmpty], varEmpty);
    AddGet(TProgressBar, 'StepBy', TProgressBar_StepBy, 1, [varEmpty], varEmpty);
    { TAttributeType }
    AddConst(cComCtrls, 'atSelected', Ord(atSelected));
    AddConst(cComCtrls, 'atDefaultText', Ord(atDefaultText));
    { TConsistentAttribute }
    AddConst(cComCtrls, 'caBold', Ord(caBold));
    AddConst(cComCtrls, 'caColor', Ord(caColor));
    AddConst(cComCtrls, 'caFace', Ord(caFace));
    AddConst(cComCtrls, 'caItalic', Ord(caItalic));
    AddConst(cComCtrls, 'caSize', Ord(caSize));
    AddConst(cComCtrls, 'caStrikeOut', Ord(caStrikeOut));
    AddConst(cComCtrls, 'caUnderline', Ord(caUnderline));
    AddConst(cComCtrls, 'caProtected', Ord(caProtected));
    { TTextAttributes }
    AddClass(cComCtrls, TTextAttributes, 'TTextAttributes');
    AddGet(TTextAttributes, 'Create', TTextAttributes_Create, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TTextAttributes, 'Assign', TTextAttributes_Assign, 1, [varEmpty], varEmpty);
    AddGet(TTextAttributes, 'Charset', TTextAttributes_Read_Charset, 0, [varEmpty], varEmpty);
    AddSet(TTextAttributes, 'Charset', TTextAttributes_Write_Charset, 0, [varEmpty]);
    AddGet(TTextAttributes, 'Color', TTextAttributes_Read_Color, 0, [varEmpty], varEmpty);
    AddSet(TTextAttributes, 'Color', TTextAttributes_Write_Color, 0, [varEmpty]);
    AddGet(TTextAttributes, 'ConsistentAttributes', TTextAttributes_Read_ConsistentAttributes, 0, [varEmpty], varEmpty);
    AddGet(TTextAttributes, 'Name', TTextAttributes_Read_Name, 0, [varEmpty], varEmpty);
    AddSet(TTextAttributes, 'Name', TTextAttributes_Write_Name, 0, [varEmpty]);
    AddGet(TTextAttributes, 'Pitch', TTextAttributes_Read_Pitch, 0, [varEmpty], varEmpty);
    AddSet(TTextAttributes, 'Pitch', TTextAttributes_Write_Pitch, 0, [varEmpty]);
    AddGet(TTextAttributes, 'Protected', TTextAttributes_Read_Protected, 0, [varEmpty], varEmpty);
    AddSet(TTextAttributes, 'Protected', TTextAttributes_Write_Protected, 0, [varEmpty]);
    AddGet(TTextAttributes, 'Size', TTextAttributes_Read_Size, 0, [varEmpty], varEmpty);
    AddSet(TTextAttributes, 'Size', TTextAttributes_Write_Size, 0, [varEmpty]);
    AddGet(TTextAttributes, 'Style', TTextAttributes_Read_Style, 0, [varEmpty], varEmpty);
    AddSet(TTextAttributes, 'Style', TTextAttributes_Write_Style, 0, [varEmpty]);
    AddGet(TTextAttributes, 'Height', TTextAttributes_Read_Height, 0, [varEmpty], varEmpty);
    AddSet(TTextAttributes, 'Height', TTextAttributes_Write_Height, 0, [varEmpty]);
    { TNumberingStyle }
    AddConst(cComCtrls, 'nsNone', Ord(nsNone));
    AddConst(cComCtrls, 'nsBullet', Ord(nsBullet));
    { TParaAttributes }
    AddClass(cComCtrls, TParaAttributes, 'TParaAttributes');
    AddGet(TParaAttributes, 'Create', TParaAttributes_Create, 1, [varEmpty], varEmpty);
    AddGet(TParaAttributes, 'Assign', TParaAttributes_Assign, 1, [varEmpty], varEmpty);
    AddGet(TParaAttributes, 'Alignment', TParaAttributes_Read_Alignment, 0, [varEmpty], varEmpty);
    AddSet(TParaAttributes, 'Alignment', TParaAttributes_Write_Alignment, 0, [varEmpty]);
    AddGet(TParaAttributes, 'FirstIndent', TParaAttributes_Read_FirstIndent, 0, [varEmpty], varEmpty);
    AddSet(TParaAttributes, 'FirstIndent', TParaAttributes_Write_FirstIndent, 0, [varEmpty]);
    AddGet(TParaAttributes, 'LeftIndent', TParaAttributes_Read_LeftIndent, 0, [varEmpty], varEmpty);
    AddSet(TParaAttributes, 'LeftIndent', TParaAttributes_Write_LeftIndent, 0, [varEmpty]);
    AddGet(TParaAttributes, 'Numbering', TParaAttributes_Read_Numbering, 0, [varEmpty], varEmpty);
    AddSet(TParaAttributes, 'Numbering', TParaAttributes_Write_Numbering, 0, [varEmpty]);
    AddGet(TParaAttributes, 'RightIndent', TParaAttributes_Read_RightIndent, 0, [varEmpty], varEmpty);
    AddSet(TParaAttributes, 'RightIndent', TParaAttributes_Write_RightIndent, 0, [varEmpty]);
    AddGet(TParaAttributes, 'Tab', TParaAttributes_Read_Tab, 1, [varEmpty], varEmpty);
    AddSet(TParaAttributes, 'Tab', TParaAttributes_Write_Tab, 1, [varNull]);
    AddGet(TParaAttributes, 'TabCount', TParaAttributes_Read_TabCount, 0, [varEmpty], varEmpty);
    AddSet(TParaAttributes, 'TabCount', TParaAttributes_Write_TabCount, 0, [varEmpty]);
    { TSearchType }
    AddConst(cComCtrls, 'stWholeWord', Ord(stWholeWord));
    AddConst(cComCtrls, 'stMatchCase', Ord(stMatchCase));
    { TCustomRichEdit }
    AddClass(cComCtrls, TCustomRichEdit, 'TCustomRichEdit');
    AddGet(TCustomRichEdit, 'Clear', TCustomRichEdit_Clear, 0, [varEmpty], varEmpty);
    AddGet(TCustomRichEdit, 'FindText', TCustomRichEdit_FindText, 4, [varEmpty, varEmpty, varEmpty, varEmpty],
      varEmpty);
    AddGet(TCustomRichEdit, 'GetSelTextBuf', TCustomRichEdit_GetSelTextBuf, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomRichEdit, 'Print', TCustomRichEdit_Print, 1, [varEmpty], varEmpty);
    AddGet(TCustomRichEdit, 'DefAttributes', TCustomRichEdit_Read_DefAttributes, 0, [varEmpty], varEmpty);
    AddSet(TCustomRichEdit, 'DefAttributes', TCustomRichEdit_Write_DefAttributes, 0, [varEmpty]);
    AddGet(TCustomRichEdit, 'SelAttributes', TCustomRichEdit_Read_SelAttributes, 0, [varEmpty], varEmpty);
    AddSet(TCustomRichEdit, 'SelAttributes', TCustomRichEdit_Write_SelAttributes, 0, [varEmpty]);
    AddGet(TCustomRichEdit, 'PageRect', TCustomRichEdit_Read_PageRect, 0, [varEmpty], varEmpty);
    AddSet(TCustomRichEdit, 'PageRect', TCustomRichEdit_Write_PageRect, 0, [varEmpty]);
    AddGet(TCustomRichEdit, 'Paragraph', TCustomRichEdit_Read_Paragraph, 0, [varEmpty], varEmpty);
    { TRichEdit }
    AddClass(cComCtrls, TRichEdit, 'TRichEdit');
    AddGet(TRichEdit, 'Create', TRichEdit_Create, 1, [varEmpty], varEmpty);
    { TUDAlignButton }
    AddConst(cComCtrls, 'udLeft', Ord(udLeft));
    AddConst(cComCtrls, 'udRight', Ord(udRight));
    { TUDOrientation }
    AddConst(cComCtrls, 'udHorizontal', Ord(udHorizontal));
    AddConst(cComCtrls, 'udVertical', Ord(udVertical));
    { TUDBtnType }
    AddConst(cComCtrls, 'btNext', Ord(btNext));
    AddConst(cComCtrls, 'btPrev', Ord(btPrev));
    { TCustomUpDown }
    AddClass(cComCtrls, TCustomUpDown, 'TCustomUpDown');
    { TUpDown }
    AddClass(cComCtrls, TUpDown, 'TUpDown');
    AddGet(TUpDown, 'Create', TUpDown_Create, 1, [varEmpty], varEmpty);
    { THKModifier }
    AddConst(cComCtrls, 'hkShift', Ord(hkShift));
    AddConst(cComCtrls, 'hkCtrl', Ord(hkCtrl));
    AddConst(cComCtrls, 'hkAlt', Ord(hkAlt));
    AddConst(cComCtrls, 'hkExt', Ord(hkExt));
    { THKInvalidKey }
    AddConst(cComCtrls, 'hcNone', Ord(hcNone));
    AddConst(cComCtrls, 'hcShift', Ord(hcShift));
    AddConst(cComCtrls, 'hcCtrl', Ord(hcCtrl));
    AddConst(cComCtrls, 'hcAlt', Ord(hcAlt));
    AddConst(cComCtrls, 'hcShiftCtrl', Ord(hcShiftCtrl));
    AddConst(cComCtrls, 'hcShiftAlt', Ord(hcShiftAlt));
    AddConst(cComCtrls, 'hcCtrlAlt', Ord(hcCtrlAlt));
    AddConst(cComCtrls, 'hcShiftCtrlAlt', Ord(hcShiftCtrlAlt));
    { TCustomHotKey }
    AddClass(cComCtrls, TCustomHotKey, 'TCustomHotKey');
    { THotKey }
    AddClass(cComCtrls, THotKey, 'THotKey');
    AddGet(THotKey, 'Create', THotKey_Create, 1, [varEmpty], varEmpty);
    { TListColumn }
    AddClass(cComCtrls, TListColumn, 'TListColumn');
    AddGet(TListColumn, 'Create', TListColumn_Create, 1, [varEmpty], varEmpty);
    AddGet(TListColumn, 'Assign', TListColumn_Assign, 1, [varEmpty], varEmpty);
    AddGet(TListColumn, 'WidthType', TListColumn_Read_WidthType, 0, [varEmpty], varEmpty);
    { TListColumns }
    AddClass(cComCtrls, TListColumns, 'TListColumns');
    AddGet(TListColumns, 'Create', TListColumns_Create, 1, [varEmpty], varEmpty);
    AddGet(TListColumns, 'Add', TListColumns_Add, 0, [varEmpty], varEmpty);
    AddGet(TListColumns, 'Owner', TListColumns_Read_Owner, 0, [varEmpty], varEmpty);
    AddIGet(TListColumns, 'Items', TListColumns_Read_Items, 1, [varEmpty], varEmpty);
    AddISet(TListColumns, 'Items', TListColumns_Write_Items, 1, [varNull]);
    { TDisplayCode }
    AddConst(cComCtrls, 'drBounds', Ord(drBounds));
    AddConst(cComCtrls, 'drIcon', Ord(drIcon));
    AddConst(cComCtrls, 'drLabel', Ord(drLabel));
    AddConst(cComCtrls, 'drSelectBounds', Ord(drSelectBounds));
    { TListItem }
    AddClass(cComCtrls, TListItem, 'TListItem');
    AddGet(TListItem, 'Create', TListItem_Create, 1, [varEmpty], varEmpty);
    AddGet(TListItem, 'CancelEdit', TListItem_CancelEdit, 0, [varEmpty], varEmpty);
    AddGet(TListItem, 'Delete', TListItem_Delete, 0, [varEmpty], varEmpty);
    AddGet(TListItem, 'DisplayRect', TListItem_DisplayRect, 1, [varEmpty], varEmpty);
    AddGet(TListItem, 'EditCaption', TListItem_EditCaption, 0, [varEmpty], varEmpty);
    AddGet(TListItem, 'GetPosition', TListItem_GetPosition, 0, [varEmpty], varEmpty);
    AddGet(TListItem, 'MakeVisible', TListItem_MakeVisible, 1, [varEmpty], varEmpty);
    AddGet(TListItem, 'Update', TListItem_Update, 0, [varEmpty], varEmpty);
    AddGet(TListItem, 'SetPosition', TListItem_SetPosition, 1, [varEmpty], varEmpty);
    AddGet(TListItem, 'Caption', TListItem_Read_Caption, 0, [varEmpty], varEmpty);
    AddSet(TListItem, 'Caption', TListItem_Write_Caption, 0, [varEmpty]);
    AddGet(TListItem, 'Checked', TListItem_Read_Checked, 0, [varEmpty], varEmpty);
    AddSet(TListItem, 'Checked', TListItem_Write_Checked, 0, [varEmpty]);
    AddGet(TListItem, 'Cut', TListItem_Read_Cut, 0, [varEmpty], varEmpty);
    AddSet(TListItem, 'Cut', TListItem_Write_Cut, 0, [varEmpty]);
    AddGet(TListItem, 'Data', TListItem_Read_Data, 0, [varEmpty], varEmpty);
    AddSet(TListItem, 'Data', TListItem_Write_Data, 0, [varEmpty]);
    AddGet(TListItem, 'DropTarget', TListItem_Read_DropTarget, 0, [varEmpty], varEmpty);
    AddSet(TListItem, 'DropTarget', TListItem_Write_DropTarget, 0, [varEmpty]);
    AddGet(TListItem, 'Focused', TListItem_Read_Focused, 0, [varEmpty], varEmpty);
    AddSet(TListItem, 'Focused', TListItem_Write_Focused, 0, [varEmpty]);
    AddGet(TListItem, 'Handle', TListItem_Read_Handle, 0, [varEmpty], varEmpty);
    AddGet(TListItem, 'ImageIndex', TListItem_Read_ImageIndex, 0, [varEmpty], varEmpty);
    AddSet(TListItem, 'ImageIndex', TListItem_Write_ImageIndex, 0, [varEmpty]);
    AddGet(TListItem, 'Index', TListItem_Read_Index, 0, [varEmpty], varEmpty);
    AddGet(TListItem, 'Left', TListItem_Read_Left, 0, [varEmpty], varEmpty);
    AddSet(TListItem, 'Left', TListItem_Write_Left, 0, [varEmpty]);
    AddGet(TListItem, 'ListView', TListItem_Read_ListView, 0, [varEmpty], varEmpty);
    AddGet(TListItem, 'Owner', TListItem_Read_Owner, 0, [varEmpty], varEmpty);
    AddGet(TListItem, 'OverlayIndex', TListItem_Read_OverlayIndex, 0, [varEmpty], varEmpty);
    AddSet(TListItem, 'OverlayIndex', TListItem_Write_OverlayIndex, 0, [varEmpty]);
    AddGet(TListItem, 'Selected', TListItem_Read_Selected, 0, [varEmpty], varEmpty);
    AddSet(TListItem, 'Selected', TListItem_Write_Selected, 0, [varEmpty]);
    AddGet(TListItem, 'StateIndex', TListItem_Read_StateIndex, 0, [varEmpty], varEmpty);
    AddSet(TListItem, 'StateIndex', TListItem_Write_StateIndex, 0, [varEmpty]);
    AddIGet(TListItem, 'SubItems', TListItem_Read_SubItems, 0, [varEmpty], varEmpty);
    AddISet(TListItem, 'SubItems', TListItem_Write_SubItems, 0, [varEmpty]);
    AddGet(TListItem, 'Top', TListItem_Read_Top, 0, [varEmpty], varEmpty);
    AddSet(TListItem, 'Top', TListItem_Write_Top, 0, [varEmpty]);
    { TListItems }
    AddClass(cComCtrls, TListItems, 'TListItems');
    AddGet(TListItems, 'Create', TListItems_Create, 1, [varEmpty], varEmpty);
    AddGet(TListItems, 'Add', TListItems_Add, 0, [varEmpty], varEmpty);
    AddGet(TListItems, 'Assign', TListItems_Assign, 1, [varEmpty], varEmpty);
    AddGet(TListItems, 'BeginUpdate', TListItems_BeginUpdate, 0, [varEmpty], varEmpty);
    AddGet(TListItems, 'Clear', TListItems_Clear, 0, [varEmpty], varEmpty);
    AddGet(TListItems, 'Delete', TListItems_Delete, 1, [varEmpty], varEmpty);
    AddGet(TListItems, 'EndUpdate', TListItems_EndUpdate, 0, [varEmpty], varEmpty);
    AddGet(TListItems, 'IndexOf', TListItems_IndexOf, 1, [varEmpty], varEmpty);
    AddGet(TListItems, 'Insert', TListItems_Insert, 1, [varEmpty], varEmpty);
    AddGet(TListItems, 'Count', TListItems_Read_Count, 0, [varEmpty], varEmpty);
    AddGet(TListItems, 'Handle', TListItems_Read_Handle, 0, [varEmpty], varEmpty);
    AddIGet(TListItems, 'Item', TListItems_Read_Item, 1, [varEmpty], varEmpty);
    AddISet(TListItems, 'Item', TListItems_Write_Item, 1, [varNull]);
    AddGet(TListItems, 'Owner', TListItems_Read_Owner, 0, [varEmpty], varEmpty);
    { TIconArrangement }
    AddConst(cComCtrls, 'iaTop', Ord(iaTop));
    AddConst(cComCtrls, 'iaLeft', Ord(iaLeft));
    { TListArrangement }
    AddConst(cComCtrls, 'arAlignBottom', Ord(arAlignBottom));
    AddConst(cComCtrls, 'arAlignLeft', Ord(arAlignLeft));
    AddConst(cComCtrls, 'arAlignRight', Ord(arAlignRight));
    AddConst(cComCtrls, 'arAlignTop', Ord(arAlignTop));
    AddConst(cComCtrls, 'arDefault', Ord(arDefault));
    AddConst(cComCtrls, 'arSnapToGrid', Ord(arSnapToGrid));
    { TViewStyle }
    AddConst(cComCtrls, 'vsIcon', Ord(vsIcon));
    AddConst(cComCtrls, 'vsSmallIcon', Ord(vsSmallIcon));
    AddConst(cComCtrls, 'vsList', Ord(vsList));
    AddConst(cComCtrls, 'vsReport', Ord(vsReport));
    { TItemState }
    AddConst(cComCtrls, 'isNone', Ord(isNone));
    AddConst(cComCtrls, 'isCut', Ord(isCut));
    AddConst(cComCtrls, 'isDropHilited', Ord(isDropHilited));
    AddConst(cComCtrls, 'isFocused', Ord(isFocused));
    AddConst(cComCtrls, 'isSelected', Ord(isSelected));
    { TItemChange }
    AddConst(cComCtrls, 'ctText', Ord(ctText));
    AddConst(cComCtrls, 'ctImage', Ord(ctImage));
    AddConst(cComCtrls, 'ctState', Ord(ctState));
    { TSearchDirection }
    AddConst(cComCtrls, 'sdLeft', Ord(sdLeft));
    AddConst(cComCtrls, 'sdRight', Ord(sdRight));
    AddConst(cComCtrls, 'sdAbove', Ord(sdAbove));
    AddConst(cComCtrls, 'sdBelow', Ord(sdBelow));
    AddConst(cComCtrls, 'sdAll', Ord(sdAll));
    { TCustomListView }
    AddClass(cComCtrls, TCustomListView, 'TCustomListView');
    AddGet(TCustomListView, 'AlphaSort', TCustomListView_AlphaSort, 0, [varEmpty], varEmpty);
    AddGet(TCustomListView, 'Arrange', TCustomListView_Arrange, 1, [varEmpty], varEmpty);
    AddGet(TCustomListView, 'FindCaption', TCustomListView_FindCaption, 5, [varEmpty, varEmpty, varEmpty, varEmpty,
      varEmpty], varEmpty);
    AddGet(TCustomListView, 'FindData', TCustomListView_FindData, 4, [varEmpty, varEmpty, varEmpty, varEmpty],
      varEmpty);
    AddGet(TCustomListView, 'GetItemAt', TCustomListView_GetItemAt, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomListView, 'GetNearestItem', TCustomListView_GetNearestItem, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomListView, 'GetNextItem', TCustomListView_GetNextItem, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TCustomListView, 'GetSearchString', TCustomListView_GetSearchString, 0, [varEmpty], varEmpty);
    AddGet(TCustomListView, 'IsEditing', TCustomListView_IsEditing, 0, [varEmpty], varEmpty);
    AddGet(TCustomListView, 'Scroll', TCustomListView_Scroll, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomListView, 'Checkboxes', TCustomListView_Read_Checkboxes, 0, [varEmpty], varEmpty);
    AddSet(TCustomListView, 'Checkboxes', TCustomListView_Write_Checkboxes, 0, [varEmpty]);
    AddGet(TCustomListView, 'Column', TCustomListView_Read_Column, 1, [varEmpty], varEmpty);
    AddGet(TCustomListView, 'DropTarget', TCustomListView_Read_DropTarget, 0, [varEmpty], varEmpty);
    AddSet(TCustomListView, 'DropTarget', TCustomListView_Write_DropTarget, 0, [varEmpty]);
    AddGet(TCustomListView, 'GridLines', TCustomListView_Read_GridLines, 0, [varEmpty], varEmpty);
    AddSet(TCustomListView, 'GridLines', TCustomListView_Write_GridLines, 0, [varEmpty]);
    AddGet(TCustomListView, 'HotTrack', TCustomListView_Read_HotTrack, 0, [varEmpty], varEmpty);
    AddSet(TCustomListView, 'HotTrack', TCustomListView_Write_HotTrack, 0, [varEmpty]);
    AddGet(TCustomListView, 'ItemFocused', TCustomListView_Read_ItemFocused, 0, [varEmpty], varEmpty);
    AddSet(TCustomListView, 'ItemFocused', TCustomListView_Write_ItemFocused, 0, [varEmpty]);
    AddGet(TCustomListView, 'RowSelect', TCustomListView_Read_RowSelect, 0, [varEmpty], varEmpty);
    AddSet(TCustomListView, 'RowSelect', TCustomListView_Write_RowSelect, 0, [varEmpty]);
    AddGet(TCustomListView, 'SelCount', TCustomListView_Read_SelCount, 0, [varEmpty], varEmpty);
    AddGet(TCustomListView, 'Selected', TCustomListView_Read_Selected, 0, [varEmpty], varEmpty);
    AddSet(TCustomListView, 'Selected', TCustomListView_Write_Selected, 0, [varEmpty]);
    AddGet(TCustomListView, 'CustomSort', TCustomListView_CustomSort, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomListView, 'StringWidth', TCustomListView_StringWidth, 1, [varEmpty], varEmpty);
    AddGet(TCustomListView, 'UpdateItems', TCustomListView_UpdateItems, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomListView, 'TopItem', TCustomListView_Read_TopItem, 0, [varEmpty], varEmpty);
    AddGet(TCustomListView, 'ViewOrigin', TCustomListView_Read_ViewOrigin, 0, [varEmpty], varEmpty);
    AddGet(TCustomListView, 'VisibleRowCount', TCustomListView_Read_VisibleRowCount, 0, [varEmpty], varEmpty);
    AddGet(TCustomListView, 'BoundingRect', TCustomListView_Read_BoundingRect, 0, [varEmpty], varEmpty);
    { TListView }
    AddClass(cComCtrls, TListView, 'TListView');
    AddGet(TListView, 'Create', TListView_Create, 1, [varEmpty], varEmpty);
    { TCommonAVI }
    AddConst(cComCtrls, 'aviNone', Ord(aviNone));
    AddConst(cComCtrls, 'aviFindFolder', Ord(aviFindFolder));
    AddConst(cComCtrls, 'aviFindFile', Ord(aviFindFile));
    AddConst(cComCtrls, 'aviFindComputer', Ord(aviFindComputer));
    AddConst(cComCtrls, 'aviCopyFiles', Ord(aviCopyFiles));
    AddConst(cComCtrls, 'aviCopyFile', Ord(aviCopyFile));
    AddConst(cComCtrls, 'aviRecycleFile', Ord(aviRecycleFile));
    AddConst(cComCtrls, 'aviEmptyRecycle', Ord(aviEmptyRecycle));
    AddConst(cComCtrls, 'aviDeleteFile', Ord(aviDeleteFile));
    { TAnimate }
    AddClass(cComCtrls, TAnimate, 'TAnimate');
    AddGet(TAnimate, 'Create', TAnimate_Create, 1, [varEmpty], varEmpty);
    AddGet(TAnimate, 'FrameCount', TAnimate_Read_FrameCount, 0, [varEmpty], varEmpty);
    AddGet(TAnimate, 'FrameHeight', TAnimate_Read_FrameHeight, 0, [varEmpty], varEmpty);
    AddGet(TAnimate, 'FrameWidth', TAnimate_Read_FrameWidth, 0, [varEmpty], varEmpty);
    AddGet(TAnimate, 'Open', TAnimate_Read_Open, 0, [varEmpty], varEmpty);
    AddSet(TAnimate, 'Open', TAnimate_Write_Open, 0, [varEmpty]);
    AddGet(TAnimate, 'Play', TAnimate_Play, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TAnimate, 'Reset', TAnimate_Reset, 0, [varEmpty], varEmpty);
    AddGet(TAnimate, 'Seek', TAnimate_Seek, 1, [varEmpty], varEmpty);
    AddGet(TAnimate, 'Stop', TAnimate_Stop, 0, [varEmpty], varEmpty);
    AddGet(TAnimate, 'ResHandle', TAnimate_Read_ResHandle, 0, [varEmpty], varEmpty);
    AddSet(TAnimate, 'ResHandle', TAnimate_Write_ResHandle, 0, [varEmpty]);
    AddGet(TAnimate, 'ResId', TAnimate_Read_ResId, 0, [varEmpty], varEmpty);
    AddSet(TAnimate, 'ResId', TAnimate_Write_ResId, 0, [varEmpty]);
    AddGet(TAnimate, 'ResName', TAnimate_Read_ResName, 0, [varEmpty], varEmpty);
    AddSet(TAnimate, 'ResName', TAnimate_Write_ResName, 0, [varEmpty]);
    AddHandler(cComCtrls, 'TTabChangingEvent', TJvInterpreterComCtrlsEvent,
      @TJvInterpreterComCtrlsEvent.TabChangingEvent);
    AddHandler(cComCtrls, 'TDrawPanelEvent', TJvInterpreterComCtrlsEvent, @TJvInterpreterComCtrlsEvent.DrawPanelEvent);
    AddHandler(cComCtrls, 'TDrawSectionEvent', TJvInterpreterComCtrlsEvent,
      @TJvInterpreterComCtrlsEvent.DrawSectionEvent);
    AddHandler(cComCtrls, 'TSectionNotifyEvent', TJvInterpreterComCtrlsEvent,
      @TJvInterpreterComCtrlsEvent.SectionNotifyEvent);
    AddHandler(cComCtrls, 'TSectionTrackEvent', TJvInterpreterComCtrlsEvent,
      @TJvInterpreterComCtrlsEvent.SectionTrackEvent);
    AddHandler(cComCtrls, 'TTVChangingEvent', TJvInterpreterComCtrlsEvent,
      @TJvInterpreterComCtrlsEvent.TVChangingEvent);
    AddHandler(cComCtrls, 'TTVChangedEvent', TJvInterpreterComCtrlsEvent, @TJvInterpreterComCtrlsEvent.TVChangedEvent);
    AddHandler(cComCtrls, 'TTVEditingEvent', TJvInterpreterComCtrlsEvent, @TJvInterpreterComCtrlsEvent.TVEditingEvent);
    AddHandler(cComCtrls, 'TTVEditedEvent', TJvInterpreterComCtrlsEvent, @TJvInterpreterComCtrlsEvent.TVEditedEvent);
    AddHandler(cComCtrls, 'TTVExpandingEvent', TJvInterpreterComCtrlsEvent,
      @TJvInterpreterComCtrlsEvent.TVExpandingEvent);
    AddHandler(cComCtrls, 'TTVCollapsingEvent', TJvInterpreterComCtrlsEvent,
      @TJvInterpreterComCtrlsEvent.TVCollapsingEvent);
    AddHandler(cComCtrls, 'TTVExpandedEvent', TJvInterpreterComCtrlsEvent,
      @TJvInterpreterComCtrlsEvent.TVExpandedEvent);
    AddHandler(cComCtrls, 'TTVCompareEvent', TJvInterpreterComCtrlsEvent, @TJvInterpreterComCtrlsEvent.TVCompareEvent);
    AddHandler(cComCtrls, 'TRichEditResizeEvent', TJvInterpreterComCtrlsEvent,
      @TJvInterpreterComCtrlsEvent.RichEditResizeEvent);
    AddHandler(cComCtrls, 'TRichEditProtectChange', TJvInterpreterComCtrlsEvent,
      @TJvInterpreterComCtrlsEvent.RichEditProtectChange);
    AddHandler(cComCtrls, 'TRichEditSaveClipboard', TJvInterpreterComCtrlsEvent,
      @TJvInterpreterComCtrlsEvent.RichEditSaveClipboard);
    AddHandler(cComCtrls, 'TUDClickEvent', TJvInterpreterComCtrlsEvent, @TJvInterpreterComCtrlsEvent.UDClickEvent);
    AddHandler(cComCtrls, 'TUDChangingEvent', TJvInterpreterComCtrlsEvent,
      @TJvInterpreterComCtrlsEvent.UDChangingEvent);
    AddHandler(cComCtrls, 'TLVDeletedEvent', TJvInterpreterComCtrlsEvent, @TJvInterpreterComCtrlsEvent.LVDeletedEvent);
    AddHandler(cComCtrls, 'TLVEditingEvent', TJvInterpreterComCtrlsEvent, @TJvInterpreterComCtrlsEvent.LVEditingEvent);
    AddHandler(cComCtrls, 'TLVEditedEvent', TJvInterpreterComCtrlsEvent, @TJvInterpreterComCtrlsEvent.LVEditedEvent);
    AddHandler(cComCtrls, 'TLVChangeEvent', TJvInterpreterComCtrlsEvent, @TJvInterpreterComCtrlsEvent.LVChangeEvent);
    AddHandler(cComCtrls, 'TLVChangingEvent', TJvInterpreterComCtrlsEvent,
      @TJvInterpreterComCtrlsEvent.LVChangingEvent);
    AddHandler(cComCtrls, 'TLVColumnClickEvent', TJvInterpreterComCtrlsEvent,
      @TJvInterpreterComCtrlsEvent.LVColumnClickEvent);
    AddHandler(cComCtrls, 'TLVCompareEvent', TJvInterpreterComCtrlsEvent, @TJvInterpreterComCtrlsEvent.LVCompareEvent);
  end;
  RegisterClasses([TTabSheet, TPageControl, TStatusPanel, TStatusPanels,
    TStatusBar, THeaderSection, THeaderSections, THeaderControl, TTreeNode,
      TTreeNodes, TTreeView, TTrackBar, TProgressBar, TTextAttributes,
      TParaAttributes, TRichEdit, TUpDown, THotKey, TListColumn, TListColumns,
      TListItem, TListItems, TListView , TAnimate]);
end;

end.

