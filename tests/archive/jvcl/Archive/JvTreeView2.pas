{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTreeView2.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvTreeView2;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Menus, ComCtrls, CommCtrl, JvTypes, JVCLVer;

type
  TJvTreeNode = class(TTreeNode)
  private
    FBold: Boolean;
    FChecked: Boolean;
    FPopupMenu: TPopupMenu;
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
    function GetBold: Boolean;
    procedure SetBold(const Value: Boolean);
    procedure SetPopupMenu(const Value: TPopupMenu);
  public
    constructor CreateEnh(AOwner: TTreeNodes);
    property Checked: Boolean read GetChecked write SetChecked;
    property Bold: Boolean read GetBold write SetBold;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
  end;

  TPageChangedEvent = procedure(Sender: TObject; Item: TTreeNode; Page: TTabSheet) of object;

  TJvTreeView2 = class(TTreeView)
  private
    FColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOver: Boolean;
    FCheckBoxes: Boolean;
    FOnHScroll: TNotifyEvent;
    FOnVScroll: TNotifyEvent;
    FMenu: TPageControl;
    FOnPage: TPageChangedEvent;
    FAboutJVCL: TJVCLAboutInfo;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure MouseEnter(var msg: Tmessage); message CM_MOUSEENTER;
    procedure MouseLeave(var msg: Tmessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure SetCheckBoxes(const Value: Boolean);
  protected
    function CreateNode: TTreeNode; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNotify(var msg: TWMNotify); message CN_NOTIFY;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property Checkboxes: Boolean read FCheckBoxes write SetCheckBoxes default False;
    function GetBold(Node: TTreeNode): Boolean;
    procedure SetBold(Node: TTreeNode; Value: Boolean);
    function GetChecked(Node: TTreenode): Boolean;
    procedure SetChecked(Node: TTreenode; Value: Boolean);
    procedure SetNodePopup(Node: TTreeNode; Value: TPopupMenu);
    function GetNodePopup(Node: TTreeNode): TPopupMenu;
    property OnVerticalScroll: TNotifyEvent read FOnVScroll write FOnVScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHScroll write FOnHScroll;
    property Menu: TPageControl read FMenu write FMenu;
    property OnPageChanged: TPageChangedEvent read FOnPage write FOnPage;
  end;

implementation

///////////////////////////////////////////////////////////
// TJvTreeNode
///////////////////////////////////////////////////////////

constructor TJvTreeNode.CreateEnh(AOwner: TTreeNodes);
begin
  inherited Create(AOwner);
  FPopupMenu := TPopupMenu.Create(AOwner.Owner);
end;

{***********************************************}

procedure TJvTreeNode.SetPopupMenu(const Value: TPopupMenu);
begin
  FPopupMenu := Value;
end;

{***********************************************}

function TJvTreeNode.GetBold: Boolean;
var
  Item: TTVItem;
begin
  with Item do
  begin
    mask := TVIF_STATE;
    hItem := ItemId;
    if TreeView_GetItem(Handle, Item) then
      Result := ((Item.State and TVIS_BOLD) = TVIS_BOLD)
    else
      Result := False;
  end;
end;

{***********************************************}

function TJvTreeNode.GetChecked: Boolean;
var
  Item: TTVItem;
begin
  with Item do
  begin
    mask := TVIF_STATE;
    hItem := ItemId;
    if TreeView_GetItem(Handle, Item) then
      Result := ((Item.State and TVIS_CHECKED) = TVIS_CHECKED)
    else
      Result := False;
  end;
end;

{***********************************************}

procedure TJvTreeNode.SetBold(const Value: Boolean);
var
  Item: TTVItem;
begin
  FBold := Value;
  FillChar(Item, SizeOf(Item), 0);
  with Item do
  begin
    mask := TVIF_STATE;
    hItem := ItemId;
    StateMask := TVIS_BOLD;
    if FBold then
      Item.State := TVIS_BOLD
    else
      Item.State := 0;
    TreeView_SetItem(Handle, Item);
  end;
end;

{***********************************************}

procedure TJvTreeNode.SetChecked(Value: Boolean);
var
  Item: TTVItem;
begin
  FChecked := Value;
  FillChar(Item, SizeOf(Item), 0);
  with Item do
  begin
    hItem := ItemId;
    mask := TVIF_STATE;
    StateMask := TVIS_STATEIMAGEMASK;
    if FChecked then
      Item.State := TVIS_CHECKED
    else
      Item.State := TVIS_CHECKED shr 1;
    TreeView_SetItem(Handle, Item);
  end;
end;

///////////////////////////////////////////////////////////
// TJvTreeView2
///////////////////////////////////////////////////////////

constructor TJvTreeView2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  FOver := False;
  FCheckBoxes := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

{***********************************************}

procedure TJvTreeView2.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if FCheckBoxes then
    Params.Style := Params.Style or TVS_CHECKBOXES;
end;

{***********************************************}

function TJvTreeView2.CreateNode: TTreeNode;
begin
  Result := TJvTreeNode.CreateEnh(Items);
end;

{***********************************************}

procedure TJvTreeView2.WMNotify(var msg: TWMNotify);
var
  Node: TTreeNode;
  Point: TPoint;
  i, j: Integer;
begin
  inherited;

  Point := Mouse.CursorPos;
  Point := ScreenToClient(point);
  with Msg, Point do
    case NMHdr^.code of
      NM_CLICK, NM_RCLICK:
        begin
          Node := GetNodeAt(x, y);
          if Assigned(Node) then
            Selected := Node
          else
          begin
            if FCheckBoxes then
            begin
              Node := GetNodeAt(x + 16, y);
              if Assigned(Node) then
                Selected := Node
            end;
          end;
          if (Selected <> nil) and (NMHdr^.code = NM_RCLICK) then
            TJvTreeNode(Selected).PopupMenu.Popup(Mouse.CursorPos.x, Mouse.CursorPos.y);
        end;
      TVN_SELCHANGEDA, TVN_SELCHANGEDW:
        begin
          if Assigned(FMenu) then
            if Selected <> nil then
            begin
              //Search for the correct page
              j := -1;
              for i := 0 to FMenu.PageCount - 1 do
                if FMenu.Pages[i].Caption = Selected.Text then
                  j := i;
              if j <> -1 then
              begin
                FMenu.ActivePage := FMenu.Pages[j];
                if Assigned(FOnPage) then
                  FOnPage(Self, Selected, FMenu.Pages[j]);
              end;
            end;
        end;

    end;
end;

{**************************************************}

procedure TJvTreeView2.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  if Assigned(FOnHScroll) then
    FOnHScroll(Self);
end;

{**************************************************}

procedure TJvTreeView2.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVScroll) then
    FOnVScroll(Self);
end;

{***********************************************}

procedure TJvTreeView2.MouseEnter(var msg: Tmessage);
begin
  FOver := True;
  FSaved := Application.HintColor;
  Application.HintColor := FColor;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvTreeView2.MouseLeave(var msg: Tmessage);
begin
  Application.HintColor := FSaved;
  FOver := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

procedure TJvTreeView2.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

{**************************************************}

procedure TJvTreeView2.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

function TJvTreeView2.GetBold(Node: TTreeNode): Boolean;
begin
  Result := TJvTreeNode(Node).Bold;
end;

{**************************************************}

procedure TJvTreeView2.SetBold(Node: TTreeNode; Value: Boolean);
begin
  TJvTreeNode(Node).Bold := Value;
end;

{**************************************************}

function TJvTreeView2.GetChecked(Node: TTreenode): Boolean;
begin
  Result := TJvTreeNode(Node).Checked;
end;

{**************************************************}

procedure TJvTreeView2.SetChecked(Node: TTreenode; Value: Boolean);
begin
  TJvTreeNode(Node).Checked := Value;
end;
{**************************************************}

procedure TJvTreeView2.SetCheckBoxes(const Value: Boolean);
begin
  FCheckBoxes := Value;
  RecreateWnd;
end;

{**************************************************}

function TJvTreeView2.GetNodePopup(Node: TTreeNode): TPopupMenu;
begin
  Result := TJvTreeNode(Node).PopupMenu;
end;

{**************************************************}

procedure TJvTreeView2.SetNodePopup(Node: TTreeNode; Value: TPopupMenu);
begin
  TJvTreeNode(Node).PopupMenu := Value;
end;

end.
