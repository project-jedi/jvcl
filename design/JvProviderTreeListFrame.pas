{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProviderTreeListFrame.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):

Last Modified: 2003-07-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvProviderTreeListFrame;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls,
  JvDataProvider, JvDataProviderImpl;

type
  TGetVirtualRootEvent = procedure(Sender: TObject; var AVirtualRoot: IJvDataItem) of object;
  TfmeJvProviderTreeList = class(TFrame)
    lvProvider: TListView;
    procedure lvProviderCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvProviderData(Sender: TObject; Item: TListItem);
    procedure lvProviderDblClick(Sender: TObject);
    procedure lvProviderMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lvProviderResize(Sender: TObject);
    procedure lvProviderSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    { Private declarations }
    FConsumerSvc: TJvDataConsumer;
    FOnGetVirtualRoot: TGetVirtualRootEvent;
    FOnItemSelect: TNotifyEvent;
    FUseVirtualRoot: Boolean;
    FLastSelectIdx: Integer;
  protected
    { Protected declarations }
    FVirtualRoot: IJvDataItem;
    function DoGetVirtualRoot: IJvDataItem;
    procedure DoItemSelect;
    procedure SetUseVirtualRoot(Value: Boolean);
    function GetViewList: IJvDataConsumerViewList;
    function UsingVirtualRoot: Boolean;
    procedure UpdateColumnSize;
    procedure UpdateSelectedItem; virtual;
    procedure ConsumerChanged(Sender: TObject); virtual;
    procedure GenerateVirtualRoot; dynamic;
    property LastSelectIdx: Integer read FLastSelectIdx write FLastSelectIdx;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDataItem(Index: Integer): IJvDataItem; virtual;
    function LocateID(ID: string): Integer; virtual;
    procedure SelectItemID(ID: string);
    function GetSelectedIndex: Integer;
    property OnGetVirtualRoot: TGetVirtualRootEvent read FOnGetVirtualRoot write FOnGetVirtualRoot;
    property OnItemSelect: TNotifyEvent read FOnItemSelect write FOnItemSelect;
    property Provider: TJvDataConsumer read FConsumerSvc;
    property UseVirtualRoot: Boolean read FUseVirtualRoot write SetUseVirtualRoot;
  end;

implementation

uses
  Commctrl,
  JvDsgnConsts, JvConsts;

{$R *.DFM}

function GetItemIndexAt(LV: TListView; X, Y: Integer): Integer;
var
  Info: TLVHitTestInfo;
begin
  if LV.HandleAllocated then
  begin
    Info.pt := Point(X, Y);
    Result := ListView_HitTest(LV.Handle, Info);
  end
  else
    Result := -1;
end;

function TfmeJvProviderTreeList.DoGetVirtualRoot: IJvDataItem;
begin
  Result := nil;
  if @FOnGetVirtualRoot <> nil then
    OnGetVirtualRoot(Self, Result);
end;

procedure TfmeJvProviderTreeList.DoItemSelect;
begin
  if @FonItemSelect <> nil then
    OnItemSelect(Self);
end;

procedure TfmeJvProviderTreeList.SetUseVirtualRoot(Value: Boolean);
begin
  if Value <> UseVirtualRoot then
  begin
    FUseVirtualRoot := Value;
    if not Value then
      FVirtualRoot := nil
    else
      GenerateVirtualRoot;
    ConsumerChanged(Self);
  end;
end;

function TfmeJvProviderTreeList.GetViewList: IJvDataConsumerViewList;
begin
  Supports(Provider as IJvDataConsumer, IJvDataConsumerViewList, Result);
end;

function TfmeJvProviderTreeList.UsingVirtualRoot: Boolean;
begin
  Result := UseVirtualRoot and (FVirtualRoot <> nil);
end;

procedure TfmeJvProviderTreeList.UpdateColumnSize;
begin
  lvProvider.Columns[0].Width := lvProvider.ClientWidth;
end;

procedure TfmeJvProviderTreeList.UpdateSelectedItem;
begin
  DoItemSelect;
end;

procedure TfmeJvProviderTreeList.ConsumerChanged(Sender: TObject);
begin
  if UseVirtualRoot then
    GenerateVirtualRoot;
  if GetViewList = nil then
    lvProvider.Items.Count := 0
  else
    lvProvider.Items.Count := GetViewList.Count + Ord(UsingVirtualRoot);
  lvProvider.Invalidate;
end;

procedure TfmeJvProviderTreeList.GenerateVirtualRoot;
begin
  FVirtualRoot := DoGetVirtualRoot;
end;

constructor TfmeJvProviderTreeList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConsumerSvc := TJvDataConsumer.Create(Self, [DPA_RenderDisabledAsGrayed, DPA_ConsumerDisplaysList]);
  FConsumerSvc.OnChanged := ConsumerChanged;
end;

destructor TfmeJvProviderTreeList.Destroy;
begin
  FreeAndNil(FConsumerSvc);
  inherited Destroy;
end;

function TfmeJvProviderTreeList.GetDataItem(Index: Integer): IJvDataItem;
begin
  if UsingVirtualRoot and (Index = 0) then
    Result := FVirtualRoot
  else
    Result := GetViewList.Item(Index - Ord(UsingVirtualRoot));
end;

function TfmeJvProviderTreeList.LocateID(ID: string): Integer;
begin
  if UsingVirtualRoot and AnsiSameText(ID, FVirtualRoot.GetID) then
    Result := 0
  else
  begin
    Result := GetViewList.IndexOfID(ID);
    if UsingVirtualRoot and (Result >= 0) then
      Inc(Result);
  end;
end;

procedure TfmeJvProviderTreeList.SelectItemID(ID: string);
var
  I: Integer;
begin
  I := LocateID(ID);
  if I > -1 then
    ListView_SetItemState(lvProvider.Handle, I, LVIS_SELECTED or LVIS_FOCUSED,
      LVIS_SELECTED or LVIS_FOCUSED);
end;

function TfmeJvProviderTreeList.GetSelectedIndex: Integer;
begin
  Result := LastSelectIdx;
end;

procedure TfmeJvProviderTreeList.lvProviderCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var
  ACanvas: TCanvas;
  ARect: TRect;
  BtnWdth: Integer;
  MidX, MidY: Integer;
begin
  ACanvas := Sender.Canvas;
  DefaultDraw := False;
  ARect := Item.DisplayRect(drBounds);
  ARect.Right := Sender.ClientRect.Right;
  if Item.Selected then
  begin
    ACanvas.Brush.Color := clHighlight;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Font.Color := clHighlightText;
    ACanvas.FillRect(ARect);
  end
  else
  begin
    ACanvas.Brush.Color := clWindow;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Font.Color := clWindowText;
    ACanvas.FillRect(ARect);
  end;
  BtnWdth := Succ(ARect.Bottom - ARect.Top) + 2;
  ARect.Left := ARect.Left + (BtnWdth * Item.Indent);
  if (UsingVirtualRoot and (Item.Index = 0)) or
    GetViewList.ItemHasChildren(Item.Index - Ord(UsingVirtualRoot)) then
  begin
    with ACanvas do
    begin
      MidX := ARect.Left + (BtnWdth - 3) div 2;
      MidY := ARect.Top + (BtnWdth - 3) div 2;
      Pen.Color := ACanvas.Font.Color;
      Pen.Style := psSolid;
      Pen.Width := 1;
      MoveTo(ARect.Left + 3, ARect.Top + 3);
      LineTo(ARect.Left + BtnWdth - 6, ARect.Top + 3);
      LineTo(ARect.Left + BtnWdth - 6, ARect.Top + BtnWdth - 6);
      LineTo(ARect.Left + 3, ARect.Top + BtnWdth - 6);
      LineTo(ARect.Left + 3, ARect.Top + 3);

      MoveTo(ARect.Left + 5, MidY);
      LineTo(ARect.Left + BtnWdth - 7, MidY);

      if (not UsingVirtualRoot or (Item.Index <> 0)) and
        not GetViewList.ItemIsExpanded(Item.Index - Ord(UsingVirtualRoot)) then
      begin
        MoveTo(MidX, ARect.Top + 5);
        LineTo(MidX, ARect.Top + BtnWdth - 7);
      end;
    end;
  end;
  ARect.Left := ARect.Left + BtnWdth;
  DrawText(ACanvas.Handle, PChar(Item.Caption), Length(Item.Caption), ARect, DT_SINGLELINE + DT_LEFT + DT_END_ELLIPSIS);
end;

procedure TfmeJvProviderTreeList.lvProviderData(Sender: TObject;
  Item: TListItem);
var
  DataItem: IJvDataItem;
  ItemText: IJvDataItemText;
begin
  if (Provider.ProviderIntf = nil) or (Item.Index > GetViewList.Count) then
    Exit;
  if UsingVirtualRoot and (Item.Index = 0) then
  begin
    DataItem := FVirtualRoot;
    Item.Indent := 0;
  end
  else
  begin
    DataItem := GetViewList.Item(Item.Index - Ord(UsingVirtualRoot));
    Item.Indent := GetViewList.ItemLevel(Item.Index - Ord(UsingVirtualRoot)) + Ord(UsingVirtualRoot);
  end;
  if DataItem <> nil then
  begin
    if Supports(DataItem, IJvDataItemText, ItemText) then
      Item.Caption := ItemText.Caption
    else
    begin
      if DataItem = FVirtualRoot then
        Item.Caption := SDataItemRootCaption
      else
        Item.Caption := SDataItemNoTextIntf;
    end;
  end
end;

procedure TfmeJvProviderTreeList.lvProviderDblClick(Sender: TObject);
begin
  if lvProvider.Selected <> nil then
    if lvProvider.Selected.Index >= Ord(UsingVirtualRoot) then
      GetViewList.ToggleItem(lvProvider.Selected.Index - Ord(UsingVirtualRoot));
end;

procedure TfmeJvProviderTreeList.lvProviderMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: Integer;
  ItemLevel: Integer;
  TmpRect: TRect;
begin
  Item := GetItemIndexAt(lvProvider, X, Y);
  if Item <> -1 then
  begin
    if UsingVirtualRoot and (Item = 0) then
      ItemLevel := 0
    else
      ItemLevel := GetViewList.ItemLevel(Item - Ord(UsingVirtualRoot)) + Ord(UsingVirtualRoot);
    ListView_GetItemRect(lvProvider.Handle, Item, TmpRect, LVIR_BOUNDS);
    TmpRect.Right := TmpRect.Left + (Succ((TmpRect.Bottom - TmpRect.Top) + 2) * Succ(ItemLevel));
    if (X < TmpRect.Right) and (X > TmpRect.Right - ((TmpRect.Bottom - TmpRect.Top) + 2)) then
      if Item >= Ord(UsingVirtualRoot) then
        GetViewList.ToggleItem(Item - Ord(UsingVirtualRoot));
  end;
end;

procedure TfmeJvProviderTreeList.lvProviderResize(Sender: TObject);
begin
  UpdateColumnSize;
end;

procedure TfmeJvProviderTreeList.lvProviderSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateSelectedItem;
  if Selected then
    FLastSelectIdx := Item.Index
  else
    FLastSelectIdx := -1;
end;

end.
