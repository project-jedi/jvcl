{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQProviderTreeListFrame;

interface

uses
  SysUtils, Classes,  
  Qt, QGraphics, QControls, QForms, QDialogs, QComCtrls, Types, QWindows, 
  JvQDataProvider, JvQDataProviderIntf;

type
  TGetVirtualRootEvent = procedure(Sender: TObject; var AVirtualRoot: IJvDataItem) of object;

  TMasterConsumer = class(TJvDataConsumer)
  private
    FSlave: TJvDataConsumer;
  protected
    OldOnChanged: TJvDataConsumerChangeEvent;
    procedure SetSlave(Value: TJvDataConsumer);
    procedure LimitSubServices(Sender: TJvDataConsumer;
      var SubSvcClass: TJvDataConsumerAggregatedObjectClass);
    procedure SlaveChanged(Sender: TJvDataConsumer; Reason: TJvDataConsumerChangeReason);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function ProviderIntf: IJvDataProvider; override;
    procedure SetProviderIntf(Value: IJvDataProvider); override;
    function ContextIntf: IJvDataContext; override;
    procedure SetContextIntf(Value: IJvDataContext); override;
    function GetInterface(const IID: TGUID; out Obj): Boolean; override;

    property Slave: TJvDataConsumer read FSlave write SetSlave;
  end;
  
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
    FConsumerSvc: TMasterConsumer;
    FOnGetVirtualRoot: TGetVirtualRootEvent;
    FOnItemSelect: TNotifyEvent;
    FUseVirtualRoot: Boolean;
    FLastSelectIdx: Integer;
  protected
    FVirtualRoot: IJvDataItem;
    function DoGetVirtualRoot: IJvDataItem;
    procedure DoItemSelect;
    procedure SetUseVirtualRoot(Value: Boolean);
    function GetViewList: IJvDataConsumerViewList;
    function UsingVirtualRoot: Boolean;
    procedure UpdateColumnSize;
    procedure NotifyConsumerItemSelect;
    procedure UpdateSelectedItem; virtual;
    procedure ConsumerChanged(Sender: TJvDataConsumer; Reason: TJvDataConsumerChangeReason); virtual;
    procedure GenerateVirtualRoot; dynamic;
    property LastSelectIdx: Integer read FLastSelectIdx write FLastSelectIdx;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDataItem(Index: Integer): IJvDataItem; virtual;
    function LocateID(ID: string): Integer; virtual;
    procedure SelectItemID(ID: string);
    function GetSelectedIndex: Integer;
    procedure UpdateViewList; virtual;
    property OnGetVirtualRoot: TGetVirtualRootEvent read FOnGetVirtualRoot write FOnGetVirtualRoot;
    property OnItemSelect: TNotifyEvent read FOnItemSelect write FOnItemSelect;
    property Provider: TMasterConsumer read FConsumerSvc;
    property UseVirtualRoot: Boolean read FUseVirtualRoot write SetUseVirtualRoot;
  end;

implementation

uses 
  JvQDsgnConsts, JvQConsts;



{$R *.xfm}


function GetItemIndexAt(LV: TListView; X, Y: Integer): Integer;
var  
  Item: TListItem; 
begin
  if LV.HandleAllocated then
  begin  
    Item := LV.GetItemAt(X, Y);
    if Item <> nil then
      Result := Item.Index
    else
      Result := -1; 
  end
  else
    Result := -1;
end;


function GetVisibleRowCount(LV: TListView): Integer;
var
  Item1, Item2: TListItem;
begin
  Result := LV.Items.Count;
  Item1 := LV.TopItem;
  if Item1 <> nil then
  begin
    Item2 := LV.GetItemAt(LV.Width div 2, LV.ClientHeight - 1);
    if Item2 <> nil then
      Result := Item2.Index - Item1.Index + 1;
  end;
end;




//=== TMasterConsumer ========================================================

constructor TMasterConsumer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, [DPA_ConsumerDisplaysList, DPA_RenderDisabledAsGrayed]);
  BeforeCreateSubSvc := LimitSubServices;
end;

destructor TMasterConsumer.Destroy;
begin
  Slave := nil;
  inherited Destroy;
end;

procedure TMasterConsumer.SetSlave(Value: TJvDataConsumer);
var
  CtxList: IJvDataContexts;
begin
  if Value <> Slave then
  begin
    ProviderChanging;
    Changing(ccrProviderSelect);
    if Slave <> nil then
      Slave.OnChanged := OldOnChanged;
    FSlave := Value;
    if Slave <> nil then
    begin
      OldOnChanged := Slave.OnChanged;
      Slave.OnChanged := SlaveChanged;
    end;
    if NeedContextFixup then
      FixupContext
    else
    begin
      if Supports(ProviderIntf, IJvDataContexts, CtxList) and (CtxList.GetCount >0 ) then
        SetContextIntf(CtxList.GetContext(0))
      else
        SetContextIntf(nil);
    end;
    ProviderChanged;
    if NeedExtensionFixups then
      FixupExtensions;
    ViewChanged(nil);
    Changed(ccrProviderSelect);
  end;
end;

procedure TMasterConsumer.LimitSubServices(Sender: TJvDataConsumer;
  var SubSvcClass: TJvDataConsumerAggregatedObjectClass);
begin
  if (Slave <> nil) and not SubSvcClass.InheritsFrom(TJvDataConsumerContext) and
    not SubSvcClass.InheritsFrom(TJvDataConsumerItemSelect) and
    not SubSvcClass.InheritsFrom(TJvCustomDataConsumerViewList) then
    SubSvcClass := nil;
end;

procedure TMasterConsumer.SlaveChanged(Sender: TJvDataConsumer;
  Reason: TJvDataConsumerChangeReason);
begin
  if Reason = ccrViewChange then
    ViewChanged(nil);
  Changed(Reason);
end;

function TMasterConsumer.ProviderIntf: IJvDataProvider;
begin
  if Slave <> nil then
    Result := Slave.ProviderIntf
  else
    Result := inherited ProviderIntf;
end;

procedure TMasterConsumer.SetProviderIntf(Value: IJvDataProvider);
begin
  if Slave <> nil then
    Slave.SetProviderIntf(Value)
  else
    inherited SetProviderIntf(Value);
end;

function TMasterConsumer.ContextIntf: IJvDataContext;
begin
  if Slave <> nil then
    Result := Slave.ContextIntf
  else
    Result := inherited ContextIntf;
end;

procedure TMasterConsumer.SetContextIntf(Value: IJvDataContext);
begin
  if Slave <> nil then
    Slave.SetContextIntf(Value)
  else
    inherited SetContextIntf(Value);
end;

function TMasterConsumer.GetInterface(const IID: TGUID; out Obj): Boolean;
begin
  Result := inherited GetInterface(IID, Obj);
  if not Result and not IsEqualGUID(IID, IJvDataConsumerItemSelect) and (Slave <> nil) then
    Result := Slave.GetInterface(IID, Obj);
end;

//=== TfmeJvProviderTreeList =================================================

constructor TfmeJvProviderTreeList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConsumerSvc := TMasterConsumer.Create(Self);
  FConsumerSvc.OnChanged := ConsumerChanged;
end;

destructor TfmeJvProviderTreeList.Destroy;
begin
  FreeAndNil(FConsumerSvc);
  inherited Destroy;
end;

function TfmeJvProviderTreeList.DoGetVirtualRoot: IJvDataItem;
begin
  Result := nil;
  if Assigned(FOnGetVirtualRoot) then
    FOnGetVirtualRoot(Self, Result);
end;

procedure TfmeJvProviderTreeList.DoItemSelect;
begin
  if Assigned(FOnItemSelect) then
    FOnItemSelect(Self);
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
    ConsumerChanged(Provider, ccrViewChange);
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
  lvProvider.Invalidate;
end;

procedure TfmeJvProviderTreeList.NotifyConsumerItemSelect;
var
  Item: IJvDataItem;
begin
  if lvProvider.Selected <> nil then
    Item := GetDataItem(lvProvider.Selected.Index)
  else
    Item := nil;
  if Provider.Slave <> nil then
    Provider.Slave.ItemSelected(Item);
  Provider.ItemSelected(Item);
end;

procedure TfmeJvProviderTreeList.UpdateSelectedItem;
begin
  NotifyConsumerItemSelect;
  DoItemSelect;
end;

procedure TfmeJvProviderTreeList.ConsumerChanged(Sender: TJvDataConsumer;
  Reason: TJvDataConsumerChangeReason);
begin
  if csDestroying in ComponentState then
    Exit;
  if UseVirtualRoot and not UsingVirtualRoot then
    GenerateVirtualRoot;
  if Reason in [ccrProviderSelect, ccrViewChange] then
    UpdateViewList;
  if (lvProvider.Items.Count > 0) and (Reason = ccrViewChange) then  
    with lvProvider do
      UpdateItems(TopItem.Index, TopItem.Index + GetVisibleRowCount(lvProvider)); 
  lvProvider.Invalidate;
end;

procedure TfmeJvProviderTreeList.GenerateVirtualRoot;
begin
  FVirtualRoot := DoGetVirtualRoot;
end;

function TfmeJvProviderTreeList.GetDataItem(Index: Integer): IJvDataItem;
begin
  Provider.Enter;
  try
    if UsingVirtualRoot and (Index = 0) then
      Result := FVirtualRoot
    else
    if (Index >= Ord(UsingVirtualRoot)) and ((Index - Ord(UsingVirtualRoot)) < GetViewList.Count) then
      Result := GetViewList.Item(Index - Ord(UsingVirtualRoot));
  finally
    Provider.Leave;
  end;
end;

function TfmeJvProviderTreeList.LocateID(ID: string): Integer;
begin
  Provider.Enter;
  try
    if UsingVirtualRoot and AnsiSameText(ID, FVirtualRoot.GetID) then
      Result := 0
    else
    begin
      Result := GetViewList.IndexOfID(ID);
      if UsingVirtualRoot and (Result >= 0) then
        Inc(Result);
    end;
  finally
    Provider.Leave;
  end;
end;

procedure TfmeJvProviderTreeList.SelectItemID(ID: string);
var
  I: Integer;
begin
  if ID = '' then
    I := -1
  else
    I := LocateID(ID);
  if I > -1 then  
  begin
    lvProvider.Items[I].Selected := True;
    lvProvider.Items[I].Focused := True;
  end 
  else
    lvProvider.Selected := nil;
  UpdateSelectedItem;
end;

function TfmeJvProviderTreeList.GetSelectedIndex: Integer;
begin
  if lvProvider.Selected = nil then
    Result := -1
  else
    Result := lvProvider.Selected.Index;
end;

procedure TfmeJvProviderTreeList.UpdateViewList;

begin 
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
  ARect := Item.DisplayRect;
  ARect.Right := Sender.ClientRect.Right;

  ACanvas := TControlCanvas.Create;
  TControlCanvas(ACanvas).Control := Sender;
  try
  ACanvas.Start; 
  DefaultDraw := False;
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
  ARect.Left := ARect.Left + (BtnWdth * 1); 
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
    DrawTextW(ACanvas.Handle, PWideChar(Item.Caption), Length(Item.Caption), ARect,
      DT_SINGLELINE + DT_LEFT + DT_END_ELLIPSIS);
    ACanvas.Stop;
  finally
    ACanvas.Free;
  end; 
end;

procedure TfmeJvProviderTreeList.lvProviderData(Sender: TObject;
  Item: TListItem);
var
  DataItem: IJvDataItem;
  ItemText: IJvDataItemText;
begin
  if (Provider.ProviderIntf = nil) or
      ((Item.Index - Ord(UsingVirtualRoot)) >= GetViewList.Count) then
    Exit;
  Provider.Enter;
  try
    if UsingVirtualRoot and (Item.Index = 0) then
    begin
      DataItem := FVirtualRoot; 
    end
    else
    begin
      DataItem := GetViewList.Item(Item.Index - Ord(UsingVirtualRoot)); 
    end;
    if DataItem <> nil then
    begin
      if Supports(DataItem, IJvDataItemText, ItemText) then
        Item.Caption := ItemText.Caption
      else
      begin
        if DataItem = FVirtualRoot then
          Item.Caption := RsDataItemRootCaption
        else
          Item.Caption := RsDataItemNoTextIntf;
      end;
    end
  finally
    Provider.Leave;
  end;
end;

procedure TfmeJvProviderTreeList.lvProviderDblClick(Sender: TObject);
begin
  Provider.Enter;
  try
    if lvProvider.Selected <> nil then
      if lvProvider.Selected.Index >= Ord(UsingVirtualRoot) then
        GetViewList.ToggleItem(lvProvider.Selected.Index - Ord(UsingVirtualRoot));
  finally
    Provider.Leave;
  end;
end;

procedure TfmeJvProviderTreeList.lvProviderMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: Integer;
  ItemLevel: Integer;
  TmpRect: TRect;
begin
  Provider.Enter;
  try
    Item := GetItemIndexAt(lvProvider, X, Y);
    if Item <> -1 then
    begin
      if UsingVirtualRoot and (Item = 0) then
        ItemLevel := 0
      else
        ItemLevel := GetViewList.ItemLevel(Item - Ord(UsingVirtualRoot)) + Ord(UsingVirtualRoot);  
      TmpRect := lvProvider.Items[Item].DisplayRect; 
      TmpRect.Right := TmpRect.Left + (Succ((TmpRect.Bottom - TmpRect.Top) + 2) * Succ(ItemLevel));
      if (X < TmpRect.Right) and (X > TmpRect.Right - ((TmpRect.Bottom - TmpRect.Top) + 2)) then
        if Item >= Ord(UsingVirtualRoot) then
          GetViewList.ToggleItem(Item - Ord(UsingVirtualRoot));
    end;
  finally
    Provider.Leave;
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
