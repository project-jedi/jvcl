{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPageMngr.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvPageManager;

interface

uses
  {$IFDEF VCL}
  Controls, Forms, StdCtrls, ExtCtrls, ActnList,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls, QForms, QStdCtrls, QExtCtrls, QActnList, QNotebook,
  {$ENDIF VisualCLX}
  SysUtils, Classes;

type
  TPageNotifyEvent = procedure(Next: Boolean) of object;
  TPageRequestEvent = procedure(CurrentPage: Integer;
    var NewPage: Integer) of object;

  TPageOwner = TNotebook;
  TPageItem = TPage;
  TJvPageProxy = class;
  TJvPageHistory = class;
  TJvPageHistoryItem = class;
  TJvPageHistoryCommand = (hcNone, hcAdd, hcBack, hcForward, hcGoto);

  TJvPageManager = class(TComponent)
  private
    FPageOwner: TPageOwner;
    FPageProxies: TList;
    FSetStartPage: Boolean;
    FDestroyHandles: Boolean;
    FButtons: array [Boolean] of TControl;
    FSaveBtnClick: array [Boolean] of TNotifyEvent;
    FChangeHelpContext: Boolean;
    FPageHistory: TJvPageHistory;
    FUseHistory: Boolean;
    FHistoryCommand: TJvPageHistoryCommand;
    FOnGetPriorPage: TPageRequestEvent;
    FOnGetNextPage: TPageRequestEvent;
    FOnCheckButtons: TNotifyEvent;
    FOnCheckProxy: TNotifyEvent;
    FOnPageChanged: TNotifyEvent;
    procedure SetPageOwner(Value: TPageOwner);
    function GetProxyIndex(const PageName: string): Integer;
    procedure AddProxy(Proxy: TJvPageProxy);
    procedure RemoveProxy(Proxy: TJvPageProxy);
    procedure DestroyProxies;
    procedure PageEnter(Page: Integer; Next: Boolean);
    procedure PageLeave(Page: Integer; Next: Boolean);
    procedure PageShow(Page: Integer; Next: Boolean);
    procedure PageHide(Page: Integer; Next: Boolean);
    procedure PageChanged;
    function GetNextEnabled: Boolean;
    function GetPriorEnabled: Boolean;
    function GetPageIndex: Integer;
    procedure SetPageIndex(Value: Integer);
    function GetPageCount: Integer;
    function GetPageName(Index: Integer): string;
    function FindFreePage: string;
    procedure SetPageProxies(Value: TList);
    function GetButton(Index: Integer): TControl;
    procedure SetButton(Index: Integer; Value: TControl);
    procedure SetDestroyHandles(Value: Boolean);
    procedure SyncBtnClick(Index: Integer; Sync: Boolean);
    procedure BtnClick(Sender: TObject);
    procedure DormantPages;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure ChangePage(Next: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CheckBtnEnabled;
    procedure Resync;
    function GetPriorPageIndex(Page: Integer): Integer; virtual;
    function GetNextPageIndex(Page: Integer): Integer; virtual;
    procedure NextPage;
    procedure PriorPage;
    procedure GotoHistoryPage(HistoryIndex: Integer);
    procedure SetPage(NewPageIndex: Integer; Next: Boolean);
    property PageNames[Index: Integer]: string read GetPageName;
    property PageCount: Integer read GetPageCount;
    property PageIndex: Integer read GetPageIndex;
    property NextEnabled: Boolean read GetNextEnabled;
    property PriorEnabled: Boolean read GetPriorEnabled;
    property PageHistory: TJvPageHistory read FPageHistory;
    property HistoryCommand: TJvPageHistoryCommand read FHistoryCommand write FHistoryCommand;
    property OnCheckProxy: TNotifyEvent read FOnCheckProxy write FOnCheckProxy; { for internal use only }
  published
    property PageOwner: TPageOwner read FPageOwner write SetPageOwner;
    property PageProxies: TList read FPageProxies write SetPageProxies;
    property NextBtn: TControl index 1 read GetButton write SetButton;
    property PriorBtn: TControl index 0 read GetButton write SetButton;
    property SetStartPage: Boolean read FSetStartPage write FSetStartPage default True;
    property DestroyHandles: Boolean read FDestroyHandles write SetDestroyHandles default False;
    property UseHistory: Boolean read FUseHistory write FUseHistory default False;
    property OnGetPriorPage: TPageRequestEvent read FOnGetPriorPage
      write FOnGetPriorPage;
    property OnGetNextPage: TPageRequestEvent read FOnGetNextPage write FOnGetNextPage;
    property OnCheckButtons: TNotifyEvent read FOnCheckButtons write FOnCheckButtons;
    property OnPageChanged: TNotifyEvent read FOnPageChanged write FOnPageChanged;
  end;

  TJvPageProxy = class(TComponent)
  private
    FPageManager: TJvPageManager;
    FPageName: string;
    FOnEnter: TPageNotifyEvent;
    FOnLeave: TPageNotifyEvent;
    FOnShow: TPageNotifyEvent;
    FOnHide: TPageNotifyEvent;
    function GetPageName: string;
    procedure SetPageName(const Value: string);
    procedure SetJvPageManager(Value: TJvPageManager);
    procedure PageEnter(Next: Boolean);
    procedure PageLeave(Next: Boolean);
    procedure PageShow(Next: Boolean);
    procedure PageHide(Next: Boolean);
  protected
    procedure SetParentComponent(Value: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
    property PageManager: TJvPageManager read FPageManager write SeTJvPageManager;
  published
    property PageName: string read GetPageName write SetPageName;
    property OnEnter: TPageNotifyEvent read FOnEnter write FOnEnter;
    property OnLeave: TPageNotifyEvent read FOnLeave write FOnLeave;
    property OnShow: TPageNotifyEvent read FOnShow write FOnShow;
    property OnHide: TPageNotifyEvent read FOnHide write FOnHide;
  end;

  TJvPageHistoryItem = class(TObject)
  public
    Index: Integer;
  end;

  TJvPageHistory = class(TList)
  private
    FCurrent: Integer;
    FHistoryCapacity: Integer;
    procedure SetCurrent(Value: Integer);
    procedure SetHistoryCapacity(Value: Integer);
    function GetPageIndex(Index: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddPageIndex(PageIndex: Integer);
    procedure DeleteHistoryItem(Index: Integer);
    procedure ResetHistory;
    property Current: Integer read FCurrent write SetCurrent;
    property HistoryCapacity: Integer read FHistoryCapacity
      write SetHistoryCapacity;
    property PageIndexes[Index: Integer]: Integer read GetPageIndex;
  end;

const
  PageNull = -1;

implementation

var
  Registered: Boolean = False;

//=== TJvPageProxy ===========================================================

constructor TJvPageProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPageName := EmptyStr;
end;

destructor TJvPageProxy.Destroy;
begin
  if FPageManager <> nil then
    FPageManager.RemoveProxy(Self);
  inherited Destroy;
end;

function TJvPageProxy.GetPageName: string;
begin
  Result := FPageName;
end;

procedure TJvPageProxy.SetPageName(const Value: string);
begin
  if (FPageManager <> nil) and (FPageManager.PageOwner <> nil) then
  begin
    if (FPageManager.PageOwner.Pages.IndexOf(Value) >= 0) then
      FPageName := Value
    else
      FPageName := '';
  end
  else
    FPageName := Value;
end;

procedure TJvPageProxy.SetJvPageManager(Value: TJvPageManager);
begin
  if FPageManager <> nil then
    FPageManager.RemoveProxy(Self);
  if Value <> nil then
    Value.AddProxy(Self);
end;

function TJvPageProxy.HasParent: Boolean;
begin
  Result := True;
end;

function TJvPageProxy.GetParentComponent: TComponent;
begin
  Result := FPageManager;
end;

procedure TJvPageProxy.SetParentComponent(Value: TComponent);
begin
  if FPageManager <> nil then
    FPageManager.RemoveProxy(Self);
  if (Value <> nil) and (Value is TJvPageManager) then
    PageManager := TJvPageManager(Value);
end;

procedure TJvPageProxy.PageEnter(Next: Boolean);
begin
  if Assigned(FOnEnter) then
    FOnEnter(Next);
end;

procedure TJvPageProxy.PageLeave(Next: Boolean);
begin
  if Assigned(FOnLeave) then
    FOnLeave(Next);
end;

procedure TJvPageProxy.PageShow(Next: Boolean);
begin
  if Assigned(FOnShow) then
    FOnShow(Next);
end;

procedure TJvPageProxy.PageHide(Next: Boolean);
begin
  if Assigned(FOnHide) then
    FOnHide(Next);
end;

//=== TJvPageManager =========================================================

constructor TJvPageManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPageProxies := TList.Create;
  FPageHistory := TJvPageHistory.Create;
  FHistoryCommand := hcAdd;
  FSetStartPage := True;
  FChangeHelpContext := True;
  FUseHistory := False;
  if not Registered then
  begin
    {$IFDEF VisualCLX}
    GroupDescendentsWith(TJvPageProxy, TControl);
    {$ENDIF VisualCLX}
    RegisterClasses([TJvPageProxy]);
    Registered := True;
  end;
end;

destructor TJvPageManager.Destroy;
begin
  DestroyProxies;
  FPageProxies.Free;
  FPageHistory.Free;
  inherited Destroy;
end;

procedure TJvPageManager.Loaded;
var
  Loading: Boolean;
begin
  Loading := csLoading in ComponentState;
  inherited Loaded;
  if not (csDesigning in ComponentState) and Loading then
  begin
    SyncBtnClick(0, True);
    SyncBtnClick(1, True);
  end;
  if FSetStartPage and not (csDesigning in ComponentState) and
    (FPageOwner <> nil) and (FPageProxies.Count > 0) then
  begin
    if (FPageProxies.Items[0] <> nil) and
      (TJvPageProxy(FPageProxies.Items[0]).PageName <> '') then
    begin
      FPageOwner.ActivePage := TJvPageProxy(FPageProxies.Items[0]).PageName;
    end;
  end;
  if DestroyHandles then
    DormantPages;
  if (FPageOwner <> nil) and (FPageHistory.Count = 0) then
    FPageHistory.AddPageIndex(FPageOwner.PageIndex);
  CheckBtnEnabled;
end;

procedure TJvPageManager.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then
  begin
    if AComponent = PageOwner then
      PageOwner := nil
    else
    if AComponent = FButtons[False] then
      FButtons[False] := nil
    else
    if AComponent = FButtons[True] then
      FButtons[True] := nil;
  end;
end;

function TJvPageManager.GetButton(Index: Integer): TControl;
begin
  Result := FButtons[Boolean(Index)];
end;

procedure TJvPageManager.SetButton(Index: Integer; Value: TControl);
begin
  if GetButton(Index) <> Value then
  begin
    if not (csLoading in ComponentState) then
      SyncBtnClick(Index, False);
    FButtons[Boolean(Index)] := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
    if not (csLoading in ComponentState) then
      SyncBtnClick(Index, True);
  end;
end;

procedure TJvPageManager.SyncBtnClick(Index: Integer; Sync: Boolean);
begin
  if (GetButton(Index) <> nil) and not (csDesigning in ComponentState) then
    if Sync then
    begin
      FSaveBtnClick[Boolean(Index)] := TButton(GetButton(Index)).OnClick;
      TButton(GetButton(Index)).OnClick := BtnClick;
    end
    else
    begin
      TButton(GetButton(Index)).OnClick := FSaveBtnClick[Boolean(Index)];
      FSaveBtnClick[Boolean(Index)] := nil;
    end;
end;

procedure TJvPageManager.BtnClick(Sender: TObject);
var
  Next: Boolean;
begin
  for Next := False to True do
    if Sender = FButtons[Next] then
    begin
      ChangePage(Next);
      if Assigned(FSaveBtnClick[Next]) then
        FSaveBtnClick[Next](Sender);
    end;
end;

procedure TJvPageManager.CheckBtnEnabled;
begin
  if not (csDesigning in ComponentState) then
  begin
    if GetButton(0) <> nil then
    begin
      if GetButton(0).Action <> nil then
        TAction(GetButton(0).Action).Enabled := PriorEnabled
      else
        GetButton(0).Enabled := PriorEnabled;
    end;
    if GetButton(1) <> nil then
    begin
      if GetButton(1).Action <> nil then
        TAction(GetButton(1).Action).Enabled := NextEnabled
      else
        GetButton(1).Enabled := NextEnabled;
    end;
    if Assigned(FOnCheckButtons) then
      FOnCheckButtons(Self);
  end;
end;

procedure TJvPageManager.GetChildren(Proc: TGetChildProc;  Root: TComponent);
var
  I: Integer;
begin
  inherited GetChildren(Proc, Root);
  for I := 0 to FPageProxies.Count - 1 do
    Proc(TJvPageProxy(FPageProxies.Items[I]));
end;

procedure TJvPageManager.SetDestroyHandles(Value: Boolean);
begin
  if Value <> FDestroyHandles then
  begin
    FDestroyHandles := Value;
    if not (csLoading in ComponentState) and FDestroyHandles then
      DormantPages;
  end;
end;

procedure TJvPageManager.SetPageOwner(Value: TPageOwner);
begin
  if FPageOwner <> Value then
  begin
    FPageOwner := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
    if not (csLoading in ComponentState) then
    begin
      Resync;
      if FDestroyHandles then
        DormantPages;
      if (FPageOwner <> nil) and (FPageHistory.Count = 0) then
      begin
        FPageHistory.AddPageIndex(FPageOwner.PageIndex);
      end;
    end;
  end;
end;

procedure TJvPageManager.SetPageProxies(Value: TList);
begin
  // without this method the ObjectInspector will not show the property  
end;

function TJvPageManager.GetProxyIndex(const PageName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FPageProxies.Count - 1 do
  begin
    if TJvPageProxy(FPageProxies.Items[I]).PageName = PageName then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

procedure TJvPageManager.Resync;
var
  I: Integer;
  Index: Integer;
  NewCount: Integer;
  NewProxy: TJvPageProxy;
begin
  if FPageOwner = nil then
    Exit;
  if PageCount > FPageProxies.Count then
  begin
    NewCount := PageCount - FPageProxies.Count;
    for I := 1 to NewCount do
    begin
      NewProxy := TJvPageProxy.Create(Owner);
      AddProxy(NewProxy);
      if Assigned(FOnCheckProxy) then
        FOnCheckProxy(NewProxy);
      {NewProxy.Name := GetUniqueName(NewProxy);}
      NewProxy.PageName := FindFreePage;
    end;
  end;
  for I := FPageProxies.Count - 1 downto 0 do
  begin
    if FPageProxies.Count > PageCount then
    begin
      if (TJvPageProxy(FPageProxies.Items[I]).PageName <> '') and
        (FPageOwner.Pages.IndexOf(TJvPageProxy(FPageProxies.Items[I]).PageName) = -1) then
        TJvPageProxy(FPageProxies.Items[I]).Free;
    end
    else
      Break;
  end;
  for I := 0 to FPageProxies.Count - 1 do
    if Assigned(FOnCheckProxy) then
      FOnCheckProxy(TObject(FPageProxies.Items[I]));
  for I := 0 to PageCount - 1 do
  begin
    Index := GetProxyIndex(PageNames[I]);
    if Index <> -1 then
      FPageProxies.Move(Index, I);
  end;
end;

procedure TJvPageManager.AddProxy(Proxy: TJvPageProxy);
begin
  FPageProxies.Add(Proxy);
  Proxy.FPageManager := Self;
end;

procedure TJvPageManager.RemoveProxy(Proxy: TJvPageProxy);
begin
  Proxy.FPageManager := nil;
  FPageProxies.Remove(Proxy);
end;

procedure TJvPageManager.DestroyProxies;
var
  Proxy: TJvPageProxy;
begin
  while FPageProxies.Count > 0 do
  begin
    Proxy := FPageProxies.Last;
    RemoveProxy(Proxy);
    Proxy.Free;
  end;
end;

function TJvPageManager.GetPageCount: Integer;
begin
  Result := 0;
  if FPageOwner <> nil then
    Result := FPageOwner.Pages.Count;
end;

function TJvPageManager.GetPageName(Index: Integer): string;
begin
  Result := '';
  if (FPageOwner <> nil) and (Index < PageCount) then
    Result := FPageOwner.Pages[Index];
end;

function TJvPageManager.FindFreePage: string;
var
  I: Integer;
begin
  Result := '';
  if PageOwner <> nil then
    for I := 0 to PageOwner.Pages.Count - 1 do
      if GetProxyIndex(PageOwner.Pages[I]) = -1 then
      begin
        Result := PageOwner.Pages[I];
        Exit;
      end;
end;

function TJvPageManager.GetPageIndex: Integer;
begin
  if PageOwner <> nil then
    Result := PageOwner.PageIndex
  else
    Result := PageNull;
end;

procedure TJvPageManager.SetPageIndex(Value: Integer);
var
  Page: TPageItem;
  OldPageIndex: Integer;
begin
  if PageOwner <> nil then
  begin
    OldPageIndex := PageOwner.PageIndex;
    PageOwner.PageIndex := Value;
    if DestroyHandles then
      DormantPages;
    if OldPageIndex <> PageOwner.PageIndex then
    begin
      if not FUseHistory then
      begin
        PageHistory.AddPageIndex(PageOwner.PageIndex);
      end
      else
      begin
        case HistoryCommand of
          hcNone:
            ;
          hcAdd:
            PageHistory.AddPageIndex(PageOwner.PageIndex);
          hcBack:
            PageHistory.Current := PageHistory.Current - 1;
          hcForward:
            PageHistory.Current := PageHistory.Current + 1;
          hcGoto:
            ;
        end;
      end;
    end;
    HistoryCommand := hcAdd;
    CheckBtnEnabled;
    { update owner form help context }
    if FChangeHelpContext and (Owner <> nil) and (Owner is TForm) and
      ((Owner as TForm).HelpContext = 0) then
    begin
      Page := TPageItem(PageOwner.Pages.Objects[PageIndex]);
      if Page <> nil then
        (Owner as TForm).HelpContext := Page.HelpContext;
    end;
  end;
end;

function TJvPageManager.GetNextEnabled: Boolean;
begin
  Result := GetNextPageIndex(PageIndex) >= 0;
end;

function TJvPageManager.GetPriorEnabled: Boolean;
begin
  Result := GetPriorPageIndex(PageIndex) >= 0;
end;

procedure TJvPageManager.NextPage;
begin
  ChangePage(True);
end;

procedure TJvPageManager.PriorPage;
begin
  ChangePage(False);
end;

procedure TJvPageManager.GotoHistoryPage(HistoryIndex: Integer);
var
  SaveCurrent: Integer;
begin
  SaveCurrent := PageHistory.Current;
  HistoryCommand := hcGoto;
  PageHistory.Current := HistoryIndex;
  try
    SetPage(PageHistory.PageIndexes[HistoryIndex], False);
  finally
    if PageOwner.PageIndex <> PageHistory.PageIndexes[HistoryIndex] then
      PageHistory.Current := SaveCurrent;
  end;
end;

procedure TJvPageManager.PageEnter(Page: Integer; Next: Boolean);
var
  ProxyIndex: Integer;
begin
  ProxyIndex := GetProxyIndex(PageOwner.Pages.Strings[Page]);
  if ProxyIndex <> PageNull then
  begin
    TJvPageProxy(FPageProxies.Items[ProxyIndex]).PageEnter(Next);
  end;
end;

procedure TJvPageManager.PageLeave(Page: Integer; Next: Boolean);
var
  ProxyIndex: Integer;
begin
  ProxyIndex := GetProxyIndex(PageOwner.Pages.Strings[Page]);
  if ProxyIndex <> PageNull then
    TJvPageProxy(FPageProxies.Items[ProxyIndex]).PageLeave(Next);
end;

procedure TJvPageManager.PageShow(Page: Integer; Next: Boolean);
var
  ProxyIndex: Integer;
begin
  ProxyIndex := GetProxyIndex(PageOwner.Pages.Strings[Page]);
  if ProxyIndex <> PageNull then
    TJvPageProxy(FPageProxies.Items[ProxyIndex]).PageShow(Next);
end;

procedure TJvPageManager.PageHide(Page: Integer; Next: Boolean);
var
  ProxyIndex: Integer;
begin
  ProxyIndex := GetProxyIndex(PageOwner.Pages.Strings[Page]);
  if ProxyIndex <> PageNull then
    TJvPageProxy(FPageProxies.Items[ProxyIndex]).PageHide(Next);
end;

procedure TJvPageManager.PageChanged;
begin
  if Assigned(FOnPageChanged) then
    FOnPageChanged(Self);
end;

function TJvPageManager.GetPriorPageIndex(Page: Integer): Integer;
begin
  if not FUseHistory then
  begin
    if Page < 1 then
      Result := PageNull
    else
      Result := Page - 1;
  end
  else
  begin
    if PageHistory.Current < 1 then
      Result := PageNull
    else
      Result := PageHistory.PageIndexes[PageHistory.Current - 1];
  end;
  if Assigned(FOnGetPriorPage) then
    FOnGetPriorPage(Page, Result);
end;

function TJvPageManager.GetNextPageIndex(Page: Integer): Integer;
begin
  if not FUseHistory then
  begin
    if Page >= PageCount - 1 then
      Result := PageNull
    else
      Result := Page + 1;
  end
  else
  begin
    if PageHistory.Current >= PageHistory.Count - 1 then
      Result := PageNull
    else
      Result := PageHistory.PageIndexes[PageHistory.Current + 1];
  end;
  if Assigned(FOnGetNextPage) then
    FOnGetNextPage(Page, Result);
end;

procedure TJvPageManager.SetPage(NewPageIndex: Integer; Next: Boolean);
var
  OldPageIndex: Integer;
begin
  if (NewPageIndex >= 0) and (NewPageIndex < PageCount) then
  begin
    OldPageIndex := PageIndex;
    PageLeave(OldPageIndex, Next);
    PageEnter(NewPageIndex, Next);
    SetPageIndex(NewPageIndex);
    if NewPageIndex = PageIndex then
    begin
      PageHide(OldPageIndex, Next);
      PageShow(NewPageIndex, Next);
      PageChanged;
    end;
  end;
end;

procedure TJvPageManager.ChangePage(Next: Boolean);
var
  NewPageIndex: Integer;
begin
  if Next then
  begin
    NewPageIndex := GetNextPageIndex(PageIndex);
    HistoryCommand := hcForward;
  end
  else
  begin
    NewPageIndex := GetPriorPageIndex(PageIndex);
    HistoryCommand := hcBack;
  end;
  SetPage(NewPageIndex, Next);
end;

type
  TJvHack = class(TWinControl);

procedure TJvPageManager.DormantPages;
var
  I: Integer;
begin
  if Assigned(FPageOwner) then
    with PageOwner do
    begin
      for I := 0 to Pages.Count - 1 do
        if PageIndex <> I then
          TJvHack(Pages.Objects[I]).DestroyHandle;
    end;
end;

//=== TJvPageHistory =========================================================

constructor TJvPageHistory.Create;
begin
  inherited Create;
  FCurrent := -1;
  FHistoryCapacity := 10;
end;

destructor TJvPageHistory.Destroy;
begin
  ResetHistory;
  inherited Destroy;
end;

procedure TJvPageHistory.SetCurrent(Value: Integer);
begin
  if Value < 0 then
    Value := -1;
  if Value > Count - 1 then
    Value := Count - 1;
  FCurrent := Value;
end;

procedure TJvPageHistory.SetHistoryCapacity(Value: Integer);
var
  I: Integer;
begin
  if Value < FHistoryCapacity then
  begin
    for I := 0 to Count - Value do
      DeleteHistoryItem(0);
  end;
  FHistoryCapacity := Value;
end;

function TJvPageHistory.GetPageIndex(Index: Integer): Integer;
begin
  Result := TJvPageHistoryItem(Items[Index]).Index;
end;

procedure TJvPageHistory.AddPageIndex(PageIndex: Integer);
var
  I: Integer;
  Item: TJvPageHistoryItem;
begin
  for I := Count - 1 downto Current + 1 do
    DeleteHistoryItem(I);
  for I := 0 to Count - HistoryCapacity do
    DeleteHistoryItem(0);
  if Count < HistoryCapacity then
  begin
    Item := TJvPageHistoryItem.Create;
    Item.Index := PageIndex;
    Add(Item);
  end;
  Current := Count - 1;
end;

procedure TJvPageHistory.DeleteHistoryItem(Index: Integer);
var
  Item: TJvPageHistoryItem;
begin
  if (Index >= 0) and (Index < Count) then
  begin
    Item := TJvPageHistoryItem(Items[Index]);
    Delete(Index);
    Item.Free;
    if Current > Count - 1 then
      Current := Count - 1;
  end;
end;

procedure TJvPageHistory.ResetHistory;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    DeleteHistoryItem(I);
end;

end.

