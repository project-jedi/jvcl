{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxPageMngr.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://JVCL.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JvxPageMngr;

interface

uses Classes, Controls, ExtCtrls{, JvxComponent};

type
  TPageNotifyEvent = procedure(Next: Boolean) of object;
  TPageRequestEvent = procedure(CurrentPage: Integer;
    var NewPage: Integer) of object;

  TPageOwner = TNotebook;
  TPageItem = TPage;
  TJvxPageProxy = class;
  TJvxPageHistory = class;
  TJvxPageHistoryItem = class;
  TJvxPageHistoryCommand = (hcNone, hcAdd, hcBack, hcForward, hcGoto);

  TJvxPageManager = class(TComponent)
  private
    FPageOwner: TPageOwner;
    FPageProxies: TList;
    FSetStartPage: Boolean;
    FDestroyHandles: Boolean;
    FButtons: array [Boolean] of TControl;
    FSaveBtnClick: array [Boolean] of TNotifyEvent;
    FChangeHelpContext: Boolean;
    FPageHistory: TJvxPageHistory;
    FUseHistory: Boolean;
    FHistoryCommand: TJvxPageHistoryCommand;
    FOnGetPriorPage: TPageRequestEvent;
    FOnGetNextPage: TPageRequestEvent;
    FOnCheckButtons: TNotifyEvent;
    FOnCheckProxy: TNotifyEvent;
    FOnPageChanged: TNotifyEvent;
    procedure SetPageOwner(Value: TPageOwner);
    function GetProxyIndex(const PageName: string): Integer;
    procedure AddProxy(Proxy: TJvxPageProxy);
    procedure RemoveProxy(Proxy: TJvxPageProxy);
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
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
{$IFDEF WIN32}
    procedure GetChildren(Proc: TGetChildProc {$IFDEF Delphi3_Up}; Root: TComponent {$ENDIF}); override;
{$ELSE}
    procedure WriteComponents(Writer: TWriter); override;
{$ENDIF WIN32}
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
    property PageHistory: TJvxPageHistory read FPageHistory;
    property HistoryCommand: TJvxPageHistoryCommand read FHistoryCommand
      write FHistoryCommand;
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
    property OnGetNextPage: TPageRequestEvent read FOnGetNextPage
      write FOnGetNextPage;
    property OnCheckButtons: TNotifyEvent read FOnCheckButtons
      write FOnCheckButtons;
    property OnPageChanged: TNotifyEvent read FOnPageChanged write FOnPageChanged;
  end;

  TJvxPageProxy = class(TComponent)
  private
    FPageManager: TJvxPageManager;
    FPageName: String;
    FOnEnter: TPageNotifyEvent;
    FOnLeave: TPageNotifyEvent;
    FOnShow: TPageNotifyEvent;
    FOnHide: TPageNotifyEvent;
    function GetPageName: string;
    procedure SetPageName(const Value: string);
    procedure SeTJvxPageManager(Value: TJvxPageManager);
    procedure PageEnter(Next: Boolean);
    procedure PageLeave(Next: Boolean);
    procedure PageShow(Next: Boolean);
    procedure PageHide(Next: Boolean);
  protected
{$IFDEF WIN32}
    procedure SetParentComponent(Value: TComponent); override;
{$ELSE}
    procedure ReadState(Reader: TReader); override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HasParent: Boolean; override;
{$IFDEF WIN32}
    function GetParentComponent: TComponent; override;
{$ENDIF}
    property PageManager: TJvxPageManager read FPageManager write SeTJvxPageManager;
  published
    property PageName: string read GetPageName write SetPageName;
    property OnEnter: TPageNotifyEvent read FOnEnter write FOnEnter;
    property OnLeave: TPageNotifyEvent read FOnLeave write FOnLeave;
    property OnShow: TPageNotifyEvent read FOnShow write FOnShow;
    property OnHide: TPageNotifyEvent read FOnHide write FOnHide;
  end;

  TJvxPageHistoryItem = class(TObject)
  public
    Index: Integer;
  end;

  TJvxPageHistory = class(TList)
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
  pageNull = -1;

implementation

uses SysUtils, Forms, StdCtrls {$IFDEF Delphi4_Up}, ActnList {$ENDIF};

const
  Registered: Boolean = False;

{ TJvxPageProxy }

constructor TJvxPageProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPageName := EmptyStr;
end;

destructor TJvxPageProxy.Destroy;
begin
  if FPageManager <> nil then FPageManager.RemoveProxy(Self);
  //if (FPageName <> nil) and (FPageName^ <> '') then Dispose(FPageName);
  inherited Destroy;
end;

function TJvxPageProxy.GetPageName: string;
begin
  Result := FPageName;
end;

procedure TJvxPageProxy.SetPageName(const Value: string);
begin
  if (FPageManager <> nil) and (FPageManager.PageOwner <> nil) then
  begin
    if (FPageManager.PageOwner.Pages.IndexOf(Value) >= 0) then FPageName := Value else FPageName := '';
  end
  else FPageName := Value;
end;

procedure TJvxPageProxy.SeTJvxPageManager(Value: TJvxPageManager);
begin
  if FPageManager <> nil then FPageManager.RemoveProxy(Self);
  if Value <> nil then Value.AddProxy(Self);
end;

function TJvxPageProxy.HasParent: Boolean;
begin
  Result := True;
end;

{$IFDEF WIN32}

function TJvxPageProxy.GetParentComponent: TComponent;
begin
  Result := FPageManager;
end;

procedure TJvxPageProxy.SetParentComponent(Value: TComponent);
begin
  if FPageManager <> nil then FPageManager.RemoveProxy(Self);
  if (Value <> nil) and (Value is TJvxPageManager) then
    PageManager := TJvxPageManager(Value);
end;

{$ELSE}

procedure TJvxPageProxy.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TJvxPageManager then begin
    PageManager := TJvxPageManager(Reader.Parent);
  end;
end;

{$ENDIF WIN32}

procedure TJvxPageProxy.PageEnter(Next: Boolean);
begin
  if Assigned(FOnEnter) then FOnEnter(Next);
end;

procedure TJvxPageProxy.PageLeave(Next: Boolean);
begin
  if Assigned(FOnLeave) then FOnLeave(Next);
end;

procedure TJvxPageProxy.PageShow(Next: Boolean);
begin
  if Assigned(FOnShow) then FOnShow(Next);
end;

procedure TJvxPageProxy.PageHide(Next: Boolean);
begin
  if Assigned(FOnHide) then FOnHide(Next);
end;

{ TJvxPageManager }

constructor TJvxPageManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPageProxies := TList.Create;
  FPageHistory := TJvxPageHistory.Create;
  FHistoryCommand := hcAdd;
  FSetStartPage := True;
  FChangeHelpContext := True;
  FUseHistory := False;
  if not Registered then begin
    RegisterClasses([TJvxPageProxy]);
    Registered := True;
  end;
end;

destructor TJvxPageManager.Destroy;
begin
  DestroyProxies;
  FPageProxies.Free;
  FPageHistory.Free;
  inherited Destroy;
end;

procedure TJvxPageManager.Loaded;
var
  Loading: Boolean;
begin
  Loading := csLoading in ComponentState;
  inherited Loaded;
  if not (csDesigning in ComponentState) and Loading then begin
    SyncBtnClick(0, True);
    SyncBtnClick(1, True);
  end;
  if FSetStartPage and not (csDesigning in ComponentState) and
    (FPageOwner <> nil) and (FPageProxies.Count > 0) then
  begin
    if (FPageProxies.Items[0] <> nil) and
      (TJvxPageProxy(FPageProxies.Items[0]).PageName <> '') then
    begin
      FPageOwner.ActivePage := TJvxPageProxy(FPageProxies.Items[0]).PageName;
    end;
  end;
  if DestroyHandles then DormantPages;
  if (FPageOwner <> nil) and (FPageHistory.Count = 0) then begin
    FPageHistory.AddPageIndex(FPageOwner.PageIndex);
  end;
  CheckBtnEnabled;
end;

procedure TJvxPageManager.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then begin
    if AComponent = PageOwner then PageOwner := nil
    else if AComponent = FButtons[False] then FButtons[False] := nil
    else if AComponent = FButtons[True] then FButtons[True] := nil;
  end;
end;

function TJvxPageManager.GetButton(Index: Integer): TControl;
begin
  Result := FButtons[Boolean(Index)];
end;

procedure TJvxPageManager.SetButton(Index: Integer; Value: TControl);
begin
  if GetButton(Index) <> Value then begin
    if not (csLoading in ComponentState) then  SyncBtnClick(Index, False);
    FButtons[Boolean(Index)] := Value;
{$IFDEF WIN32}
    if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
    if not (csLoading in ComponentState) then  SyncBtnClick(Index, True);
  end;
end;

procedure TJvxPageManager.SyncBtnClick(Index: Integer; Sync: Boolean);
begin
  if (GetButton(Index) <> nil) and not (csDesigning in ComponentState) then
    if Sync then begin
      FSaveBtnClick[Boolean(Index)] := TButton(GetButton(Index)).OnClick;
      TButton(GetButton(Index)).OnClick := BtnClick;
    end
    else begin
      TButton(GetButton(Index)).OnClick := FSaveBtnClick[Boolean(Index)];
      FSaveBtnClick[Boolean(Index)] := nil;
    end;
end;

procedure TJvxPageManager.BtnClick(Sender: TObject);
var
  Next: Boolean;
begin
  for Next := False to True do
    if Sender = FButtons[Next] then begin
      ChangePage(Next);
      if Assigned(FSaveBtnClick[Next]) then FSaveBtnClick[Next](Sender);
    end;
end;

procedure TJvxPageManager.CheckBtnEnabled;
begin
  if not (csDesigning in ComponentState) then begin
{$IFDEF Delphi4_Up}
    if GetButton(0) <> nil then begin
      if GetButton(0).Action <> nil then
        TAction(GetButton(0).Action).Enabled := PriorEnabled
      else
        GetButton(0).Enabled := PriorEnabled;
    end;
    if GetButton(1) <> nil then begin
      if GetButton(1).Action <> nil then
        TAction(GetButton(1).Action).Enabled := NextEnabled
      else
        GetButton(1).Enabled := NextEnabled;
    end;
{$ELSE}
    if GetButton(0) <> nil then GetButton(0).Enabled := PriorEnabled;
    if GetButton(1) <> nil then GetButton(1).Enabled := NextEnabled;
{$ENDIF}
    if Assigned(FOnCheckButtons) then FOnCheckButtons(Self);
  end;
end;

{$IFDEF WIN32}
procedure TJvxPageManager.GetChildren(Proc: TGetChildProc {$IFDEF Delphi3_Up};
  Root: TComponent {$ENDIF});
var
  I: Integer;
begin
  inherited GetChildren(Proc{$IFDEF Delphi3_Up}, Root {$ENDIF});
  for I := 0 to FPageProxies.Count - 1 do begin
    Proc(TJvxPageProxy(FPageProxies.Items[I]));
  end;
end;
{$ELSE}
procedure TJvxPageManager.WriteComponents(Writer: TWriter);
var
  I: Integer;
  Proxy: TJvxPageProxy;
begin
  inherited WriteComponents(Writer);
  for I := 0 to FPageProxies.Count - 1 do begin
    Proxy := FPageProxies.Items[I];
    if Proxy.Owner = Writer.Root then Writer.WriteComponent(Proxy);
  end;
end;
{$ENDIF WIN32}

procedure TJvxPageManager.SetDestroyHandles(Value: Boolean);
begin
  if Value <> FDestroyHandles then begin
    FDestroyHandles := Value;
    if not (csLoading in ComponentState) and FDestroyHandles then
      DormantPages;
  end;
end;

procedure TJvxPageManager.SetPageOwner(Value: TPageOwner);
begin
  if FPageOwner <> Value then begin
    FPageOwner := Value;
{$IFDEF WIN32}
    if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
    if not (csLoading in ComponentState) then begin
      Resync;
      if FDestroyHandles then DormantPages;
      if (FPageOwner <> nil) and (FPageHistory.Count = 0) then begin
        FPageHistory.AddPageIndex(FPageOwner.PageIndex);
      end;
    end;
  end;
end;

procedure TJvxPageManager.SetPageProxies(Value: TList);
begin
end;

function TJvxPageManager.GetProxyIndex(const PageName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FPageProxies.Count - 1 do begin
    if TJvxPageProxy(FPageProxies.Items[I]).PageName = PageName then begin
      Result := I;
      Exit;
    end;
  end;
end;

procedure TJvxPageManager.Resync;
var
  I: Integer;
  Index: Integer;
  NewCount: Integer;
  NewProxy: TJvxPageProxy;
begin
  if FPageOwner = nil then Exit;
  if PageCount > FPageProxies.Count then begin
    NewCount := PageCount - FPageProxies.Count;
    for I := 1 to NewCount do begin
      NewProxy := TJvxPageProxy.Create(Owner);
      AddProxy(NewProxy);
      if Assigned(FOnCheckProxy) then FOnCheckProxy(NewProxy);
      {NewProxy.Name := GetUniqueName(NewProxy);}
      NewProxy.PageName := FindFreePage;
    end;
  end;
  for I := FPageProxies.Count - 1 downto 0 do begin
    if FPageProxies.Count > PageCount then begin
      if (TJvxPageProxy(FPageProxies.Items[I]).PageName <> '') and
        (FPageOwner.Pages.IndexOf(TJvxPageProxy(FPageProxies.Items[I]).PageName) = -1) then
        TJvxPageProxy(FPageProxies.Items[I]).Free;
    end
    else Break;
  end;
  for I := 0 to FPageProxies.Count - 1 do
    if Assigned(FOnCheckProxy) then
      FOnCheckProxy(TObject(FPageProxies.Items[I]));
  for I := 0 to PageCount - 1 do begin
    Index := GetProxyIndex(PageNames[I]);
    if Index <> -1 then begin
      FPageProxies.Move(Index, I);
    end;
  end;
end;

procedure TJvxPageManager.AddProxy(Proxy: TJvxPageProxy);
begin
  FPageProxies.Add(Proxy);
  Proxy.FPageManager := Self;
end;

procedure TJvxPageManager.RemoveProxy(Proxy: TJvxPageProxy);
begin
  Proxy.FPageManager := nil;
  FPageProxies.Remove(Proxy);
end;

procedure TJvxPageManager.DestroyProxies;
var
  Proxy: TJvxPageProxy;
begin
  while FPageProxies.Count > 0 do begin
    Proxy := FPageProxies.Last;
    RemoveProxy(Proxy);
    Proxy.Free;
  end;
end;

function TJvxPageManager.GetPageCount: Integer;
begin
  Result := 0;
  if FPageOwner <> nil then begin
    Result := FPageOwner.Pages.Count;
  end;
end;

function TJvxPageManager.GetPageName(Index: Integer): string;
begin
  Result := '';
  if (FPageOwner <> nil) and (Index < PageCount) then begin
    Result := FPageOwner.Pages[Index];
  end;
end;

function TJvxPageManager.FindFreePage: string;
var
  I: Integer;
begin
  Result := '';
  if PageOwner <> nil then
    for I := 0 to PageOwner.Pages.Count - 1 do
      if GetProxyIndex(PageOwner.Pages[I]) = -1 then begin
        Result := PageOwner.Pages[I];
        Exit;
      end;
end;

function TJvxPageManager.GetPageIndex: Integer;
begin
  if PageOwner <> nil then Result := PageOwner.PageIndex
  else Result := pageNull;
end;

procedure TJvxPageManager.SetPageIndex(Value: Integer);
var
  Page: TPageItem;
  OldPageIndex: Integer;
begin
  if PageOwner <> nil then begin
    OldPageIndex := PageOwner.PageIndex;
    PageOwner.PageIndex := Value;
    if DestroyHandles then DormantPages;
    if OldPageIndex <> PageOwner.PageIndex then begin
      if not FUseHistory then begin
        PageHistory.AddPageIndex(PageOwner.PageIndex);
      end
      else begin
        case HistoryCommand of
          hcNone: ;
          hcAdd: PageHistory.AddPageIndex(PageOwner.PageIndex);
          hcBack: PageHistory.Current := PageHistory.Current - 1;
          hcForward: PageHistory.Current := PageHistory.Current + 1;
          hcGoto: ;
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
      if Page <> nil then (Owner as TForm).HelpContext := Page.HelpContext;
    end;
  end;
end;

function TJvxPageManager.GetNextEnabled: Boolean;
begin
  Result := GetNextPageIndex(PageIndex) >= 0;
end;

function TJvxPageManager.GetPriorEnabled: Boolean;
begin
  Result := GetPriorPageIndex(PageIndex) >= 0;
end;

procedure TJvxPageManager.NextPage;
begin
  ChangePage(True);
end;

procedure TJvxPageManager.PriorPage;
begin
  ChangePage(False);
end;

procedure TJvxPageManager.GotoHistoryPage(HistoryIndex: Integer);
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

procedure TJvxPageManager.PageEnter(Page: Integer; Next: Boolean);
var
  ProxyIndex: Integer;
begin
  ProxyIndex := GetProxyIndex(PageOwner.Pages.Strings[Page]);
  if ProxyIndex <> pageNull then begin
    TJvxPageProxy(FPageProxies.Items[ProxyIndex]).PageEnter(Next);
  end;
end;

procedure TJvxPageManager.PageLeave(Page: Integer; Next: Boolean);
var
  ProxyIndex: Integer;
begin
  ProxyIndex := GetProxyIndex(PageOwner.Pages.Strings[Page]);
  if ProxyIndex <> pageNull then begin
    TJvxPageProxy(FPageProxies.Items[ProxyIndex]).PageLeave(Next);
  end;
end;

procedure TJvxPageManager.PageShow(Page: Integer; Next: Boolean);
var
  ProxyIndex: Integer;
begin
  ProxyIndex := GetProxyIndex(PageOwner.Pages.Strings[Page]);
  if ProxyIndex <> pageNull then begin
    TJvxPageProxy(FPageProxies.Items[ProxyIndex]).PageShow(Next);
  end;
end;

procedure TJvxPageManager.PageHide(Page: Integer; Next: Boolean);
var
  ProxyIndex: Integer;
begin
  ProxyIndex := GetProxyIndex(PageOwner.Pages.Strings[Page]);
  if ProxyIndex <> pageNull then begin
    TJvxPageProxy(FPageProxies.Items[ProxyIndex]).PageHide(Next);
  end;
end;

procedure TJvxPageManager.PageChanged;
begin
  if Assigned(FOnPageChanged) then FOnPageChanged(Self);
end;

function TJvxPageManager.GetPriorPageIndex(Page: Integer): Integer;
begin
  if not FUseHistory then begin
    if Page < 1 then
      Result := pageNull
    else
      Result := Page - 1;
    end
  else begin
    if PageHistory.Current < 1 then
      Result := pageNull
    else
      Result := PageHistory.PageIndexes[PageHistory.Current - 1];
  end;
  if Assigned(FOnGetPriorPage) then FOnGetPriorPage(Page, Result);
end;

function TJvxPageManager.GetNextPageIndex(Page: Integer): Integer;
begin
  if not FUseHistory then begin
    if Page >= PageCount - 1 then
      Result := pageNull
    else
      Result := Page + 1;
    end
  else begin
    if PageHistory.Current >= PageHistory.Count - 1 then
      Result := pageNull
    else
      Result := PageHistory.PageIndexes[PageHistory.Current + 1];
  end;
  if Assigned(FOnGetNextPage) then FOnGetNextPage(Page, Result);
end;

procedure TJvxPageManager.SetPage(NewPageIndex: Integer; Next: Boolean);
var
  OldPageIndex: Integer;
begin
  if (NewPageIndex >=0) and (NewPageIndex < PageCount) then begin
    OldPageIndex := PageIndex;
    PageLeave(OldPageIndex, Next);
    PageEnter(NewPageIndex, Next);
    SetPageIndex(NewPageIndex);
    if NewPageIndex = PageIndex then begin
      PageHide(OldPageIndex, Next);
      PageShow(NewPageIndex, Next);
      PageChanged;
    end;
  end;
end;

procedure TJvxPageManager.ChangePage(Next: Boolean);
var
  NewPageIndex: Integer;
begin
  if Next then begin
    NewPageIndex := GetNextPageIndex(PageIndex);
    HistoryCommand := hcForward;
  end
  else begin
    NewPageIndex := GetPriorPageIndex(PageIndex);
    HistoryCommand := hcBack;
  end;
  SetPage(NewPageIndex, Next);
end;

type
  TJvxHack = class(TWinControl);

procedure TJvxPageManager.DormantPages;
var
  I: Integer;
begin
  if Assigned(FPageOwner) then
    with PageOwner do begin
      for I := 0 to Pages.Count - 1 do
        if PageIndex <> I then
          TJvxHack(Pages.Objects[I]).DestroyHandle;
    end;
end;

{ TJvxPageHistory }

constructor TJvxPageHistory.Create;
begin
  inherited Create;
  FCurrent := -1;
  FHistoryCapacity := 10;
end;

destructor TJvxPageHistory.Destroy;
begin
  ResetHistory;
  inherited Destroy;
end;

procedure TJvxPageHistory.SetCurrent(Value: Integer);
begin
  if Value < 0 then Value := -1;
  if Value > Count - 1 then Value := Count - 1;
  FCurrent := Value;
end;

procedure TJvxPageHistory.SetHistoryCapacity(Value: Integer);
var
  I: Integer;
begin
  if Value < FHistoryCapacity then begin
    for I := 0 to Count - Value do begin
      DeleteHistoryItem(0);
    end;
  end;
  FHistoryCapacity := Value;
end;

function TJvxPageHistory.GetPageIndex(Index: Integer): Integer;
begin
  Result := TJvxPageHistoryItem(Items[Index]).Index;
end;

procedure TJvxPageHistory.AddPageIndex(PageIndex: Integer);
var
  I: Integer;
  Item: TJvxPageHistoryItem;
begin
  for I := Count - 1 downto Current + 1 do begin
    DeleteHistoryItem(I);
  end;
  for I := 0 to Count - HistoryCapacity do begin
    DeleteHistoryItem(0);
  end;
  if Count < HistoryCapacity then begin
    Item := TJvxPageHistoryItem.Create;
    Item.Index := PageIndex;
    Add(Item);
  end;
  Current := Count - 1;
end;

procedure TJvxPageHistory.DeleteHistoryItem(Index: Integer);
var
  Item: TJvxPageHistoryItem;
begin
  if (Index >= 0) and (Index < Count) then begin
    Item := TJvxPageHistoryItem(Items[Index]);
    Delete(Index);
    Item.Free;
    if Current > Count - 1 then Current := Count - 1;
  end;
end;

procedure TJvxPageHistory.ResetHistory;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
    DeleteHistoryItem(I);
  end;
end;

end.
