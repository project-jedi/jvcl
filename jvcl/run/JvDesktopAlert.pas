{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDesktopAlert.PAS, released on 2004-03-23.

The Initial Developer of the Original Code is Peter Thornqvist <peter3 at peter3 dot com>
Portions created by Sébastien Buysse are Copyright (C) 2004 Peter Thornqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2004-03-23

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
* When stacking several alert forms on top of each other, they don't always move
  down in the list as they should when another alert form is closed. This is controlled
  by the code in RegisterStackedForm and UnregisterStackedForm.

-----------------------------------------------------------------------------}
unit JvDesktopAlert;

interface
uses
  Windows, Classes, SysUtils, Controls, Graphics, Forms, Menus, ImgList,
  JvComponent;

type
  TJvDesktopAlertList = class;
  TJvDesktopAlertChangePersistent = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
  protected
    procedure Change;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvDesktopAlertColors = class(TJvDesktopAlertChangePersistent)
  private
    FWindowFrom: TColor;
    FCaptionTo: TColor;
    FWindowTo: TColor;
    FFrame: TColor;
    FCaptionFrom: TColor;

    procedure SetCaptionFrom(const Value: TColor);
    procedure SetCaptionTo(const Value: TColor);
    procedure SetFrame(const Value: TColor);
    procedure SetWindowFrom(const Value: TColor);
    procedure SetWindowTo(const Value: TColor);
  public
    constructor Create;
  published
    property Frame: TColor read FFrame write SetFrame default $808080;
    property WindowFrom: TColor read FWindowFrom write SetWindowFrom default $D8DCE0;
    property WindowTo: TColor read FWindowTo write SetWindowTo default $A8B4B8;
    property CaptionFrom: TColor read FCaptionFrom write SetCaptionFrom default $B0B4B0;
    property CaptionTo: TColor read FCaptionTo write SetCaptionTo default $808480;
  end;

  TJvDesktopAlertPosition = (dapTopLeft, dapTopRight, dapBottomLeft, dapBottomRight, dapCustom);

  TJvDesktopAlertLocation = class(TJvDesktopAlertChangePersistent)
  private
    FTop: integer;
    FLeft: integer;
    FPosition: TJvDesktopAlertPosition;
    FAlwaysResetPosition: boolean;
    procedure SetTop(const Value: integer);
    procedure SetLeft(const Value: integer);
    procedure SetPosition(const Value: TJvDesktopAlertPosition);
  public
    constructor Create;
  published
    property Position: TJvDesktopAlertPosition read FPosition write SetPosition default dapBottomRight;
    property Top: integer read FTop write SetTop;
    property Left: integer read FLeft write SetLeft;
    property AlwaysResetPosition: boolean read FAlwaysResetPosition write FAlwaysResetPosition default True;
  end;

  TJvDesktopAlertItem = class(TCollectionItem)
  private
    FImageIndex: integer;
    FOnClick: TNotifyEvent;
    FTag: integer;
  public
    procedure Assign(Source: TPersistent); override;

  published
    property ImageIndex: integer read FImageIndex write FImageIndex;
    property Tag: integer read FTag write FTag;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TJvDesktopAlertButtons = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TJvDesktopAlertItem;
    procedure SetItem(Index: integer; const Value: TJvDesktopAlertItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TJvDesktopAlertItem;

    property Items[Index: integer]: TJvDesktopAlertItem read GetItem write SetItem; default;
    procedure Assign(Source: TPersistent); override;
  end;

  TJvDesktopAlertOption = (daoCanClick, daoCanMove, daoCanMoveAnywhere, daoCanClose, daoCanFade);
  TJvDesktopAlertOptions = set of TJvDesktopAlertOption;

  TJvDesktopAlert = class(TJvComponent)
  private
    FList: TJvDesktopAlertList;
    FMaxAlphaBlendValue: byte;
    FFadeInTime: integer;
    FFadeOutTime: integer;
    FWaitTime: integer;
    FMessageText: string;
    FHeaderText: string;
    FImages: TCustomImageList;
    FFont: TFont;
    FHeaderFont: TFont;
    FButtons: TJvDesktopAlertButtons;
    FColors: TJvDesktopAlertColors;
    FLocation: TJvDesktopAlertLocation;
    FOptions: TJvDesktopAlertOptions;
    FOnClose: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMessageClick: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FData: TObject;
    FImage: TPicture;
    FPopupMenu: TPopupMenu;
    FDropDownMenu: TPopUpMenu;
    function GetList: TJvDesktopAlertList;
    procedure SetButtons(const Value: TJvDesktopAlertButtons);
    procedure SetColors(const Value: TJvDesktopAlertColors);
    procedure SetDropDownMenu(const Value: TPopUpMenu);
    procedure SetFont(const Value: TFont);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetImage(const Value: TPicture);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure InternalOnShow(Sender: TObject);
    procedure InternalOnClose(Sender: TObject; var Action: TCloseAction);
    procedure InternalMouseEnter(Sender: TObject);
    procedure InternalMouseLeave(Sender: TObject);
    procedure InternalMessageClick(Sender: TObject);
    procedure InternalOnMove(Sender: TObject);
    function GetAlertList: TJvDesktopAlertList;
    procedure SetAlertList(const Value: TJvDesktopAlertList);
  protected
    FFormButtons: array of TControl;
    FDesktopForm: TForm;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Showing: boolean;
    procedure Execute;
    property Data: TObject read FData write FData;
  published
    property AlertList: TJvDesktopAlertList read GetAlertList write SetAlertList;
    property HeaderText: string read FHeaderText write FHeaderText;
    property MessageText: string read FMessageText write FMessageText;

    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property Font: TFont read FFont write SetFont;
    property Options: TJvDesktopAlertOptions read FOptions write FOptions default [daoCanClick..daoCanFade];
    property Colors: TJvDesktopAlertColors read FColors write SetColors;
    property Buttons: TJvDesktopAlertButtons read FButtons write SetButtons;
    property Location: TJvDesktopAlertLocation read FLocation write FLocation;
    property Image: TPicture read FImage write SetImage;
    property Images: TCustomImageList read FImages write SetImages;
    property DropDownMenu: TPopUpMenu read FDropDownMenu write SetDropDownMenu;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;

    property FadeInTime: integer read FFadeInTime write FFadeInTime default 25;
    property FadeOutTime: integer read FFadeOutTime write FFadeOutTime default 50;
    property WaitTime: integer read FWaitTime write FWaitTime default 1400;
    property MaxAlphaBlendValue: byte read FMaxAlphaBlendValue write FMaxAlphaBlendValue default 255;

    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMessageClick: TNotifyEvent read FOnMessageClick write FOnMessageClick;
  end;

  TJvDesktopAlertList = class(TJvComponent)
  private
    FItems: TThreadList;
    FPosition: TJvDesktopAlertPosition;
    function GetCount: integer;
    function GetItems(Index: integer): TForm;
    procedure SetPosition(const Value: TJvDesktopAlertPosition);
  protected
    procedure Sort;
    procedure Optimize;
  public
    procedure Add(AForm: TForm);
    procedure Remove(AForm: TForm);
    property Items[Index: integer]: TForm read GetItems;
    property Count: integer read GetCount;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // all forms must have the same position property
    property Position: TJvDesktopAlertPosition read FPosition write SetPosition default dapBottomRight;
  end;

implementation
uses
  JvDeskTopAlertForm;

var
  FGlobalList: TJvDesktopAlertList;

function GlobalList: TJvDesktopAlertList;
begin
  if FGlobalList = nil then
    FGlobalList := TJvDesktopAlertList.Create(nil);
  Result := FGlobalList;
end;

{ TJvDesktopAlertChangePersistent }

procedure TJvDesktopAlertChangePersistent.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ TJvDesktopAlertColors }

constructor TJvDesktopAlertColors.Create;
begin
  inherited Create;
  FFrame := $808080;
  FWindowFrom := $D8DCE0;
  FWindowTo := $A8B4B8;
  FCaptionFrom := $B0B4B0;
  FCaptionTo := $808480;
end;

procedure TJvDesktopAlertColors.SetCaptionFrom(const Value: TColor);
begin
  if FCaptionFrom <> Value then
  begin
    FCaptionFrom := Value;
    Change;
  end;
end;

procedure TJvDesktopAlertColors.SetCaptionTo(const Value: TColor);
begin
  if FCaptionTo <> Value then
  begin
    FCaptionTo := Value;
    Change;
  end;
end;

procedure TJvDesktopAlertColors.SetFrame(const Value: TColor);
begin
  if FFrame <> Value then
  begin
    FFrame := Value;
    Change;
  end;
end;

procedure TJvDesktopAlertColors.SetWindowFrom(const Value: TColor);
begin
  if FWindowFrom <> Value then
  begin
    FWindowFrom := Value;
    Change;
  end;
end;

procedure TJvDesktopAlertColors.SetWindowTo(const Value: TColor);
begin
  if FWindowTo <> Value then
  begin
    FWindowTo := Value;
    Change;
  end;
end;

{ TJvDesktopAlertLocation }

constructor TJvDesktopAlertLocation.Create;
begin
  inherited Create;
  FPosition := dapBottomRight;
  FAlwaysResetPosition := true;
end;

procedure TJvDesktopAlertLocation.SetLeft(const Value: integer);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    Change;
  end;
end;

procedure TJvDesktopAlertLocation.SetPosition(
  const Value: TJvDesktopAlertPosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Change;
  end;
end;

procedure TJvDesktopAlertLocation.SetTop(const Value: integer);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    Change;
  end;
end;

{ TJvDesktopAlertItem }

procedure TJvDesktopAlertItem.Assign(Source: TPersistent);
begin
  if (Source is TJvDesktopAlertItem) and (Source <> Self) then
  begin
    ImageIndex := TJvDesktopAlertItem(Source).ImageIndex;
    OnClick := TJvDesktopAlertItem(Source).OnClick;
    Tag := TJvDesktopAlertItem(Source).Tag;
    Exit;
  end;
  inherited;
end;

{ TJvDesktopAlertButtons }

function TJvDesktopAlertButtons.Add: TJvDesktopAlertItem;
begin
  Result := TJvDesktopAlertItem(inherited Add);
end;

procedure TJvDesktopAlertButtons.Assign(Source: TPersistent);
var
  i: integer;
begin
  if (Source is TJvDesktopAlertButtons) and (Source <> Self) then
  begin
    Clear;
    for i := 0 to TJvDesktopAlertButtons(Source).Count - 1 do
      Add.Assign(TJvDesktopAlertButtons(Source)[i]);
    Exit;
  end;
  inherited;
end;

constructor TJvDesktopAlertButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvDesktopAlertItem);
end;

function TJvDesktopAlertButtons.GetItem(Index: integer): TJvDesktopAlertItem;
begin
  Result := TJvDesktopAlertItem(inherited Items[Index]);
end;

procedure TJvDesktopAlertButtons.SetItem(Index: integer; const Value: TJvDesktopAlertItem);
begin
  inherited Items[Index] := Value;
end;

{ TJvDesktopAlert }

constructor TJvDesktopAlert.Create(AOwner: TComponent);
begin
  inherited;
  FFont := TFont.Create;
  FFont.Assign(Screen.IconFont);
  FHeaderFont := TFont.Create;
  FHeaderFont.Assign(FFOnt);
  FHeaderFont.Style := [fsBold];
  FColors := TJvDesktopAlertColors.Create;
  FButtons := TJvDesktopAlertButtons.Create(Self);
  FLocation := TJvDesktopAlertLocation.Create;
  FImage := TPicture.Create;
  FDesktopForm := TJvFormDesktopAlert.CreateNew(self, 1);
//  FDesktopForm.FreeNotification(Self);
  FOptions := [daoCanClick..daoCanFade];
  FFadeInTime := 25;
  FFadeOutTime := 50;
  FWaitTime := 1400;
  FMaxAlphaBlendValue := 255;
end;

destructor TJvDesktopAlert.Destroy;
begin
  if (FDesktopForm <> nil) and FDesktopForm.Showing then
    FDesktopForm.Close;
  FHeaderFont.Free;
  FFont.Free;
  FColors.Free;
  FButtons.Free;
  FLocation.Free;
  FImage.Free;
  GetList.Remove(FDesktopForm);
  FDesktopForm.Release;
  inherited Destroy;
end;

procedure TJvDesktopAlert.Execute;
var
  ARect: TRect;
  i, X, Y: integer;
  AForm: TJvFormDesktopAlert;
begin
  AForm := TJvFormDesktopAlert(FDesktopForm); // save us some typecasting
  Assert(AForm <> nil);
  if AForm.Showing then AForm.Close;
  SystemParametersInfo(SPI_GETWORKAREA, 0, @ARect, 0);
  case Location.Position of
    dapTopLeft:
      begin
        AForm.Top := ARect.Top;
        AForm.Left := ARect.Left;
      end;
    dapTopRight:
      begin
        AForm.Top := ARect.Top;
        AForm.Left := ARect.Right - AForm.Width;
      end;
    dapBottomLeft:
      begin
        AForm.Top := ARect.Bottom - AForm.Height;
        AForm.Left := ARect.Left;
      end;
    dapBottomRight:
      begin
        AForm.Top := ARect.Bottom - AForm.Height;
        AForm.Left := ARect.Right - AForm.Width;
      end;
    dapCustom:
      begin
        AForm.Top := Location.Top;
        AForm.Left := Location.Left;
      end;
  end;
  AForm.OnShow := InternalOnShow;
  AForm.OnClose := InternalOnClose;
  AForm.OnMouseEnter := InternalMouseEnter;
  AForm.OnMouseLeave := InternalMouseLeave;
  AForm.OnUserMove := InternalOnMove;
  AForm.lblText.OnClick := InternalMessageClick;
  AForm.Moveable := (daoCanMove in Options);
  AForm.MoveAnywhere := (daoCanMoveAnywhere in Options);
  AForm.Closeable := (daoCanClose in Options);

  AForm.ClickableMessage := daoCanClick in Options;
  if daoCanFade in Options then
  begin
    AForm.FadeInTime := FadeInTime;
    AForm.FadeOutTime := FadeOutTime;
  end
  else
  begin
    AForm.FadeInTime := 0;
    AForm.FadeOutTime := 0;
  end;

  AForm.WaitTime := WaitTime;
  AForm.MaxAlphaBlendValue := MaxAlphaBlendValue;

  AForm.tbDropDown.DropDownMenu := DropDownMenu;
  AForm.imIcon.Picture := Image;

  AForm.Font := Font;
  AForm.lblHeader.Caption := HeaderText;
  AForm.lblHeader.Font := HeaderFont;
  AForm.lblText.Caption := MessageText;
  AForm.WindowColorFrom := Colors.WindowFrom;
  AForm.WindowColorTo := Colors.WindowTo;
  AForm.CaptionColorFrom := Colors.CaptionFrom;
  AForm.CaptionColorTo := Colors.CaptionTo;
  AForm.FrameColor := Colors.Frame;

  for i := 0 to Length(FFormButtons) - 1 do
    FFormButtons[i].Free;
  SetLength(FFormButtons, Buttons.Count);
  X := 2;
  Y := AForm.Height - 23;
  for i := 0 to Length(FFormButtons) - 1 do
  begin
    FFormButtons[i] := TJvDesktopAlertButton.Create(AForm);
    with TJvDesktopAlertButton(FFormButtons[i]) do
    begin
      SetBounds(X, Y, 21, 21);
      ToolType := abtImage;
      Images := Self.Images;
      ImageIndex := Buttons[i].ImageIndex;
      Tag := Buttons[i].Tag;
      OnClick := Buttons[i].OnClick;
      Parent := AForm;
      Inc(X, 22);
    end;
  end;
  AForm.Show;
  GetList.Position := Location.Position;
  GetList.Add(AForm);
end;

function TJvDesktopAlert.GetAlertList: TJvDesktopAlertList;
begin
  if FList = GlobalList then
    Result := nil
  else
    Result := FList;
end;

function TJvDesktopAlert.GetList: TJvDesktopAlertList;
begin
  if FList = nil then
    Result := GlobalList
  else
    Result := FList;
end;

procedure TJvDesktopAlert.InternalMessageClick(Sender: TObject);
begin
  if Assigned(FOnMessageClick) and (daoCanClick in Options) then
    FOnMessageClick(Self)
end;

procedure TJvDesktopAlert.InternalMouseEnter(Sender: TObject);
begin
//  UnregisterStackedForm(TJvFormDesktopAlert(FDesktopForm), Location.Position);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvDesktopAlert.InternalMouseLeave(Sender: TObject);
begin
//  RegisterStackedForm(TJvFormDesktopAlert(FDesktopForm), Location.Position);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvDesktopAlert.InternalOnClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Location.Position = dapCustom then
  begin
    Location.Top := FDesktopForm.Top;
    Location.Left := FDesktopForm.Left;
  end;
  if Assigned(FOnClose) then
    FOnClose(Self);
  FDesktopForm.Visible := false;
  GetList.Remove(FDesktopForm);
  GetList.Optimize;
end;

procedure TJvDesktopAlert.InternalOnMove(Sender: TObject);
begin
  if not Location.AlwaysResetPosition and (Location.Position <> dapCustom) then
  begin
    GetList.Remove(FDesktopForm);
    Location.Position := dapCustom;
  end;
end;

procedure TJvDesktopAlert.InternalOnShow(Sender: TObject);
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

procedure TJvDesktopAlert.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = Images then
      Images := nil;
    if AComponent = DropDownMenu then
      DropDownMenu := nil;
    if AComponent = PopupMenu then
      PopupMenu := nil;
    if AComponent = FList then
      AlertList := nil;
  end;
end;

procedure TJvDesktopAlert.SetAlertList(const Value: TJvDesktopAlertList);
begin
  if FList <> Value then
  begin
    FList := Value;
    if FList <> nil then
    begin
      Location.Position := FList.Position;
      FList.FreeNotification(Self);
    end;
  end;
end;

procedure TJvDesktopAlert.SetButtons(const Value: TJvDesktopAlertButtons);
begin
  FButtons.Assign(Value);
end;

procedure TJvDesktopAlert.SetColors(const Value: TJvDesktopAlertColors);
begin
//  FColors := Value;
end;

procedure TJvDesktopAlert.SetDropDownMenu(const Value: TPopUpMenu);
begin
  if FDropDownMenu <> Value then
  begin
    FDropDownMenu := Value;
    if FDropDownMenu <> nil then
      FDropDownMenu.FreeNotification(Self);
  end;
end;

procedure TJvDesktopAlert.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TJvDesktopAlert.SetHeaderFont(const Value: TFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TJvDesktopAlert.SetImage(const Value: TPicture);
begin
  FImage.Assign(Value);
end;

procedure TJvDesktopAlert.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    if FImages <> nil then
      FImages.FreeNotification(Self);
  end;
end;

procedure TJvDesktopAlert.SetPopupMenu(const Value: TPopupMenu);
begin
  if FPopupMenu <> Value then
  begin
    FPopupMenu := Value;
    if FPopupMenu <> nil then
      FPopupMenu.FreeNotification(Self);
  end;
end;

function TJvDesktopAlert.Showing: boolean;
begin
  Result := (FDesktopForm <> nil) and FDesktopForm.Showing;
end;

{ TJvDesktopAlertList }

procedure TJvDesktopAlertList.Add(AForm: TForm);
var
  DoOptimize: boolean;
begin
  if Position = dapCustom then Exit; // raise?
  DoOptimize := False;
  with FItems.LockList do
  try
    if (AForm is TJvFormDesktopAlert) and (IndexOf(AForm) < 0) then
    begin
//      case Position of
//        dapBottomRight, dapBottomLeft:
//          TJvFormDesktopAlert(AForm).Top := -(Count * AForm.Height);
//        dapTopRight, dapTopLeft:
//          TJvFormDesktopAlert(AForm).Top := Count * AForm.Height;
//      end;
      FItems.Remove(AForm);
      FItems.Add(AForm);
      DoOptimize := True;
    end;
  finally
    FItems.UnlockList;
  end;
  if DoOptimize then
    Optimize;
end;

constructor TJvDesktopAlertList.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TThreadList.Create;
  FPosition := dapBottomRight;
end;

destructor TJvDesktopAlertList.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TJvDesktopAlertList.GetCount: integer;
begin
  with FItems.LockList do
  try
    Result := Count;
  finally
    FItems.UnlockList;
  end;
end;

function TJvDesktopAlertList.GetItems(Index: integer): TForm;
begin
  with FItems.LockList do
  try
    Result := TForm(Items[Index]);
  finally
    FItems.UnlockList;
  end;
end;

procedure TJvDesktopAlertList.Optimize;
var
  i, Y: integer;
  R: TRect;
  function Showing(AForm: TForm): boolean;
  begin
    Result := True;
  end;

begin
//  Sort;
  SystemParametersInfo(SPI_GETWORKAREA, 0, @R, 0);
  case Position of
    dapBottomRight, dapBottomLeft:
      begin
        Y := R.Bottom;
        i := 0;
        while i < Count do
        begin
          if Showing(Items[i]) then
          begin
            Dec(Y, Items[i].Height);
            TJvFormDesktopAlert(Items[i]).SetNewTop(Y);
            if Position = dapBottomRight then
              TJvFormDesktopAlert(Items[i]).SetNewLeft(R.Right - Items[i].Width)
            else
              TJvFormDesktopAlert(Items[i]).SetNewLeft(R.Left);
          end;
          Inc(i);
        end;
      end;

    dapTopRight, dapTopLeft:
      begin
        Y := R.Top;
        i := 0;
        while i < Count do
        begin
          if Showing(Items[i]) then
          begin
            TJvFormDesktopAlert(Items[i]).SetNewTop(Y);
            if Position = dapTopRight then
              TJvFormDesktopAlert(Items[i]).SetNewLeft(R.Right - Items[i].Width)
            else
              TJvFormDesktopAlert(Items[i]).SetNewLeft(R.Left);
            Inc(Y, Items[i].Height);
          end;
          Inc(i);
        end;
      end;
  end;
end;

procedure TJvDesktopAlertList.Remove(AForm: TForm);
begin
  if Position = dapCustom then Exit; // raise?
  if (AForm <> nil) and (AForm is TJvFormDesktopAlert) then
  begin
    FItems.Remove(AForm);
    Optimize;
  end;
end;

procedure TJvDesktopAlertList.SetPosition(const Value: TJvDesktopAlertPosition);
begin
  if FPosition <> Value then
  begin
    FItems.Clear;
    FPosition := Value;
  end;
end;

function BottomSort(Item1, Item2: Pointer): integer;
begin
  Result := TForm(Item1).Top - TForm(Item2).Top;
end;

function TopSort(Item1, Item2: Pointer): integer;
begin
  Result := TForm(Item2).Top - TForm(Item1).Top;
end;

procedure TJvDesktopAlertList.Sort;
begin
  with FItems.LockList do
  try
    case Position of
      dapBottomRight, dapBottomLeft:
        Sort(BottomSort);
      dapTopLeft, dapTopRight:
        Sort(TopSort);
    end;
  finally
    FItems.UnlockList;
  end;
end;

initialization

finalization
  FGlobalList.Free;

end.

