{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDesktopAlert.PAS, released on 2004-03-23.

The Initial Developer of the Original Code is Peter Thornqvist <peter3 at sourceforge dot net>
Portions created by Peter Thornqvist are Copyright (C) 2004 Peter Thornqvist.
All Rights Reserved.

Contributor(s):
Hans-Eric Grönlund (stack logic)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvDesktopAlert;

interface

uses
  Windows, Classes, Controls, Graphics, Forms, Menus, ImgList,
  JvComponent, JvBaseDlg, JvDesktopAlertForm;

const
  JvDefaultFrameColor = TColor($00943000);
  JvDefaultWindowFromColor = TColor($00FFE7CE);
  JvDefaultWindowToColor = TColor($00E7A67B);
  JvDefaultCaptionFromColor = TColor($00D68652);
  JvDefaultCaptionToColor = TColor($00944110);

type
  TJvDesktopAlertStack = class;
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
    procedure Assign(Source: TPersistent); override;
  published
    property Frame: TColor read FFrame write SetFrame default JvDefaultFrameColor;
    property WindowFrom: TColor read FWindowFrom write SetWindowFrom default JvDefaultWindowFromColor;
    property WindowTo: TColor read FWindowTo write SetWindowTo default JvDefaultWindowToColor;
    property CaptionFrom: TColor read FCaptionFrom write SetCaptionFrom default JvDefaultCaptionFromColor;
    property CaptionTo: TColor read FCaptionTo write SetCaptionTo default JvDefaultCaptionToColor;
  end;

  TJvDesktopAlertPosition =
   (dapTopLeft, dapTopRight, dapBottomLeft, dapBottomRight, dapCustom,
    dapDesktopCenter, dapMainFormCenter, dapOwnerFormCenter, dapActiveFormCenter);

  TJvDesktopAlertLocation = class(TJvDesktopAlertChangePersistent)
  private
    FTop: Integer;
    FLeft: Integer;
    FPosition: TJvDesktopAlertPosition;
    FAlwaysResetPosition: Boolean;
    FHeight: Integer;
    FWidth: Integer;
    procedure SetTop(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetPosition(const Value: TJvDesktopAlertPosition);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  public
    constructor Create;
  published
    property Position: TJvDesktopAlertPosition read FPosition write SetPosition default dapBottomRight;
    property Top: Integer read FTop write SetTop;
    property Left: Integer read FLeft write SetLeft;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property AlwaysResetPosition: Boolean read FAlwaysResetPosition write FAlwaysResetPosition default True;
  end;

  TJvDesktopAlertButtonItem = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FOnClick: TNotifyEvent;
    FTag: Integer;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property Tag: Integer read FTag write FTag;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TJvDesktopAlertButtons = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TJvDesktopAlertButtonItem;
    procedure SetItem(Index: Integer; const Value: TJvDesktopAlertButtonItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TJvDesktopAlertButtonItem;
    property Items[Index: Integer]: TJvDesktopAlertButtonItem read GetItem write SetItem; default;
    procedure Assign(Source: TPersistent); override;
  end;

  TJvDesktopAlertOption = (daoCanClick, daoCanMove, daoCanMoveAnywhere, daoCanClose, daoCanFade);
  TJvDesktopAlertOptions = set of TJvDesktopAlertOption;

  TJvDesktopAlert = class(TJvCommonDialogP)
  private
    FStacker: TJvDesktopAlertStack;
    FImages: TCustomImageList;
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
    FAutoFocus: Boolean;
    function GetStacker: TJvDesktopAlertStack;
    procedure SetButtons(const Value: TJvDesktopAlertButtons);
    procedure SetColors(const Value: TJvDesktopAlertColors);
    procedure SetDropDownMenu(const Value: TPopupMenu);
    procedure SetFont(const Value: TFont);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetImage(const Value: TPicture);
    procedure SetImages(const Value: TCustomImageList);
    procedure SeTPopupMenu(const Value: TPopupMenu);
    procedure InternalOnShow(Sender: TObject);
    procedure InternalOnClose(Sender: TObject; var Action: TCloseAction);
    procedure InternalMouseEnter(Sender: TObject);
    procedure InternalMouseLeave(Sender: TObject);
    procedure InternalMessageClick(Sender: TObject);
    procedure InternalOnMove(Sender: TObject);
    function GetAlertStack: TJvDesktopAlertStack;
    procedure SetAlertStack(const Value: TJvDesktopAlertStack);
    function GetFadeInTime: Integer;
    function GetFadeOutTime: Integer;
    function GetFont: TFont;
    function GetHeaderFont: TFont;
    function GetImage: TPicture;
    function GetAlphaBlendValue: Byte;
    function GetWaitTime: Integer;
    procedure SetFadeInTime(const Value: Integer);
    procedure SetFadeOutTime(const Value: Integer);
    procedure SetAlphaBlendValue(const Value: Byte);
    function GetDropDownMenu: TPopupMenu;
    function GetHeaderText: string;
    function GetMessageText: string;
    function GeTPopupMenu: TPopupMenu;
    procedure SetHeaderText(const Value: string);
    procedure SetLocation(const Value: TJvDesktopAlertLocation);
    procedure SetMessageText(const Value: string);
    procedure DoLocationChange(Sender: TObject);
    function GetParentFont: Boolean;
    function GetShowHint: Boolean;
    function GetHint: string;
    procedure SetHint(const Value: string);
    procedure SetParentFont(const Value: Boolean);
    procedure SetShowHint(const Value: Boolean);
    procedure SetWaitTime(const Value: Integer);
    procedure SetOptions(const Value: TJvDesktopAlertOptions);
    function GetCloseButtonClick: TNotifyEvent;
    procedure SetCloseButtonClick(const Value: TNotifyEvent);
  protected
    FFormButtons: array of TControl;
    FDesktopForm: TJvFormDesktopAlert;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Showing: Boolean;
    procedure Close(Immediate: Boolean);
    procedure Execute; override;
    property Data: TObject read FData write FData;
  published
    property AlertStack: TJvDesktopAlertStack read GetAlertStack write SetAlertStack;
    property AutoFocus: Boolean read FAutoFocus write FAutoFocus default False;
    property HeaderText: string read GetHeaderText write SetHeaderText;
    property MessageText: string read GetMessageText write SetMessageText;

    property HeaderFont: TFont read GetHeaderFont write SetHeaderFont;
    property Hint: string read GetHint write SetHint;
    property ShowHint: Boolean read GetShowHint write SetShowHint;
    property Font: TFont read GetFont write SetFont;
    property ParentFont: Boolean read GetParentFont write SetParentFont;
    property Options: TJvDesktopAlertOptions read FOptions write SetOptions default [daoCanClick..daoCanFade];
    property Colors: TJvDesktopAlertColors read FColors write SetColors;
    property Buttons: TJvDesktopAlertButtons read FButtons write SetButtons;
    property Location: TJvDesktopAlertLocation read FLocation write SetLocation;
    property Image: TPicture read GetImage write SetImage;
    property Images: TCustomImageList read FImages write SetImages;
    property DropDownMenu: TPopupMenu read GetDropDownMenu write SetDropDownMenu;
    property PopupMenu: TPopupMenu read GeTPopupMenu write SeTPopupMenu;

    property FadeInTime: Integer read GetFadeInTime write SetFadeInTime default 25;
    property FadeOutTime: Integer read GetFadeOutTime write SetFadeOutTime default 50;
    property WaitTime: Integer read GetWaitTime write SetWaitTime default 1400;
    property AlphaBlendValue: Byte read GetAlphaBlendValue write SetAlphaBlendValue default 255;

    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnCloseButtonClick: TNotifyEvent read GetCloseButtonClick write SetCloseButtonClick;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMessageClick: TNotifyEvent read FOnMessageClick write FOnMessageClick;
  end;

  TJvDesktopAlertStack = class(TJvComponent)
  private
    FItems: TList;
    FPosition: TJvDesktopAlertPosition;
    function GetCount: Integer;
    function GetItems(Index: Integer): TJvFormDesktopAlert;
    procedure SetPosition(const Value: TJvDesktopAlertPosition);
  protected
    procedure UpdatePositions;
  public
    procedure Add(AForm: TForm);
    procedure Remove(AForm: TForm);

    property Items[Index: Integer]: TJvFormDesktopAlert read GetItems;
    property Count: Integer read GetCount;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // all forms must have the same position property
    property Position: TJvDesktopAlertPosition read FPosition write SetPosition default dapBottomRight;
  end;

implementation

uses
  JvJVCLUtils, JvTypes, JvFinalize;

const
  sUnitName = 'JvDesktopAlert';

var
  FGlobalStacker: TJvDesktopAlertStack = nil;

function GlobalStacker: TJvDesktopAlertStack;
begin
  if FGlobalStacker = nil then
  begin
    FGlobalStacker := TJvDesktopAlertStack.Create(nil);
    AddFinalizeObjectNil(sUnitName, TObject(FGlobalStacker));
  end;
  Result := FGlobalStacker;
end;

//=== { TJvDesktopAlertChangePersistent } ====================================

procedure TJvDesktopAlertChangePersistent.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//=== { TJvDesktopAlertColors } ==============================================

constructor TJvDesktopAlertColors.Create;
begin
  inherited Create;
  FFrame := JvDefaultFrameColor;
  FWindowFrom := JvDefaultWindowFromColor;
  FWindowTo := JvDefaultWindowToColor;
  FCaptionFrom := JvDefaultCaptionFromColor;
  FCaptionTo := JvDefaultCaptionToColor;
end;

procedure TJvDesktopAlertColors.Assign(Source: TPersistent);
begin
  if Source is TJvDesktopAlertColors then
  begin
    if Source <> Self then
    begin
      FFrame := TJvDesktopAlertColors(Source).Frame;
      FWindowFrom := TJvDesktopAlertColors(Source).WindowFrom;
      FWindowTo := TJvDesktopAlertColors(Source).WindowTo;
      FCaptionFrom := TJvDesktopAlertColors(Source).CaptionFrom;
      FCaptionTo := TJvDesktopAlertColors(Source).CaptionTo;
      Change;
    end;
  end
  else
    inherited Assign(Source);
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

//=== { TJvDesktopAlertLocation } ============================================

constructor TJvDesktopAlertLocation.Create;
begin
  inherited Create;
  FPosition := dapBottomRight;
  FAlwaysResetPosition := True;
end;

procedure TJvDesktopAlertLocation.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Change;
  end;
end;

procedure TJvDesktopAlertLocation.SetLeft(const Value: Integer);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    Change;
  end;
end;

procedure TJvDesktopAlertLocation.SetPosition(const Value: TJvDesktopAlertPosition);
begin
//  if FPosition <> Value then
  begin
    FPosition := Value;
    Change;
  end;
end;

procedure TJvDesktopAlertLocation.SetTop(const Value: Integer);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    Change;
  end;
end;

procedure TJvDesktopAlertLocation.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Change;
  end;
end;

//=== { TJvDesktopAlertButtonItem } ==========================================

procedure TJvDesktopAlertButtonItem.Assign(Source: TPersistent);
begin
  if Source is TJvDesktopAlertButtonItem then
  begin
    if Source <> Self then
    begin
      ImageIndex := TJvDesktopAlertButtonItem(Source).ImageIndex;
      OnClick := TJvDesktopAlertButtonItem(Source).OnClick;
      Tag := TJvDesktopAlertButtonItem(Source).Tag;
    end;
  end
  else
    inherited Assign(Source);
end;

//=== { TJvDesktopAlertButtons } =============================================

constructor TJvDesktopAlertButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvDesktopAlertButtonItem);
end;

function TJvDesktopAlertButtons.Add: TJvDesktopAlertButtonItem;
begin
  Result := TJvDesktopAlertButtonItem(inherited Add);
end;

procedure TJvDesktopAlertButtons.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TJvDesktopAlertButtons then
  begin
    if Source <> Self then
    begin
      Clear;
      for I := 0 to TJvDesktopAlertButtons(Source).Count - 1 do
        Add.Assign(TJvDesktopAlertButtons(Source)[I]);
    end;
  end
  else
    inherited Assign(Source);
end;

function TJvDesktopAlertButtons.GetItem(Index: Integer): TJvDesktopAlertButtonItem;
begin
  Result := TJvDesktopAlertButtonItem(inherited Items[Index]);
end;

procedure TJvDesktopAlertButtons.SetItem(Index: Integer; const Value: TJvDesktopAlertButtonItem);
begin
  inherited Items[Index] := Value;
end;

//=== { TJvDesktopAlert } ====================================================

constructor TJvDesktopAlert.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColors := TJvDesktopAlertColors.Create;
  FButtons := TJvDesktopAlertButtons.Create(Self);
  FLocation := TJvDesktopAlertLocation.Create;
  FLocation.OnChange := DoLocationChange;
  FDesktopForm := TJvFormDesktopAlert.CreateNew(Self, 1);
//  FDesktopForm.FreeNotification(Self);
  FOptions := [daoCanClick..daoCanFade];
  FadeInTime := 25;
  FadeOutTime := 50;
  WaitTime := 1400;
  AlphaBlendValue := 255;
end;

destructor TJvDesktopAlert.Destroy;
begin
  if (FDesktopForm <> nil) and FDesktopForm.Showing then
    FDesktopForm.Close;
  FColors.Free;
  FButtons.Free;
  FLocation.Free;
  GetStacker.Remove(FDesktopForm);
  FDesktopForm.Release;
  inherited Destroy;
end;

procedure TJvDesktopAlert.Close(Immediate: Boolean);
begin
  if Showing then
  begin
    if Immediate then
      FDesktopForm.Close
    else
      FDesktopForm.FadeClose;
  end;
end;

procedure TJvDesktopAlert.DoLocationChange(Sender: TObject);
begin
  if GetStacker.Position <> Location.Position then
  begin
    if GetStacker = FGlobalStacker then
      GetStacker.Position := Location.Position
    else
      Location.Position := GetStacker.Position;
  end;
end;

procedure TJvDesktopAlert.Execute;
var
  ARect: TRect;
  I, X, Y: Integer;
  FActiveWindow: HWND;

  procedure CenterForm(AForm: TForm; ARect: TRect);
  begin
    AForm.Top := ARect.Top + ((ARect.Bottom - ARect.Top) - AForm.Height) div 2;
    AForm.Left := ARect.Left + ((ARect.Right - ARect.Left) - AForm.Width) div 2;
  end;

begin
  Assert(FDesktopForm <> nil);
  if FDesktopForm.Visible then
    FDesktopForm.Close;

  ARect := ScreenWorkArea;
  if Location.Width <> 0 then
    FDesktopForm.Width := Location.Width
  else
    FDesktopForm.Width := cDefaultAlertFormWidth;
  if Location.Height <> 0 then
    FDesktopForm.Height := Location.Height
  else
    FDesktopForm.Height := cDefaultAlertFormHeight;
  case Location.Position of
    dapTopLeft:
      begin
        FDesktopForm.Top := ARect.Top;
        FDesktopForm.Left := ARect.Left;
      end;
    dapTopRight:
      begin
        FDesktopForm.Top := ARect.Top;
        FDesktopForm.Left := ARect.Right - FDesktopForm.Width;
      end;
    dapBottomLeft:
      begin
        FDesktopForm.Top := ARect.Bottom - FDesktopForm.Height;
        FDesktopForm.Left := ARect.Left;
      end;
    dapBottomRight:
      begin
        FDesktopForm.Top := ARect.Bottom - FDesktopForm.Height;
        FDesktopForm.Left := ARect.Right - FDesktopForm.Width;
      end;
    dapCustom:
      begin
        FDesktopForm.Top := Location.Top;
        FDesktopForm.Left := Location.Left;
      end;
    dapDesktopCenter, dapMainFormCenter, dapOwnerFormCenter, dapActiveFormCenter:
      begin
        CenterForm(FDesktopForm, ARect);
        if (Location.Position = dapActiveFormCenter) and (Screen.ActiveForm <> nil) then
          CenterForm(FDesktopForm, Screen.ActiveForm.BoundsRect)
        else
        if (Location.Position = dapMainFormCenter) and (Application <> nil) and (Application.MainForm <> nil) then
          CenterForm(FDesktopForm, Application.MainForm.BoundsRect)
        else
        if (Location.Position = dapOwnerFormCenter) and (Owner is TCustomForm) then
          CenterForm(FDesktopForm, TCustomForm(Owner).BoundsRect);
      end;
  end;

  FDesktopForm.OnShow := InternalOnShow;
  FDesktopForm.OnClose := InternalOnClose;
  FDesktopForm.OnMouseEnter := InternalMouseEnter;
  FDesktopForm.OnMouseLeave := InternalMouseLeave;
  FDesktopForm.OnUserMove := InternalOnMove;
  FDesktopForm.lblText.OnClick := InternalMessageClick;
  FDesktopForm.Moveable := (daoCanMove in Options);
  FDesktopForm.MoveAnywhere := (daoCanMoveAnywhere in Options);
  FDesktopForm.Closeable := (daoCanClose in Options);
  FDesktopForm.ClickableMessage := daoCanClick in Options;
  if not Assigned(FDesktopForm.tbClose.OnClick) then
    FDesktopForm.tbClose.OnClick := FDesktopForm.acCloseExecute;

  if not (daoCanFade in Options) then
  begin
    FDesktopForm.FadeInTime := 0;
    FDesktopForm.FadeOutTime := 0;
  end
  else
  begin
    FDesktopForm.FadeInTime := FadeInTime;
    FDesktopForm.FadeOutTime := FadeOutTime;
  end;

  FDesktopForm.WaitTime := WaitTime;
  FDesktopForm.MaxAlphaBlendValue := AlphaBlendValue;

  FDesktopForm.tbDropDown.DropDownMenu := DropDownMenu;
  FDesktopForm.imIcon.Picture := Image;

  FDesktopForm.Font := Font;
  FDesktopForm.lblHeader.Caption := HeaderText;
  FDesktopForm.lblHeader.Font := HeaderFont;
  FDesktopForm.lblText.Caption := MessageText;
  FDesktopForm.WindowColorFrom := Colors.WindowFrom;
  FDesktopForm.WindowColorTo := Colors.WindowTo;
  FDesktopForm.CaptionColorFrom := Colors.CaptionFrom;
  FDesktopForm.CaptionColorTo := Colors.CaptionTo;
  FDesktopForm.FrameColor := Colors.Frame;

  for I := 0 to Length(FFormButtons) - 1 do
    FFormButtons[I].Free;
  SetLength(FFormButtons, Buttons.Count);
  X := 2;
  Y := FDesktopForm.Height - 23;
  for I := 0 to Length(FFormButtons) - 1 do
  begin
    FFormButtons[I] := TJvDesktopAlertButton.Create(FDesktopForm);
    with TJvDesktopAlertButton(FFormButtons[I]) do
    begin
      SetBounds(X, Y, 21, 21);
      ToolType := abtImage;
      Images := Self.Images;
      ImageIndex := Buttons[I].ImageIndex;
      Tag := Buttons[I].Tag;
      OnClick := Buttons[I].OnClick;
      Parent := FDesktopForm;
      Inc(X, 22);
    end;
  end;
  Location.Position := GetStacker.Position;
  if not AutoFocus then
    FActiveWindow := GetActiveWindow
  else
    FActiveWindow := NullHandle;
  FDesktopForm.Show;
  if not AutoFocus and (FActiveWindow <> NullHandle) then
    SetActiveWindow(FActiveWindow);
  GetStacker.Add(FDesktopForm);
end;

function TJvDesktopAlert.GetAlertStack: TJvDesktopAlertStack;
begin
  if FStacker = GlobalStacker then
    Result := nil
  else
    Result := FStacker;
end;

function TJvDesktopAlert.GetDropDownMenu: TPopupMenu;
begin
  Result := FDesktopForm.tbDropDown.DropDownMenu;
end;

function TJvDesktopAlert.GetFadeInTime: Integer;
begin
  Result := FDesktopForm.FadeInTime;
end;

function TJvDesktopAlert.GetFadeOutTime: Integer;
begin
  Result := FDesktopForm.FadeOutTime;
end;

function TJvDesktopAlert.GetFont: TFont;
begin
  Result := FDesktopForm.lblText.Font;
end;

function TJvDesktopAlert.GetHeaderFont: TFont;
begin
  Result := FDesktopForm.lblHeader.Font;
end;

function TJvDesktopAlert.GetHeaderText: string;
begin
  Result := FDesktopForm.lblHeader.Caption;
end;

function TJvDesktopAlert.GetImage: TPicture;
begin
  Result := FDesktopForm.imIcon.Picture;
end;

function TJvDesktopAlert.GetAlphaBlendValue: Byte;
begin
  Result := FDesktopForm.MaxAlphaBlendValue;
end;

function TJvDesktopAlert.GetMessageText: string;
begin
  Result := FDesktopForm.lblText.Caption;
end;

function TJvDesktopAlert.GetParentFont: Boolean;
begin
  Result := FDesktopForm.ParentFont;
end;

function TJvDesktopAlert.GeTPopupMenu: TPopupMenu;
begin
  Result := FDesktopForm.PopupMenu;
end;

function TJvDesktopAlert.GetShowHint: Boolean;
begin
  Result := FDesktopForm.ShowHint;
end;

function TJvDesktopAlert.GetStacker: TJvDesktopAlertStack;
begin
  if FStacker = nil then
    Result := GlobalStacker
  else
    Result := FStacker;
end;

function TJvDesktopAlert.GetWaitTime: Integer;
begin
  Result := FDesktopForm.WaitTime;
end;

function TJvDesktopAlert.GetHint: string;
begin
  Result := FDesktopForm.Hint;
end;

procedure TJvDesktopAlert.InternalMessageClick(Sender: TObject);
begin
  if Assigned(FOnMessageClick) and (daoCanClick in Options) then
    FOnMessageClick(Self)
end;

procedure TJvDesktopAlert.InternalMouseEnter(Sender: TObject);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvDesktopAlert.InternalMouseLeave(Sender: TObject);
begin
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
  GetStacker.Remove(FDesktopForm);
end;

procedure TJvDesktopAlert.InternalOnMove(Sender: TObject);
begin
  if not (csDesigning in ComponentState) and not Location.AlwaysResetPosition and
    (Location.Position <> dapCustom) then
  begin
    GetStacker.Remove(FDesktopForm);
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
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FStacker then
      AlertStack := nil;
end;

procedure TJvDesktopAlert.SetAlertStack(const Value: TJvDesktopAlertStack);
begin
  if FStacker <> Value then
  begin
    FStacker := Value;
    if FStacker <> nil then
    begin
      Location.Position := FStacker.Position;
      FStacker.FreeNotification(Self);
    end;
  end;
end;

procedure TJvDesktopAlert.SetButtons(const Value: TJvDesktopAlertButtons);
begin
  FButtons.Assign(Value);
end;

procedure TJvDesktopAlert.SetColors(const Value: TJvDesktopAlertColors);
begin
  FColors.Assign(Value);
end;

procedure TJvDesktopAlert.SetDropDownMenu(const Value: TPopupMenu);
begin
  FDesktopForm.tbDropDown.DropDownMenu := Value;
end;

procedure TJvDesktopAlert.SetFadeInTime(const Value: Integer);
begin
  FDesktopForm.FadeInTime := Value;
end;

procedure TJvDesktopAlert.SetFadeOutTime(const Value: Integer);
begin
  FDesktopForm.FadeOutTime := Value;
end;

procedure TJvDesktopAlert.SetFont(const Value: TFont);
begin
  FDesktopForm.lblText.Font := Value;
end;

procedure TJvDesktopAlert.SetHeaderFont(const Value: TFont);
begin
  FDesktopForm.lblHeader.Font := Value;
end;

procedure TJvDesktopAlert.SetHeaderText(const Value: string);
begin
  FDesktopForm.lblHeader.Caption := Value;
end;

procedure TJvDesktopAlert.SetHint(const Value: string);
begin
  FDesktopForm.Hint := Value;
end;

procedure TJvDesktopAlert.SetImage(const Value: TPicture);
begin
  FDesktopForm.imIcon.Picture := Value;
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

procedure TJvDesktopAlert.SetLocation(const Value: TJvDesktopAlertLocation);
begin
  //
end;

procedure TJvDesktopAlert.SetAlphaBlendValue(const Value: Byte);
begin
  FDesktopForm.MaxAlphaBlendValue := Value;
end;

procedure TJvDesktopAlert.SetMessageText(const Value: string);
begin
  FDesktopForm.lblText.Caption := Value;
  FDesktopForm.lblText.Update;
end;

procedure TJvDesktopAlert.SetParentFont(const Value: Boolean);
begin
  FDesktopForm.ParentFont := Value;
end;

procedure TJvDesktopAlert.SeTPopupMenu(const Value: TPopupMenu);
begin
  FDesktopForm.PopupMenu := Value;
end;

procedure TJvDesktopAlert.SetShowHint(const Value: Boolean);
begin
  FDesktopForm.ShowHint := Value;
end;

function TJvDesktopAlert.Showing: Boolean;
begin
  Result := (FDesktopForm <> nil) and FDesktopForm.Showing;
end;

procedure TJvDesktopAlert.SetWaitTime(const Value: Integer);
begin
  FDesktopForm.WaitTime := Value;
end;

procedure TJvDesktopAlert.SetOptions(const Value: TJvDesktopAlertOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    if not (daoCanMove in FOptions) then
      Exclude(FOptions, daoCanMoveAnywhere);
  end;
end;

function TJvDesktopAlert.GetCloseButtonClick: TNotifyEvent;
begin
  Result := FDesktopForm.tbClose.OnClick;
end;

procedure TJvDesktopAlert.SetCloseButtonClick(const Value: TNotifyEvent);
begin
  FDesktopForm.tbClose.OnClick := Value;
end;

//=== { TJvDesktopAlertStack } ===============================================

constructor TJvDesktopAlertStack.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TList.Create;
  FPosition := dapBottomRight;
end;

destructor TJvDesktopAlertStack.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TJvDesktopAlertStack.Add(AForm: TForm);
begin
  FItems.Add(AForm);
  UpdatePositions;
end;

function TJvDesktopAlertStack.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJvDesktopAlertStack.GetItems(Index: Integer): TJvFormDesktopAlert;
begin
  Result := TJvFormDesktopAlert(FItems[Index]);
  Assert((Result = nil) or (Result is TJvFormDesktopAlert));
end;

procedure TJvDesktopAlertStack.Remove(AForm: TForm);
var
  Index, PrevNilSlot: Integer;
  Form: TJvFormDesktopAlert;
begin
  if (AForm <> nil) and (AForm is TJvFormDesktopAlert) then
  begin
    // The basic trick here is to push piling forms down in the list, while keeping the
    // static ones (i.e. a form that has the mouse pointer over it) in place.
    Index := FItems.IndexOf(AForm);
    if Index >= 0 then
    begin
      FItems[Index] := nil;

      Inc(Index);
      while Index < FItems.Count do
      begin
        Form := Items[Index];
        if Assigned(Form) and (not Form.MouseInControl) then
        begin
          PrevNilSlot := Pred(Index);
          while FItems[PrevNilSlot] <> nil do
            Dec(PrevNilSlot);
          FItems[PrevNilSlot] := FItems[Index];
          FItems[Index] := nil;
        end;

        Inc(Index);
      end;

      while (Pred(FItems.Count) >= 0) and (FItems[Pred(FItems.Count)] = nil) do
        FItems.Delete(Pred(FItems.Count));

      UpdatePositions;
    end;
  end;
end;

procedure TJvDesktopAlertStack.SetPosition(const Value: TJvDesktopAlertPosition);
begin
  if FPosition <> Value then
  begin
//    if Value = dapCustom then raise
//      Exception.Create('TJvDesktopAlertStack does not handle dapCustom alerts!');
//    FItems.Clear;
    FPosition := Value;
  end;
end;

procedure TJvDesktopAlertStack.UpdatePositions;
var
  C, I: Integer;
  Form: TJvFormDesktopAlert;
  X, Y: Integer;
  R: TRect;
begin
  C := Count;
  if C > 0 then
  begin
    R := ScreenWorkArea;
    case Position of
      dapBottomRight:
        begin
          Y := R.Bottom;
          for I := 0 to Pred(C) do
          begin
            Form := Items[I];
            if Assigned(Form) and Form.Visible then
            begin
              X := R.Right - Form.Width;
              Dec(Y, Form.Height);
              Form.SetNewOrigin(X, Y);
            end;
          end;
        end;
      dapBottomLeft:
        begin
          X := R.Left;
          Y := R.Bottom;
          for I := 0 to Pred(C) do
          begin
            Form := Items[I];
            if Assigned(Form) and Form.Visible then
            begin
              Dec(Y, Form.Height);
              Form.SetNewOrigin(X, Y);
            end;
          end;
        end;
      dapTopRight:
        begin
          Y := R.Top;
          for I := 0 to Pred(C) do
          begin
            Form := Items[I];
            if Assigned(Form) and Form.Visible then
            begin
              X := R.Right - Form.Width;
              Form.SetNewOrigin(X, Y);
              Inc(Y, Form.Height);
            end;
          end;
        end;
      dapTopLeft:
        begin
          Y := R.Top;
          X := R.Left;
          for I := 0 to Pred(C) do
          begin
            Form := Items[I];
            if Assigned(Form) and Form.Visible then
            begin
              Form.SetNewOrigin(X, Y);
              Inc(Y, Form.Height);
            end;
          end;
        end;
    end;
  end;
end;

initialization

finalization
  FinalizeUnit(sUnitName);

end.

