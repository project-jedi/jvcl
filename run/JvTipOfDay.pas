{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTipsOfDay.PAS, released on 2001-02-28.

The Initial Developers of the Original Code are Sébastien Buysse [sbuysse att buypin dott com]
and Peter Thörnqvist [peter3 at sourceforge dot net]. Portions created by Sébastien Buysse
are Copyright (C) 2001 Sébastien Buysse. Portions created by Peter Thörnqvist
are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].
                Remko Bonte [remkobonte att myrealbox dott com]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvTipOfDay;

{$I jvcl.inc}

interface

uses
  Classes, Graphics, Controls, Messages, Forms, StdCtrls,
  JvAppStorage, JvBaseDlg, JvButtonPersistent, JvSpeedButton, JvTypes;
  
type
  TJvCanShowEvent = procedure(Sender: TObject; var CanShow: Boolean) of object;
  TJvTipOfDayOption = (toShowOnStartUp, toUseAppStorage, toShowWhenFormShown);
  TJvTipOfDayOptions = set of TJvTipOfDayOption;

  TJvTipOfDayStyle = (tsVC, tsStandard);

  TJvTipOfDay = class(TJvCommonDialogP)
  private
    FAppStorage: TJvCustomAppStorage;
    FAppStoragePath: string;
    FTitle: string;
    FCheckBoxText: string;
    FHeaderText: string;
    FColor: TColor;
    FDefaultFonts: Boolean;
    FTipFont: TFont;
    FHeaderFont: TFont;
    FButtonNext: TJvButtonPersistent;
    FButtonClose: TJvButtonPersistent;
    FOptions: TJvTipOfDayOptions;
    FTips: TStringList;
    FStyle: TJvTipOfDayStyle;
    FCurrentTip: Integer;
    FOnAfterExecute: TNotifyEvent;
    FOnCanShow: TJvCanShowEvent;
    { For reentrance check: }
    FRunning: Boolean;
    { FIsAutoExecute = False  -> User called Execute
      FIsAutoExecute = True   -> Execute is called in method Loaded }
    FIsAutoExecute: Boolean;
    { Maybe a bit overkill, but use a generic base class to access the
      visual components, thus enabling you to easily extend the
      'Tip of the Day' component }
    FTipLabel: TControl;
    FNextTipButton: TControl;
    FCheckBox: TButtonControl;
    {$IFDEF VCL}
    { Parent form: }
    FForm: TCustomForm;
    FDummyMsgSend: Boolean;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure FormHide(Sender : TObject);
    {$ENDIF VisualCLX}
    procedure FontChanged(Sender: TObject);
    // function GetRegKey: string;
    function GetTips: TStrings;
    function IsFontStored: Boolean;
    procedure SetButtonClose(const Value: TJvButtonPersistent);
    procedure SetButtonNext(const Value: TJvButtonPersistent);
    procedure SetDefaultFonts(const Value: Boolean);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetTipFont(const Value: TFont);
    procedure SetTips(const Value: TStrings);
    procedure SetStyle(const Value: TJvTipOfDayStyle);
  protected
    procedure SetAppStorage(Value: TJvCustomAppStorage);
    { Called after the dialog has been shown. Fires the OnAfterExecute
      event, thus enabling the user to update the appstorage or other
      persistent data: }
    procedure DoAfterExecute; virtual;
    { Determines whether the dialog can be shown; user can write an
      event handler to override the default behaviour: }
    function CanShow: Boolean; virtual;
    { Initializes the "Standard Component Gallery" Tip of the Day dialog: }
    procedure InitStandard(AForm: TForm);
    { Initializes the "New VC++ look" Tip of the Day dialog: }
    procedure InitVC(AForm: TForm);
    { Called in Loaded method; sets flag FIsAutoExecute to True to indicate
      that the Execute was automatically called, thus not by the user: }
    procedure AutoExecute;
    { Functions to read/write from a default location a value that
      determines whether the dialog must be shown; if the user wants
      to store this value in another location he must write an OnCanShow
      and an OnAfterExecute event handler: }
    function ReadFromAppStorage: Boolean; virtual;
    procedure WriteToAppStorage(DoShowOnStartUp: Boolean); virtual;
    { Sets the fonts (HeaderFont and TipFont) to the default fonts
      associated with Style: }
    procedure UpdateFonts;
    { Places a new tip on the dialog: }
    procedure UpdateTip;
    { Handles button clicks on the 'Next' button: }
    procedure HandleNextClick(Sender: TObject);
    { Hooks/Unhooks the parent form, this is done if
      toShowWhenFormShown is in Options }
    procedure HookForm;
    procedure UnHookForm;
    {$IFDEF VCL}
    { The hook; responds when the parent form activates }
    function HookProc(var Msg: TMessage): Boolean;
    {$ENDIF VCL}
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
    property IsAutoExecute: Boolean read FIsAutoExecute;
  published
    property AppStorage: TJvCustomAppStorage read FAppStorage write SetAppStorage;
    property AppStoragePath: string read FAppStoragePath write FAppStoragePath;
    property ButtonNext: TJvButtonPersistent read FButtonNext write SetButtonNext;
    property ButtonClose: TJvButtonPersistent read FButtonClose write SetButtonClose;
    property CheckBoxText: string read FCheckBoxText write FCheckBoxText;
    property Color: TColor read FColor write FColor default clWhite;
    property DefaultFonts: Boolean read FDefaultFonts write SetDefaultFonts default True;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont stored IsFontStored;
    property HeaderText: string read FHeaderText write FHeaderText;
    property OnAfterExecute: TNotifyEvent read FOnAfterExecute write FOnAfterExecute;
    property OnCanShow: TJvCanShowEvent read FOnCanShow write FOnCanShow;
    property Options: TJvTipOfDayOptions read FOptions write FOptions default [toShowOnStartUp];
    property Style: TJvTipOfDayStyle read FStyle write SetStyle default tsVC;
    property TipFont: TFont read FTipFont write SetTipFont stored IsFontStored;
    property Tips: TStrings read GetTips write SetTips;
    property Title: string read FTitle write FTitle;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils,
  {$IFDEF VCL}
  Windows, ExtCtrls, Dialogs,  JvWndProcHook,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QExtCtrls, QDialogs,
  {$ENDIF VisualCLX}
  JvButton, JvResources;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvTipOfDay.res}
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
{$R ../Resources/JvTipOfDay.res}
{$ENDIF UNIX}

{$IFDEF VisualCLX}
const
  psInsideFrame: TPenStyle = psSolid;
{$ENDIF VisualCLX}


type
  TControlAccessProtected = class(TControl);
  TButtonControlAccessProtected = class(TButtonControl);

constructor TJvTipOfDay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTips := TStringList.Create;

  FTipFont := TFont.Create;
  FTipFont.OnChange := FontChanged;

  FHeaderFont := TFont.Create;
  FHeaderFont.OnChange := FontChanged;

  FButtonNext := TJvButtonPersistent.Create;
  FButtonNext.Caption := RsNextCaption;
  FButtonNext.Flat := False;
  FButtonNext.HotTrack := False;

  FButtonClose := TJvButtonPersistent.Create;
  FButtonClose.Caption := RsCloseCaption;
  FButtonClose.Flat := False;
  FButtonClose.HotTrack := False;

  FHeaderText := RsTipsHeaderText;
  FTitle := RsTipsTitle;
  FCheckBoxText := RsTipsCheckBoxText;

  FColor := clWhite;
  FStyle := tsVC;
  FDefaultFonts := True;
  FOptions := [toShowOnStartUp];
  FIsAutoExecute := False;

  UpdateFonts;
end;

destructor TJvTipOfDay.Destroy;
begin
  FTips.Free;
  FTipFont.Free;
  FHeaderFont.Free;
  FButtonNext.Free;
  FButtonClose.Free;
  inherited Destroy;
end;

procedure TJvTipOfDay.SetAppStorage(Value: TJvCustomAppStorage);
begin
  FAppStorage := Value;
end;

procedure TJvTipOfDay.AutoExecute;
begin
  FIsAutoExecute := True;
  try
    Execute;
  finally
    FIsAutoExecute := False;
  end;
end;

function TJvTipOfDay.CanShow: Boolean;
begin
  // Show the dialog if the user called Execute (FIsAutoExecute=False) or
  // if flag toShowOnStartUp is in Options..
  Result := not FIsAutoExecute or (toShowOnStartUp in Options);

  // ..but enable the user to override this behaviour
  if not (csDesigning in ComponentState) and Assigned(FOnCanShow) then
    FOnCanShow(Self, Result);
end;

procedure TJvTipOfDay.DoAfterExecute;
begin
  if csDesigning in ComponentState then
    Exit;
  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(Self);
end;

procedure TJvTipOfDay.Execute;
var
  LForm: TForm;
begin
  // Reentrance check
  if FRunning then
    Exit;
  FRunning := True;
  try
    if toUseAppStorage in Options then
    begin
      if ReadFromAppStorage then
        Include(FOptions, toShowOnStartUp)
      else
        Exclude(FOptions, toShowOnStartUp);
    end;

    if not CanShow then
      Exit;

    { toShowOnStartUp will be changed in ExecuteVS/ExecuteStandard if
      the user changes the checkbox }
    LForm := TForm.Create(Application);
    with LForm do
    try
      if Style = tsVC then
        InitVC(LForm)
      else
        InitStandard(LForm);

      Randomize;
      FCurrentTip := Random(Tips.Count);

      UpdateTip;

  {$IFDEF VCL}
      ShowModal;

      if TButtonControlAccessProtected(FCheckBox).Checked then
        Include(FOptions, toShowOnStartUp)
      else
        Exclude(FOptions, toShowOnStartUp)
    finally
      Free;
    end;

    DoAfterExecute;

    if toUseAppStorage in Options then
      WriteToAppStorage(toShowOnStartUp in Options);
  finally
    FRunning := False;
  end;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
      OnHide := FormHide ;  // onclose
      FormStyle := fsStayOnTop;
      Show ;  // Shown non modal
    except
      Free;
    end;
  except
    FRunning := False;
  end;
  {$ENDIF VisualCLX}
end;

{$IFDEF VisualCLX}
procedure TJvTipOfDay.FormHide(Sender: TObject);
begin
  with Sender as TForm do
  begin
    if TButtonControlAccessProtected(FCheckBox).Checked then
      Include(FOptions, toShowOnStartUp)
    else
      Exclude(FOptions, toShowOnStartUp);
    Release;   // destroy it
    FRunning := False;
  end;
end;
{$ENDIF VisualCLX}

procedure TJvTipOfDay.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FAppStorage) then
    FAppStorage := nil;
end;

procedure TJvTipOfDay.FontChanged(Sender: TObject);
begin
  FDefaultFonts := False;
end;

{
function TJvTipOfDay.GetRegKey: string;
begin
  Result := Application.Title + '_' + Name;
end;
}

procedure TJvTipOfDay.HandleNextClick(Sender: TObject);
begin
  FCurrentTip := (FCurrentTip + 1) mod Tips.Count;
  UpdateTip;
end;

procedure TJvTipOfDay.HookForm;
begin
  {$IFDEF VCL}
  if Owner is TControl then
    FForm := GetParentForm(TControl(Owner))
  else
    FForm := nil;
  if not Assigned(FForm) then
    Exit;
  FDummyMsgSend := False;
  JvWndProcHook.RegisterWndProcHook(FForm, HookProc, hoAfterMsg);
  {$ENDIF VCL}
end;

{$IFDEF VCL}
function TJvTipOfDay.HookProc(var Msg: TMessage): Boolean;
begin
  Result := False;
  case Msg.Msg of
    WM_ACTIVATEAPP:
      begin
        { Maybe the form is hooked by other components that are also
          waiting for WM_ACTIVATEAPP; if we call AutoExecute now, those
          components will not receive that message until the tip dialog
          is closed; Thus we send a dummy message to the hooked window and
          respond to that message }
        PostMessage(FForm.Handle, WM_NULL, 0, 0);
        FDummyMsgSend := True;
      end;
    // (rom) better use a private message value
    WM_NULL:
      if not FRunning and FDummyMsgSend then
      begin
        FDummyMsgSend := False;
        AutoExecute;
        UnHookForm;
      end;
  end;
end;
{$ENDIF VCL}

procedure TJvTipOfDay.InitStandard(AForm: TForm);
begin
  with AForm do
  begin
    {$IFDEF VCL}
    BorderStyle := bsDialog;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    BorderStyle := fbsDialog;
    {$ENDIF VisualCLX}
    { Title }
    Caption := Self.Title;
    ClientHeight := 267;
    ClientWidth := 347;

    // Maybe poMainFormCenter? If so check if whe're at design-time
    Position := poScreenCenter;

    with TShape.Create(AForm) do
    begin
      Parent := AForm;
      SetBounds(18, 18, 311, 200);
      Brush.Color := clBtnFace;
      Pen.Color := cl3DDkShadow;
    end;
    with TShape.Create(AForm) do
    begin
      Parent := AForm;
      SetBounds(30, 28, 285, 180);
      Brush.Color := Self.Color;
      Pen.Color := Self.Color;
    end;

    with TImage.Create(AForm) do
    begin
      Parent := AForm;
      SetBounds(30, 28, 40, 53);
      AutoSize := True;
      Picture.Bitmap.LoadFromResourceName(HInstance, 'JVTIPOFDAYPIC2');
      Transparent := True;
    end;
    { Header: 'Did you know...' }
    with TLabel.Create(AForm) do
    begin
      Parent := AForm;
      SetBounds(70, 46, 105, 16);
      Caption := Self.HeaderText;
      Color := Self.Color;
      Font := Self.HeaderFont;
    end;
    { Tip label }
    FTipLabel := TLabel.Create(AForm);
    with TLabel(FTipLabel) do
    begin
      Parent := AForm;
      SetBounds(42, 102, 261, 83);
      AutoSize := False;
      Color := Self.Color;
      WordWrap := True;
      Font := Self.TipFont;
    end;

    { CheckBox: 'Show Tips on StartUp' }
    FCheckBox := TCheckBox.Create(AForm);
    with TCheckBox(FCheckBox) do
    begin
      Parent := AForm;
      SetBounds(20, 236, 144, 17);
      Caption := Self.CheckBoxText;
      Checked := toShowOnStartUp in Options;
    end;

    { ButtonNext }
    if ButtonNext.Flat then
      { Flat means no focus.. }
      FNextTipButton := TJvSpeedButton.Create(AForm)
    else
      { ..so create a TJvButton unless Flat is set to True }
      FNextTipButton := TJvCustomButton.Create(AForm);

    with TControlAccessProtected(FNextTipButton) do
    begin
      Parent := AForm;
      SetBounds(164, 232, 75, 25);
      OnClick := HandleNextClick;
      Assign(ButtonNext);
    end;

    { ButtonClose }
    if ButtonClose.Flat then
      { Flat means no focus.. }
      with TJvSpeedButton.Create(AForm) do
      begin
        Parent := AForm;
        SetBounds(252, 232, 75, 25);
        Assign(ButtonClose);
        ModalResult := mrOk;
      end
    else
      { ..so create a TJvButton unless Flat is set to True }
      with TJvCustomButton.Create(AForm) do
      begin
        Parent := AForm;
        SetBounds(251, 232, 75, 25);
        Cancel := True;
        Default := True;
        Assign(ButtonClose);
        ModalResult := mrOk;
      end;
  end;
end;

procedure TJvTipOfDay.InitVC(AForm: TForm);
begin
  with AForm do
  begin
    {$IFDEF VCL}
    BorderStyle := bsDialog;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    BorderStyle := fbsDialog;
    {$ENDIF VisualCLX}

    { Title }
    Caption := Self.Title;
    ClientHeight := 258;
    ClientWidth := 400;

    // Maybe poMainFormCenter? If so check if whe're at design-time
    Position := poScreenCenter;
    with TShape.Create(AForm) do
    begin
      Parent := AForm;
      SetBounds(8, 8, 384, 206);
      Brush.Color := Self.Color;
      Pen.Color := clGray;
      Pen.Style := psInsideFrame;
    end;
    with TShape.Create(AForm) do
    begin
      Parent := AForm;
      SetBounds(8, 8, 53, 205);
      Brush.Color := clGray;
      Pen.Color := clGray;
    end;
    with TShape.Create(AForm) do
    begin
      Parent := AForm;
      SetBounds(61, 63, 330, 1);
      Brush.Color := clGray;
      Pen.Color := clGray;
      Pen.Width := 10;
    end;

    { Header: 'Did you know...' }
    with TLabel.Create(AForm) do
    begin
      Parent := AForm;
      SetBounds(71, 24, 135, 23);
      Caption := Self.HeaderText;
      Color := Self.Color;
      Font := Self.HeaderFont;
    end;
    with TImage.Create(AForm) do
    begin
      Parent := AForm;
      SetBounds(21, 22, 41, 43);
      Picture.Bitmap.LoadFromResourceName(HInstance, 'JVTIPOFDAYPIC1');
    end;

    { CheckBox: 'Show Tips on StartUp' }
    FCheckBox := TCheckBox.Create(AForm);
    with TCheckBox(FCheckBox) do
    begin
      Parent := AForm;
      SetBounds(8, 225, 200, 17);
      Caption := Self.CheckBoxText;
      Checked := toShowOnStartUp in Options;
    end;

    { ButtonNext }
    if ButtonNext.Flat then
      { Flat means no focus.. }
      FNextTipButton := TJvSpeedButton.Create(AForm)
    else
      { ..so create a TJvButton unless Flat is set to True }
      FNextTipButton := TJvCustomButton.Create(AForm);

    with TControlAccessProtected(FNextTipButton) do
    begin
      Parent := AForm;
      SetBounds(227, 225, 77, 25);
      OnClick := HandleNextClick;
      Assign(ButtonNext);
    end;

    { ButtonClose }
    if ButtonClose.Flat then
      { Flat means no focus.. }
      with TJvSpeedButton.Create(AForm) do
      begin
        Parent := AForm;
        SetBounds(317, 225, 75, 25);
        Assign(ButtonClose);
        ModalResult := mrOk;
      end
    else
      { ..so create a TJvButton unless Flat is set to True }
      with TJvCustomButton.Create(AForm) do
      begin
        Parent := AForm;
        SetBounds(317, 225, 75, 25);
        Cancel := True;
        Default := True;
        Assign(ButtonClose);
        ModalResult := mrOk;
      end;

    { Tip label }
    FTipLabel := TLabel.Create(AForm);
    with TLabel(FTipLabel) do
    begin
      Parent := AForm;
      SetBounds(71, 75, 306, 134);
      AutoSize := False;
      Color := Self.Color;
      WordWrap := True;
      Font := Self.TipFont;
    end;
  end;
end;

function TJvTipOfDay.IsFontStored: Boolean;
begin
  Result := not DefaultFonts;
end;

procedure TJvTipOfDay.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    Exit;

  if toShowWhenFormShown in Options then
    HookForm
  else
    // Call AutoExecute, which will call Execute.
    // Execute will determine (by calling CanShow) if the dialog actually
    // must be shown.
    AutoExecute;
end;

procedure TJvTipOfDay.LoadFromFile(const AFileName: string);
begin
  if Length(AFileName) = 0 then
    with TOpenDialog.Create(Application) do
    try
      if Execute then
        Tips.LoadFromFile(FileName);
    finally
      Free;
    end
  else
  if FileExists(AFileName) then
    Tips.LoadFromFile(AFileName);
end;

function TJvTipOfDay.ReadFromAppStorage: Boolean;
begin
  if Assigned(AppStorage) then
    Result := AppStorage.ReadBoolean(AppStorage.ConcatPaths([AppStoragePath,RsStoreShowOnStartUp]), toShowOnStartUp in Options)
  else
    Result := False;
end;

procedure TJvTipOfDay.SaveToFile(const AFileName: string);
begin
  if Length(AFileName) = 0 then
    with TSaveDialog.Create(Application) do
    try
      if Execute then
        Tips.SaveToFile(FileName);
    finally
      Free;
    end
  else
    Tips.SaveToFile(AFileName);
end;

procedure TJvTipOfDay.SetButtonClose(const Value: TJvButtonPersistent);
begin
  FButtonClose.Assign(Value);
end;

procedure TJvTipOfDay.SetButtonNext(const Value: TJvButtonPersistent);
begin
  FButtonNext.Assign(Value);
end;

procedure TJvTipOfDay.SetDefaultFonts(const Value: Boolean);
begin
  if Value <> FDefaultFonts then
  begin
    FDefaultFonts := Value;
    if FDefaultFonts then
      UpdateFonts;
  end;
end;

procedure TJvTipOfDay.SetHeaderFont(const Value: TFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TJvTipOfDay.SetStyle(const Value: TJvTipOfDayStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    if FDefaultFonts then
      UpdateFonts;
  end;
end;

procedure TJvTipOfDay.SetTipFont(const Value: TFont);
begin
  FTipFont.Assign(Value);
end;

function TJvTipOfDay.GetTips: TStrings;
begin
  Result := FTips;
end;

procedure TJvTipOfDay.SetTips(const Value: TStrings);
begin
  FTips.Assign(Value);
end;

procedure TJvTipOfDay.UnHookForm;
begin
  {$IFDEF VCL}
  JvWndProcHook.UnRegisterWndProcHook(FForm, HookProc, hoAfterMsg);
  {$ENDIF VCL}
end;

procedure TJvTipOfDay.UpdateFonts;
var
  SavedDefaultFonts: Boolean;
begin
  { If we change the fonts, FDefaultFonts will be set to False (in
    FontChanged), thus before changing we must save the current
    value of FDefaultFonts
  }
  SavedDefaultFonts := FDefaultFonts;

  FTipFont.Charset := DEFAULT_CHARSET;
  FTipFont.Color := clWindowText;
  FTipFont.Name := 'MS Sans Serif';
  FTipFont.Pitch := fpDefault;
  FTipFont.Size := 8;
  FTipFont.Style := [];

  FHeaderFont.Charset := DEFAULT_CHARSET;
  FHeaderFont.Color := clWindowText;
  FHeaderFont.Pitch := fpDefault;
  FHeaderFont.Style := [fsBold];

  case Style of
    tsVC:
      begin
        FHeaderFont.Name := 'Times New Roman';
        FHeaderFont.Size := 15;
      end;
    tsStandard:
      begin
        FHeaderFont.Name := 'System';
        FHeaderFont.Size := 10;
      end;
  end;

  FDefaultFonts := SavedDefaultFonts;
end;

procedure TJvTipOfDay.UpdateTip;
begin
  if Tips.Count > 0 then
    TControlAccessProtected(FTipLabel).Caption := Tips[FCurrentTip];
  if Tips.Count <= 1 then
    TControlAccessProtected(FNextTipButton).Enabled := False;
end;

procedure TJvTipOfDay.WriteToAppStorage(DoShowOnStartUp: Boolean);
begin
  if Assigned(AppStorage) then
    AppStorage.WriteBoolean(AppStorage.ConcatPaths([AppStoragePath,RsStoreShowOnStartUp]), DoShowOnStartUp);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

