unit JvCoreIntf;
{$I JVCL.INC}
interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JVCLVer, JvTypes;

type
  IJVCLCoreComponent = interface
    ['{497B3435-EC5A-4CD4-A030-9E64DE1E57B0}']
    function GetAboutJVCL: TJVCLAboutInfo;
    procedure SetAboutJVCL(const Value: TJVCLAboutInfo);
    property AboutJVCL: TJVCLAboutInfo read GetAboutJVCL write SetAboutJVCL;
  end;

  IJVCLCoreControl = interface(IJVCLCoreComponent)
    ['{C856548E-76C3-4030-B63C-7582501385B3}']
    function GetControl: TControl;
    procedure SetControl(const Value: TControl);
    function GetHotTrack: Boolean;
    procedure SetHotTrack(const Value: Boolean);
    function GetHotTrackFont: TFont;
    procedure SetHotTrackFont(const Value: TFont);
    function GetHintColor: TColor;
    procedure SetHintColor(const Value: TColor);
    function GetOnMouseEnter: TNotifyEvent;
    procedure SetOnMouseEnter(const Value: TNotifyEvent);
    function GetOnMouseLeave: TNotifyEvent;
    procedure SetOnMouseLeave(const Value: TNotifyEvent);
    function GetOnParentColorChange: TNotifyEvent;
    procedure SetOnParentColorChange(const Value: TNotifyEvent);

    property HotTrack: Boolean read GetHotTrack write SetHotTrack;
    property HotTrackFont: TFont read GetHotTrackFont write SetHotTrackFont;
    property HintColor: TColor read GetHintColor write SetHintColor;

    property OnMouseEnter: TNotifyEvent read GetOnMouseEnter write SetOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read GetOnMouseLeave write SetOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read GetOnParentColorChange write SetOnParentColorChange;
  end;

  IJVCLCoreWinControl = interface(IJVCLCoreControl)
  ['{2964A416-52C8-4DF3-B532-8E55F552B1CF}']
    function GetKillFocus: TJvFocusChangeEvent;
    function GetSetFocus: TJvFocusChangeEvent;
    procedure SetKillFocus(const Value: TJvFocusChangeEvent);
    procedure SetSetFocus(const Value: TJvFocusChangeEvent);
    function GetOnCtl3DChanged: TNotifyEvent;
    procedure SetOnCtl3DChanged(const Value: TNotifyEvent);

    property OnCtl3DChanged: TNotifyEvent read GetOnCtl3DChanged write SetOnCtl3DChanged;
    property OnKillFocus: TJvFocusChangeEvent read GetKillFocus write SetKillFocus;
    property OnSetFocus: TJvFocusChangeEvent read GetSetFocus write SetSetFocus;
  end;

  TJVCLCoreComponent = class(TPersistent)
  private
    FAboutJVCL: TJVCLAboutInfo;
    function GetAboutJVCL: TJVCLAboutInfo;
    procedure SetAboutJVCL(const Value: TJVCLAboutInfo);
  public
    constructor Create(AOwner:TControl);virtual;
  published
    property AboutJVCL: TJVCLAboutInfo read GetAboutJVCL write SetAboutJVCL;
  end;

  TJVCLCoreControl = class(TJVCLCoreComponent)
  private
    FControl:TControl;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChange: TNotifyEvent;
    FHotTrack: Boolean;
    FHotTrackFont: TFont;
    FFontSave: TFont;
    FHintColor: TColor;
    FOldWndProc: TWndMethod;
    FMouseOver: boolean;
    FSavedHint: TColor;
  protected
    function GetOwner: TPersistent;override;
    function GetControl:TControl;
    function GetHintColor: TColor;
    function GetHotTrack: Boolean;
    function GetHotTrackFont: TFont;
    function GetOnMouseEnter: TNotifyEvent;
    function GetOnMouseLeave: TNotifyEvent;
    function GetOnParentColorChange: TNotifyEvent;
    procedure SetControl(const Value: TControl);virtual;
    procedure SetHintColor(const Value: TColor);
    procedure SetHotTrack(const Value: Boolean);
    procedure SetHotTrackFont(const Value: TFont);
    procedure SetOnMouseEnter(const Value: TNotifyEvent);
    procedure SetOnMouseLeave(const Value: TNotifyEvent);
    procedure SetOnParentColorChange(const Value: TNotifyEvent);
  protected
    procedure ControlWndProc(var Message: TMessage); virtual;
    procedure DoMouseEnter(var Message: TMessage); virtual;
    procedure DoMouseLeave(var Message: TMessage); virtual;
    procedure DoParentColorChanged(var Message: TMessage); virtual;

    property Control: TControl read GetControl write SetControl;
  public
    constructor Create(AOwner:TControl);override;
//    constructor Create(AOwner:TComponent);override;
    destructor Destroy; override;
  published
    property HotTrack: Boolean read GetHotTrack write SetHotTrack;
    property HotTrackFont: TFont read GetHotTrackFont write SetHotTrackFont;
    property HintColor: TColor read GetHintColor write SetHintColor;

    property OnMouseEnter: TNotifyEvent read GetOnMouseEnter write SetOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read GetOnMouseLeave write SetOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read GetOnParentColorChange write SetOnParentColorChange;
  end;


  TJVCLCoreWinControl = class(TJVCLCoreControl)
  private
    FOnCtl3DChanged: TNotifyEvent;
    FOnKillFocus: TJvFocusChangeEvent;
    FOnSetFocus: TJvFocusChangeEvent;
    function GetKillFocus: TJvFocusChangeEvent;
    function GetSetFocus: TJvFocusChangeEvent;
    procedure SetKillFocus(const Value: TJvFocusChangeEvent);
    procedure SetSetFocus(const Value: TJvFocusChangeEvent);
    function GetOnCtl3DChanged: TNotifyEvent;
    procedure SetOnCtl3DChanged(const Value: TNotifyEvent);
  protected
    procedure ControlWndProc(var Message: TMessage); override;
    procedure DoCtl3DChanged(var Message: TMessage); virtual;
    procedure DoKillFocus(var Message: TMessage); virtual;
    procedure DoSetFocus(var Message: TMessage); virtual;
  published
    property AboutJVCL;
    property HotTrack;
    property HotTrackFont;
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property OnCtl3DChanged: TNotifyEvent read GetOnCtl3DChanged write SetOnCtl3DChanged;
    property OnKillFocus: TJvFocusChangeEvent read GetKillFocus write SetKillFocus;
    property OnSetFocus: TJvFocusChangeEvent read GetSetFocus write SetSetFocus;
  end;

  TJVCLCoreClass = class of TJVCLCoreComponent;

  { test controls (to be removed) }

  TJvIntfLabel = class(TLabel, IJVCLCoreControl)
  private
    FJVCLCore: TJVCLCoreControl;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property JVCLCore: TJVCLCoreControl read FJVCLCore implements IJVCLCoreControl;
  end;

  TJvIntfButton = class(TButton, IJVCLCoreWinControl)
  private
    FJVCLCore: TJVCLCoreWinControl;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property JVCLCore: TJVCLCoreWinControl read FJVCLCore implements IJVCLCoreWinControl;
  end;
  


function CreateJVCLCoreClass(AControl: TControl; AClass: TJVCLCoreClass = nil): TJVCLCoreComponent;


implementation


type
  THackControl = class(TControl);

function CreateJVCLCoreClass(AControl: TControl; AClass: TJVCLCoreClass = nil): TJVCLCoreComponent;
var obj:IJVCLCoreControl;
begin
  if AClass = nil then
    Result := TJVCLCoreComponent.Create(AControl)
  else
    Result := AClass.Create(AControl);
  if Supports(Result,IJVCLCoreControl,obj) then
    obj.SetControl(AControl);
{$IFDEF COMPILER6_UP}
//  Result.Name := Copy(Result.ClassName, 2, MaxInt);
//  Result.SetSubComponent(true);
{$ENDIF}
end;

{ TJVCLCoreComponent }

constructor TJVCLCoreComponent.Create(AOwner: TControl);
begin
  inherited Create;
end;

function TJVCLCoreComponent.GetAboutJVCL: TJVCLAboutInfo;
begin
  Result := FAboutJVCL;
end;

procedure TJVCLCoreComponent.SetAboutJVCL(const Value: TJVCLAboutInfo);
begin
  FAboutJVCL := Value;
end;

{ TJVCLCoreControl }

procedure TJVCLCoreControl.ControlWndProc(var Message: TMessage);
begin
  if Assigned(FOldWndProc) then
    FOldWndProc(Message);
  case Message.Msg of
    CM_MOUSEENTER:
      DoMouseEnter(Message);
    CM_MOUSELEAVE:
      DoMouseLeave(Message);
    CM_PARENTCOLORCHANGED:
      DoParentColorChanged(Message);
  end;
end;

constructor TJVCLCoreControl.Create(AOwner:TControl);
// constructor TJVCLCoreControl.Create(AOwner:TComponent);
begin
  inherited;
  if AOwner is TControl then
    Control := TControl(AOwner);
  FHotTrackFont := TFont.Create;
  FFontSave := TFont.Create;
  HintColor := clInfoBk;
end;

destructor TJVCLCoreControl.Destroy;
begin
  Control := nil;
  FHotTrackFont.Free;
  FFontSave.Free;
  inherited;
end;

procedure TJVCLCoreControl.DoMouseEnter(var Message: TMessage);
begin
  if not (FMouseOver) then
  begin
    FSavedHint := Application.HintColor;
    Application.HintColor := FHintColor;
    if FHotTrack then
    begin
      FFontSave.Assign(THackControl(Control).Font);
      THackControl(Control).Font.Assign(FHotTrackFont);
    end;
    FMouseOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Control);
end;

procedure TJVCLCoreControl.DoMouseLeave(var Message: TMessage);
begin
  if FMouseOver then
  begin
    Application.HintColor := FSavedHint;
    if FHotTrack then
      THackControl(Control).Font.Assign(FFontSave);
    FMouseOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Control);
end;

procedure TJVCLCoreControl.DoParentColorChanged(var Message: TMessage);
begin
  if Assigned(FOnParentColorChange) then
    FOnParentColorChange(Control);
end;

function TJVCLCoreControl.GetControl: TControl;
begin
  Result := FControl;
end;

function TJVCLCoreControl.GetHintColor: TColor;
begin
  Result := FHintColor;
end;

function TJVCLCoreControl.GetHotTrack: Boolean;
begin
  Result := FHotTrack;
end;

function TJVCLCoreControl.GetHotTrackFont: TFont;
begin
  Result := FHotTrackFont;
end;

function TJVCLCoreControl.GetOnMouseEnter: TNotifyEvent;
begin
  Result := FOnMouseEnter;
end;

function TJVCLCoreControl.GetOnMouseLeave: TNotifyEvent;
begin
  Result := FOnMouseLeave;
end;

function TJVCLCoreControl.GetOnParentColorChange: TNotifyEvent;
begin
  Result := FOnParentColorChange;
end;

function TJVCLCoreControl.GetOwner: TPersistent;
begin
  Result := FControl;
end;

procedure TJVCLCoreControl.SetControl(const Value: TControl);
begin
  if FControl <> Value then
  begin
    if FControl <> nil then
      FControl.WindowProc := FOldWndProc;
    FControl := Value;
    if FControl <> nil then
    begin
      FOldWndProc := FControl.WindowProc;
      FControl.WindowProc := ControlWndProc;
    end;
  end;
end;

procedure TJVCLCoreControl.SetHintColor(const Value: TColor);
begin
  FHintColor := Value;
end;

procedure TJVCLCoreControl.SetHotTrack(const Value: Boolean);
begin
  FHotTrack := Value;
end;

procedure TJVCLCoreControl.SetHotTrackFont(const Value: TFont);
begin
  FHotTrackFont.Assign(Value);
end;


procedure TJVCLCoreControl.SetOnMouseEnter(const Value: TNotifyEvent);
begin
  FOnMouseEnter := Value;
end;

procedure TJVCLCoreControl.SetOnMouseLeave(const Value: TNotifyEvent);
begin
  FOnMouseLeave := Value;
end;

procedure TJVCLCoreControl.SetOnParentColorChange(
  const Value: TNotifyEvent);
begin
  FOnParentColorChange := Value;
end;
{ TJVCLCoreWinControl }

procedure TJVCLCoreWinControl.ControlWndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    CM_CTL3DCHANGED:
      DoCtl3DChanged(Message);
    WM_KILLFOCUS:
      DoKillFocus(Message);
    WM_SETFOCUS:
      DoSetFocus(Message);
  end;
end;

procedure TJVCLCoreWinControl.DoCtl3DChanged(var Message: TMessage);
begin
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Control);
end;

procedure TJVCLCoreWinControl.DoKillFocus(var Message: TMessage);
begin
  if Assigned(FOnKillFocus) then
    FOnKillFocus(Control, FindControl(Message.WParam));
end;

procedure TJVCLCoreWinControl.DoSetFocus(var Message: TMessage);
begin
  if Assigned(FOnSetFocus) then
    FOnSetFocus(Control, FindControl(Message.wParam));
end;

function TJVCLCoreWinControl.GetKillFocus: TJvFocusChangeEvent;
begin
  Result := FOnKillFocus;
end;

function TJVCLCoreWinControl.GetOnCtl3DChanged: TNotifyEvent;
begin
  Result := FOnCtl3DChanged;
end;

function TJVCLCoreWinControl.GetSetFocus: TJvFocusChangeEvent;
begin
  Result := FOnSetFocus;
end;

procedure TJVCLCoreWinControl.SetKillFocus(const Value: TJvFocusChangeEvent);
begin
  FOnKillFocus := Value;
end;

procedure TJVCLCoreWinControl.SetOnCtl3DChanged(const Value: TNotifyEvent);
begin
  FOnCtl3DChanged := Value;
end;

procedure TJVCLCoreWinControl.SetSetFocus(const Value: TJvFocusChangeEvent);
begin
  FOnSetFocus := Value;
end;

{ TJvIntfLabel }

constructor TJvIntfLabel.Create(AOwner: TComponent);
begin
  inherited;
  FJVCLCore := CreateJVCLCoreClass(self, TJVCLCoreControl) as TJVCLCoreControl;
end;

{ TJvIntfButton }

constructor TJvIntfButton.Create(AOwner: TComponent);
begin
  inherited;
  FJVCLCore := CreateJVCLCoreClass(self, TJVCLCoreWinControl) as TJVCLCoreWinControl;
end;



initialization
  // why is this needed?
  RegisterClasses([TJvIntfButton,TJvIntfLabel]);

end.

