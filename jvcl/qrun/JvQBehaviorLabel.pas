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

The Original Code is: JvBehaviorLabel.PAS, released on 2003-03-24.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):
Sébastien Buysse [sbuysse att buypin dott com] - original author of the merged components
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
* TJvBehaviorLabel is a merging of several label components in JVCL: JvSpecialLabel,
  JvRealLabel, JvBouncingLabel, JvBlinkingLabel and JvAppearingLabel
* To change the way the label works, change the Behavior property: this in turn changes the
  BehaviorOptions property to show only options available for the current Behavior.
* New behaviors can be added by creating a sub-class of TJvLabelBehavior, implement the
  functionality and register it with RegisterLabelBehaviorOptions.

Known Issues:
* Changing Behavior at design-time does not update the BehaviorOptions property unless
  you collapse / expand the Options property in the OI manually. No known solution yet. SOLVED

-----------------------------------------------------------------------------}
// $Id$

unit JvQBehaviorLabel;

{$I jvcl.inc}

interface

uses
  Classes, QWindows, QMessages, QControls, QExtCtrls,
  JvQExStdCtrls;

type
  TJvCustomBehaviorLabel = class;

  TJvLabelBehaviorName = string;
  TJvLabelScrollDirection = (sdLeftToRight, sdRightToLeft);
  TJvAppearDirection = (drFromLeft, drFromRight, drFromTop, drFromBottom);

  // TJvLabelBehavior is the base class for label behaviors
  // To create a new behavior, derive a new class from this base class,
  // add appropriate published properties, override the Start, Stop and possibly the OwnerResize methods.
  // Register the new behavior by calling RegisterLabelBehaviorOptions
  TJvLabelBehavior = class(TPersistent)
  private
    FLabel: TJvCustomBehaviorLabel;
    FTmpActive: Boolean;
    FActive: Boolean;
    FTemporary: Boolean;
    procedure SetActive(const Value: Boolean);
  protected
    // Call Suspend to store the current state of the Active property and
    // set Active to False. If the behavior was already inactive, Suspend does nothing
    procedure Suspend;
    // Call Resume to set the Active property to the state it was in before calling Suspend.
    // Resume sets Active to True if it was True when Suspend was called.
    // If Active was False before calling Suspend, Resume does nothing
    procedure Resume;
    // OwnerResize is called when the OwnerLabel is resized. Override this
    // method to do special processing when the OwnerLabel changes it's size or position.
    // OwnerResize does nothing in this class
    procedure OwnerResize; virtual;
    // Start is automatically called when Active is set to True
    // Override this method to take special action when the behavior is "started".
    // Start does nothing in this class
    procedure Start; virtual;
    // Stop is automatically called when Active is set to True
    // Override this method to take special action when the behavior is "stopped".
    // Stop does nothing in this class
    procedure Stop; virtual;
    // The label that the behavior is acting upon
    property OwnerLabel: TJvCustomBehaviorLabel read FLabel;
  public
    constructor Create(ALabel: TJvCustomBehaviorLabel); virtual;
    destructor Destroy; override;
  published
    // Set Active to True to enable the behavior and set it to False to disable it.
    // Active calls Start and Stop as appropriate
    property Active: Boolean read FActive write SetActive default False;
  end;

  // TJvLabelNone implements no special behavior
  TJvLabelNone = class(TJvLabelBehavior)
  published
    property Active;
  end;

  // TJvLabelBlink implements a blinking behavior
  TJvLabelBlink = class(TJvLabelBehavior)
  private
    FDelay: Cardinal;
    FInterval: Cardinal;
    FTimer: TTimer;
    FToggled: Boolean;
    procedure SetDelay(const Value: Cardinal);
    procedure SetInterval(const Value: Cardinal);
    procedure DoTimerEvent(Sender: TObject);
  protected
    procedure Start; override;
    procedure Stop; override;
  public
    constructor Create(ALabel: TJvCustomBehaviorLabel); override;
  published
    property Active;
    // Delay specifies the initial delay before the blinking starts. Delay is specified in milliseconds.
    property Delay: Cardinal read FDelay write SetDelay default 100;
    // Interval specifies the number f milliseconds that elapses between "blinks"
    property Interval: Cardinal read FInterval write SetInterval default 400;
  end;

  // TJvLabelBounce implements a bouncing label
  // NOTE that to use this behavior, the labels Align property should be set to alNone
  TJvLabelBounce = class(TJvLabelBehavior)
  private
    FOriginalRect: TRect;
    FInterval: Cardinal;
    FParent: TWinControl;
    FDirection: Integer;
    FTimer: TTimer;
    FPixels: Integer;
    procedure SetInterval(const Value: Cardinal);
    procedure SetPixels(const Value: Integer);
    procedure DoTimerEvent(Sender: TObject);
  protected
    procedure Start; override;
    procedure Stop; override;
  public
    constructor Create(ALabel: TJvCustomBehaviorLabel); override;
  published
    property Active;
    // Interval specifies the number of milliseconds that elapses between "bounces"
    // Lower values will make the label move faster
    property Interval: Cardinal read FInterval write SetInterval default 20;
    // Pixels specifes the number of pixels the label is moved at each bounce.
    // Lower values will make the label move slower and smoother. Compensate by decreasing the value of Interval
    property Pixels: Integer read FPixels write SetPixels default 6;
  end;

  // TJvLabelScroll implements a scrolling behavior, a behavior where the text is scrolled horizontally
  // This is sometimes also referred to as a "marquee"
  TJvLabelScroll = class(TJvLabelBehavior)
  private
    FInterval: Cardinal;
    FDirection: TJvLabelScrollDirection;
    FTimer: TTimer;
    FPadding: Boolean;
    procedure SetDirection(const Value: TJvLabelScrollDirection);
    procedure SetInterval(const Value: Cardinal);
    procedure DoTimerEvent(Sender: TObject);
    procedure SetPadding(Value: Boolean);
  protected
    procedure Start; override;
    procedure Stop; override;
  public
    constructor Create(ALabel: TJvCustomBehaviorLabel); override;
  published
    property Active;
    // Set Padding to True to simulate the Caption being scrolled "around the Edge" of the
    // label. This property is implemented such that the text is right-padded with spaces
    property Padding: Boolean read FPadding write SetPadding default False;
    // Interval specifies the number of milliseconds that elapses between each scroll
    // A lower Interval increases the speed of the scroll
    property Interval: Cardinal read FInterval write SetInterval default 50;
    // Direction specifies the direction of the scroll. Possible values are
    // sdLeftToRight - the text is scrolled from left to right
    // sdRightToLeft - the text is scrolled from right to left
    property Direction: TJvLabelScrollDirection read FDirection write SetDirection default sdLeftToRight;
  end;

  // TJvLabelAppear implements a behavior where the label appears
  // from one edge, moves across the form and stops at the other edge
  // NOTE that to use this behavior, the labels Align property should be set to alNone
  TJvLabelAppear = class(TJvLabelBehavior)
  private
    FParent: TWinControl;
    FDelay: Cardinal;
    FInterval: Cardinal;
    FPixels: Integer;
    FAppearFrom: TJvAppearDirection;
    FTimer: TTimer;
    FOriginalRect: TRect;
    FFirst: Boolean;
    procedure SetDelay(const Value: Cardinal);
    procedure SetInterval(const Value: Cardinal);
    procedure DoTimerEvent(Sender: TObject);
  protected
    procedure Start; override;
    procedure Stop; override;
  public
    constructor Create(ALabel: TJvCustomBehaviorLabel); override;
  published
    property Active;
    // Delay sets the initial delay before the label starts moving
    property Delay: Cardinal read FDelay write SetDelay default 100;
    // Interval sets the number of milliseconds that elapses between each move of the label
    property Interval: Cardinal read FInterval write SetInterval default 20;
    // Pixels sets number of piels the label moves at each interval
    property Pixels: Integer read FPixels write FPixels default 3;
    // AppearFrom sets the edge from which the label appears. It also specifies the direction the label moves in
    // Possible values for AppearFrom are:
    // drFromLeft - label appears from the parents left edge and moves to the right edge where it stops
    // drFromRight - label appears from the parents right edge and moves to the left edge where it stops
    // drFromTop   - label appears from the parents top edge and moves to the bottom edge where it stops
    // drFromBottom - label appears from the parents bottom edge and moves to the top edge where it stops
    property AppearFrom: TJvAppearDirection read FAppearFrom write FAppearFrom default drFromRight;
  end;

  // TJvLabelTyping implements a behavior where the label's original Caption is typed
  // into the label character by character
  TJvLabelTyping = class(TJvLabelBehavior)
  private
    FMakeErrors: Boolean;
    FInterval: Cardinal;
    FTextPos: Integer;
    FTimer: TTimer;
    procedure SetInterval(const Value: Cardinal);
    procedure SetMakeErrors(const Value: Boolean);
    procedure DoTimerEvent(Sender: TObject);
  protected
    procedure Start; override;
    procedure Stop; override;
  public
    constructor Create(ALabel: TJvCustomBehaviorLabel); override;
  published
    property Active;
    // MakeErrors specifies whether the typing sometimes contains errors. Errors are
    // removed after a short delay and the correct characters are "typed" instead.
    property MakeErrors: Boolean read FMakeErrors write SetMakeErrors default True;
    // Interval sets the speed of the typing in milliseconds
    property Interval: Cardinal read FInterval write SetInterval default 100;
  end;

  // TJvLabelSpecial implements a behavior where each character of the Caption is
  // started at #32 (space) and automatically incremented up to it's final value.
  // When the final value is reached, the next character of the original Caption is
  // added and incremented. This proceeds until the entire original Caption is shown in the label.
  TJvLabelSpecial = class(TJvLabelBehavior)
  private
    FInterval: Cardinal;
    FTextPos: Integer;
    FCharValue: Integer;
    FTimer: TTimer;
    procedure SetInterval(const Value: Cardinal);
    procedure DoTimerEvent(Sender: TObject);
  protected
    procedure Start; override;
    procedure Stop; override;
  public
    constructor Create(ALabel: TJvCustomBehaviorLabel); override;
  published
    property Active;
    // Interval sets the number of milliseconds that elapses between increments
    property Interval: Cardinal read FInterval write SetInterval default 20;
  end;

  // TJvLabelCodeBreaker "decodes" the text in the label to the
  // text in DecodedText. Interval sets the number of milliseconds between
  // "decode attempts", i.e character changes
  TJvLabelCodeBreaker = class(TJvLabelBehavior)
  private
    FScratchPad: TCaption;
    FDecodedText: TCaption;
    FInterval: Integer;
    FCurrentPos: Integer;
    FTimer: TTimer;
    procedure SetInterval(const Value: Integer);
    procedure DoTimer(Sender: TObject);
  protected
    procedure Start; override;
    procedure Stop; override;
  public
    constructor Create(ALabel: TJvCustomBehaviorLabel); override;
  published
    property DecodedText: TCaption read FDecodedText write FDecodedText;
    property Interval: Integer read FInterval write SetInterval default 10;
  end;

  TJvLabelBehaviorOptionsClass = class of TJvLabelBehavior;

  TJvCustomBehaviorLabel = class(TJvExCustomLabel)
  private
    FBehavior: TJvLabelBehaviorName;
    FOptions: TJvLabelBehavior;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FUseEffectText: Boolean;
    FEffectText: TCaption;
    function GetOptions: TJvLabelBehavior;
    function BehaviorStored: Boolean;
    procedure UpdateDesigner;
    procedure SetBehavior(const Value: TJvLabelBehaviorName);
    procedure SetOptions(const Value: TJvLabelBehavior);
    procedure SetUseEffectText(const Value: Boolean);
  protected
    procedure Resize; override;
    procedure DoStart; dynamic;
    procedure DoStop; dynamic;  
    function GetText: TCaption; override; 
    property Behavior: TJvLabelBehaviorName read FBehavior write SetBehavior stored BehaviorStored;
    property Caption;
    property BehaviorOptions: TJvLabelBehavior read GetOptions write SetOptions;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    // do not make these published
    property EffectText: TCaption read FEffectText write FEffectText;
    property UseEffectText: Boolean read FUseEffectText write SetUseEffectText;
  end;

  TJvBehaviorLabel = class(TJvCustomBehaviorLabel)
  published 
    property Behavior;
    property BehaviorOptions;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property OnStart;
    property OnStop;

    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property Caption;
    property Color;
    property Constraints;
    property OnEndDrag; 
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  // register a new behaviour
procedure RegisterLabelBehaviorOptions(const Name: TJvLabelBehaviorName; BehaviorOptionsClass:
  TJvLabelBehaviorOptionsClass);
// returns the class of the behaviour named Name or TJvLabelBehavior if Name not registered
function GetLabelBehaviorOptionsClass(const Name: TJvLabelBehaviorName): TJvLabelBehaviorOptionsClass;
// returns the registered name of BehaviorOptionsClass or an empty string if BehaviorOptionsClass is not registered
function GetLabelBehaviorName(BehaviorOptionsClass: TJvLabelBehaviorOptionsClass): string;
// Copies the internal TStrings list to Strings where each Strings[] is the name of a
// registered class and each Objects[] is a pointer to the corresponding class
procedure GetRegisteredLabelBehaviorOptions(Strings: TStrings);

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, QForms,
  JvQTypes, JvQResources, JvQFinalize;

const
  sUnitName = 'JvBehaviorLabel';

var
  AllBehaviorOptions: TStringList = nil;

function GetLabelBehaviorOptionsClass(const Name: TJvLabelBehaviorName): TJvLabelBehaviorOptionsClass;
var
  I: Integer;
begin
  Result := TJvLabelBehavior;
  if AllBehaviorOptions <> nil then
  begin
    I := AllBehaviorOptions.IndexOf(Name);
    if I >= 0 then
      Result := TJvLabelBehaviorOptionsClass(AllBehaviorOptions.Objects[I]);
  end;
end;

function GetLabelBehaviorName(BehaviorOptionsClass: TJvLabelBehaviorOptionsClass): string;
var
  I: Integer;
begin
  Result := '';
  if AllBehaviorOptions <> nil then
  begin
    I := AllBehaviorOptions.IndexOfObject(TObject(BehaviorOptionsClass));
    if I >= 0 then
      Result := AllBehaviorOptions[I];
  end;
end;

procedure GetRegisteredLabelBehaviorOptions(Strings: TStrings);
begin
  if Strings <> nil then
    Strings.Assign(AllBehaviorOptions);
end;

procedure RegisterLabelBehaviorOptions(const Name: TJvLabelBehaviorName;
  BehaviorOptionsClass: TJvLabelBehaviorOptionsClass);
begin
  if AllBehaviorOptions = nil then
  begin
    AllBehaviorOptions := TStringList.Create;
    AddFinalizeObjectNil(sUnitName, TObject(AllBehaviorOptions));
    AllBehaviorOptions.Sorted := True;
  end;
  if AllBehaviorOptions.IndexOf(Name) >= 0 then
    Exit;
  //    raise Exception.CreateFmt('Options %s already registered!',[Name]); // can't raise here: we are probably in an initialization section
  AllBehaviorOptions.AddObject(Name, TObject(BehaviorOptionsClass));
end;

//=== { TJvLabelBehavior } ===================================================

constructor TJvLabelBehavior.Create(ALabel: TJvCustomBehaviorLabel);
begin
  if ALabel = nil then
    raise EJVCLException.CreateResFmt(@RsENeedBehaviorLabel, [ClassName]);
  inherited Create;
  FLabel := ALabel;
  FActive := False;
end;

destructor TJvLabelBehavior.Destroy;
begin
  FTemporary := True;
  Stop;
  inherited Destroy;
end;

procedure TJvLabelBehavior.OwnerResize;
begin
  //
end;

procedure TJvLabelBehavior.Resume;
begin
  Active := FTmpActive;
  FTemporary := False;
end;

procedure TJvLabelBehavior.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if FActive then
      Stop;
    FActive := Value;
    if FActive then
      Start;
  end;
end;

procedure TJvLabelBehavior.Start;
begin
  if not FTemporary then
    OwnerLabel.DoStart;
end;

procedure TJvLabelBehavior.Stop;
begin
  if not FTemporary then
    OwnerLabel.DoStop;
end;

procedure TJvLabelBehavior.Suspend;
begin
  FTmpActive := Active;
  FTemporary := True;
  Active := False;
end;

//=== { TJvCustomBehaviorLabel } =============================================

constructor TJvCustomBehaviorLabel.Create(AComponent: TComponent);
begin
 // registration
  if not Assigned(AllBehaviorOptions) then
  begin
    RegisterLabelBehaviorOptions(RsNoneCaption, TJvLabelNone);
    RegisterLabelBehaviorOptions('Blinking', TJvLabelBlink);
    RegisterLabelBehaviorOptions('Bouncing', TJvLabelBounce);
    RegisterLabelBehaviorOptions('Scrolling', TJvLabelScroll);
    RegisterLabelBehaviorOptions('Typing', TJvLabelTyping);
    RegisterLabelBehaviorOptions('Appearing', TJvLabelAppear);
    RegisterLabelBehaviorOptions('Special', TJvLabelSpecial);
    RegisterLabelBehaviorOptions('CodeBreaker', TJvLabelCodeBreaker);
  end;

  inherited Create(AComponent);
  FBehavior := RsNoneCaption;
  FUseEffectText := False;
  FEffectText := '';
end;

destructor TJvCustomBehaviorLabel.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TJvCustomBehaviorLabel.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TJvCustomBehaviorLabel.DoStop;
begin
  if Assigned(FOnStop) then
    FOnStop(Self);
end;




function TJvCustomBehaviorLabel.GetText: TCaption;
begin
  if UseEffectText then
    Result := EffectText
  else
    Result := inherited GetText;
end;


function TJvCustomBehaviorLabel.BehaviorStored: Boolean;
begin
  Result := FBehavior <> RsNoneCaption;
end;

function TJvCustomBehaviorLabel.GetOptions: TJvLabelBehavior;
begin
  if FOptions = nil then
  begin
    // (p3) this doesn't update Options in the OI at DT (unless you collapse/expand the property)
    FOptions := GetLabelBehaviorOptionsClass(FBehavior).Create(Self);
    UpdateDesigner;
  end;
  Result := FOptions;
end;

procedure TJvCustomBehaviorLabel.Resize;
begin
  inherited Resize;
  BehaviorOptions.OwnerResize;
end;

procedure TJvCustomBehaviorLabel.SetBehavior(const Value: TJvLabelBehaviorName);
var
  S: TStringList;
begin
  if FBehavior <> Value then
  begin
    S := TStringList.Create;
    try
      GetRegisteredLabelBehaviorOptions(S);
      if S.IndexOf(Value) < 0 then
        Exit;
    finally
      S.Free;
    end;
    // (p3) this doesn't update Options in the OI at DT (unless you collapse/expand the property)
    FBehavior := Value;
    FreeAndNil(FOptions);
    UpdateDesigner;
  end;
end;

procedure TJvCustomBehaviorLabel.SetOptions(const Value: TJvLabelBehavior);
begin
  if Value = nil then
    Behavior := ''
  else
  if (FOptions = nil) or (FOptions.ClassType <> Value.ClassType) then
    Behavior := GetLabelBehaviorName(TJvLabelBehaviorOptionsClass(Value.ClassType));
  UpdateDesigner;
end;

procedure TJvCustomBehaviorLabel.SetUseEffectText(const Value: Boolean);
begin
  if Value <> FUseEffectText then
  begin
    FUseEffectText := Value;
    if ComponentState * [csLoading, csDestroying] = [] then
      Repaint;
  end;
end;

procedure TJvCustomBehaviorLabel.UpdateDesigner;
var
  F: TCustomForm;
begin
  if csDesigning in ComponentState then
  begin
    F := GetParentForm(Self);  
    if (F <> nil) and (F.DesignerHook <> nil) then
      F.DesignerHook.Modified; 
  end;
end;

//=== { TJvLabelBlink } ======================================================

constructor TJvLabelBlink.Create(ALabel: TJvCustomBehaviorLabel);
begin
  inherited Create(ALabel);
  ALabel.EffectText := '';
  FDelay := 100;
  FInterval := 400;
end;

procedure TJvLabelBlink.DoTimerEvent(Sender: TObject);
begin
  FTimer.Enabled := False;
  FTimer.Interval := FInterval;
  FToggled := not FToggled;
  OwnerLabel.UseEffectText := FToggled;
  FTimer.Enabled := FInterval > 0;
end;

procedure TJvLabelBlink.SetDelay(const Value: Cardinal);
begin
  if FDelay <> Value then
  begin
    Suspend;
    FDelay := Value;
    Resume;
  end;
end;

procedure TJvLabelBlink.SetInterval(const Value: Cardinal);
begin
  if FInterval <> Value then
  begin
    Suspend;
    FInterval := Value;
    Resume;
  end;
end;

procedure TJvLabelBlink.Start;
begin
  inherited Start;
  if OwnerLabel.ComponentState * [csLoading, csDestroying] <> [] then
    Exit;
  if FTimer = nil then
  begin
    FTimer := TTimer.Create(nil);
    FTimer.Enabled := False;
    FTimer.OnTimer := DoTimerEvent;
  end;
  FTimer.Interval := FDelay;
  FToggled := False;
  if FDelay = 0 then
    FDelay := 1;
  FTimer.Enabled := True; // not (csDesigning in OwnerLabel.ComponentState);
end;

procedure TJvLabelBlink.Stop;
begin
  if FTimer <> nil then
  begin
    FreeAndNil(FTimer);
    OwnerLabel.UseEffectText := False;
  end;
  inherited Stop;
end;

//=== { TJvLabelBounce } =====================================================

constructor TJvLabelBounce.Create(ALabel: TJvCustomBehaviorLabel);
begin
  inherited Create(ALabel);
  FInterval := 20;
  FPixels := 6;
end;

procedure TJvLabelBounce.DoTimerEvent(Sender: TObject);
begin
  FTimer.Enabled := False;
  if Pixels = 0 then
    Pixels := Random(8);
  with OwnerLabel do
    case FDirection of
      0:
        if (Left - Pixels <= 0) or (Top + Height + Pixels >= FParent.ClientHeight) then
        begin
          FDirection := Random(4);
          //            Pixels := Random(8);
        end
        else
        begin
          Left := Left - Pixels;
          Top := Top + Pixels;
        end;
      1:
        if (Top + Height + Pixels >= FParent.ClientHeight) or
          (Left + Width + Pixels >= FParent.ClientWidth) then
        begin
          FDirection := Random(4);
          //            Pixels := Random(8);
        end
        else
        begin
          Top := Top + Pixels;
          Left := Left + Pixels;
        end;
      2:
        if (Left - Pixels <= 0) or (Top - Pixels <= 0) then
        begin
          FDirection := Random(4);
          //            Pixels := Random(8);
        end
        else
        begin
          Left := Left - Pixels;
          Top := Top - Pixels;
        end;
      3:
        if (Left + Width + Pixels > FParent.ClientWidth) or (Top - Pixels <= 0) then
        begin
          FDirection := Random(4);
          //            Pixels := Random(8);
        end
        else
        begin
          Left := Left + Pixels;
          Top := Top - Pixels;
        end;
    end;
  FTimer.Enabled := True;
end;

procedure TJvLabelBounce.SetInterval(const Value: Cardinal);
begin
  if FInterval <> Value then
  begin
    Suspend;
    FInterval := Value;
    if FInterval <= 0 then
      FInterval := 20;
    Resume;
  end;
end;

procedure TJvLabelBounce.SetPixels(const Value: Integer);
begin
  if FPixels <> Value then
  begin
    Suspend;
    FPixels := Value;
    if FPixels <= 0 then
      FPixels := 6;
    Resume;
  end;
end;

procedure TJvLabelBounce.Start;
begin
  if OwnerLabel.ComponentState * [csLoading, csDestroying] <> [] then
    Exit;
  FParent := OwnerLabel.Parent;
  if FParent = nil then
    raise EJVCLException.CreateResFmt(@RsENoOwnerLabelParent, [ClassName]);
  inherited Start;
  FOriginalRect := OwnerLabel.BoundsRect;
  Randomize;
  if FTimer = nil then
  begin
    FTimer := TTimer.Create(nil);
    FTimer.Enabled := False;
    FTimer.OnTimer := DoTimerEvent;
  end;
  FTimer.Interval := Interval;
  FTimer.Enabled := True;
end;

procedure TJvLabelBounce.Stop;
begin
  FreeAndNil(FTimer);
  if not IsRectEmpty(FOriginalRect) then
    OwnerLabel.BoundsRect := FOriginalRect;
  inherited Stop;
end;

//=== { TJvLabelScroll } =====================================================

constructor TJvLabelScroll.Create(ALabel: TJvCustomBehaviorLabel);
begin
  inherited Create(ALabel);
  FInterval := 50;
  FDirection := sdLeftToRight;
end;

procedure TJvLabelScroll.DoTimerEvent(Sender: TObject);
var
  Tmp: TCaption;
begin
  FTimer.Enabled := False;
  if OwnerLabel.Caption <> '' then
  begin
    Tmp := OwnerLabel.EffectText;
    if FDirection = sdLeftToRight then
      Tmp := Tmp[Length(Tmp)] + Copy(Tmp, 1, Length(Tmp) - 1)
    else
      Tmp := Copy(Tmp, 2, Length(Tmp) - 1) + Tmp[1];
    OwnerLabel.EffectText := Tmp;
    OwnerLabel.Repaint;
  end;
  FTimer.Enabled := True;
end;

procedure TJvLabelScroll.SetDirection(const Value: TJvLabelScrollDirection);
begin
  if FDirection <> Value then
  begin
    Suspend;
    FDirection := Value;
    Resume;
  end;
end;

procedure TJvLabelScroll.SetInterval(const Value: Cardinal);
begin
  if FInterval <> Value then
  begin
    Suspend;
    FInterval := Value;
    Resume;
  end;
end;

procedure TJvLabelScroll.SetPadding(Value: Boolean);
var
  Tmp: TCaption;
begin
  FPadding := Value;
  Tmp := '';
  while OwnerLabel.Canvas.TextWidth(Tmp) < OwnerLabel.Width do
    Tmp := Tmp + ' ';
  if Value then
    OwnerLabel.EffectText := OwnerLabel.Caption + Tmp
  else
    OwnerLabel.EffectText := OwnerLabel.Caption;
end;

procedure TJvLabelScroll.Start;
begin
  inherited Start;
  if OwnerLabel.ComponentState * [csLoading, csDestroying] <> [] then
    Exit;
  if FTimer = nil then
  begin
    FTimer := TTimer.Create(nil);
    FTimer.Enabled := False;
    FTimer.OnTimer := DoTimerEvent;
  end;
  FTimer.Interval := Interval;
  SetPadding(Padding);
  OwnerLabel.UseEffectText := True;
  FTimer.Enabled := True;
end;

procedure TJvLabelScroll.Stop;
begin
  FreeAndNil(FTimer);
  OwnerLabel.UseEffectText := False;
  inherited Stop;
end;

//=== { TJvLabelAppear } =====================================================

constructor TJvLabelAppear.Create(ALabel: TJvCustomBehaviorLabel);
begin
  inherited Create(ALabel);
  FDelay := 100;
  FInterval := 20;
  FPixels := 3;
  FAppearFrom := drFromRight;
end;

procedure TJvLabelAppear.DoTimerEvent(Sender: TObject);
var
  FWidth, FHeight: Integer;
  FSuspend: Boolean;
begin
  FWidth := FOriginalRect.Right - FOriginalRect.Left;
  FHeight := FOriginalRect.Bottom - FOriginalRect.Top;
  FSuspend := False;
  if FFirst then
  begin
    case FAppearFrom of
      drFromRight:
        begin
          OwnerLabel.Left := FParent.ClientWidth;
          OwnerLabel.Width := 0;
        end;
      drFromLeft:
        OwnerLabel.Left := -OwnerLabel.Width;
      drFromTop:
        OwnerLabel.Top := -OwnerLabel.Height;
      drFromBottom:
        begin
          OwnerLabel.Top := FParent.ClientHeight;
          OwnerLabel.Height := 0;
        end;
    end;
    OwnerLabel.Visible := True;
    FFirst := False;
  end;

  case FAppearFrom of
    drFromRight:
      begin
        if Abs(OwnerLabel.Left - FOriginalRect.Left) < Pixels then
        begin
          OwnerLabel.Left := FOriginalRect.Left;
          FSuspend := True;
        end
        else
          OwnerLabel.Left := OwnerLabel.Left - Pixels;
        if OwnerLabel.Width <> FWidth then
        begin
          if OwnerLabel.Left + FWidth < FParent.ClientWidth then
            OwnerLabel.Width := FWidth
          else
            OwnerLabel.Width := FParent.ClientWidth - OwnerLabel.Left - 2;
        end;
      end;
    drFromLeft:
      if Abs(OwnerLabel.Left - FOriginalRect.Left) < Pixels then
      begin
        OwnerLabel.Left := FOriginalRect.Left;
        FSuspend := True;
      end
      else
        OwnerLabel.Left := OwnerLabel.Left + Pixels;
    drFromTop:
      if Abs(OwnerLabel.Top - FOriginalRect.Top) < Pixels then
      begin
        OwnerLabel.Top := FOriginalRect.Top;
        FSuspend := True;
      end
      else
        OwnerLabel.Top := OwnerLabel.Top + Pixels;
    drFromBottom:
      begin
        if Abs(OwnerLabel.Top - FOriginalRect.Top) < Pixels then
        begin
          OwnerLabel.Top := FOriginalRect.Top;
          FSuspend := True;
        end
        else
          OwnerLabel.Top := OwnerLabel.Top - Pixels;
        if OwnerLabel.Height <> FHeight then
        begin
          if OwnerLabel.Top + FHeight < FParent.ClientHeight then
            OwnerLabel.Height := FHeight
          else
            OwnerLabel.Height := FParent.ClientHeight - OwnerLabel.Top - 2;
        end;
      end;
  end;
  FTimer.Interval := Interval;
  if FSuspend then
    Active := False
  else
    FTimer.Enabled := True;
end;

procedure TJvLabelAppear.SetDelay(const Value: Cardinal);
begin
  if FDelay <> Value then
  begin
    Suspend;
    FDelay := Value;
    Resume;
  end;
end;

procedure TJvLabelAppear.SetInterval(const Value: Cardinal);
begin
  if FInterval <> Value then
  begin
    Suspend;
    FInterval := Value;
    Resume;
  end;
end;

procedure TJvLabelAppear.Start;
begin
  if OwnerLabel.ComponentState * [csLoading, csDestroying] <> [] then
    Exit;
  FParent := OwnerLabel.Parent;
  if FParent = nil then
    raise EJVCLException.CreateResFmt(@RsENoOwnerLabelParent, [ClassName]);
  inherited Start;
  if FTimer = nil then
  begin
    FTimer := TTimer.Create(nil);
    FTimer.Enabled := False;
    FTimer.OnTimer := DoTimerEvent;
  end;
  FTimer.Interval := Delay;
  FFirst := True;
  FOriginalRect := OwnerLabel.BoundsRect;
  FTimer.Enabled := True;
end;

procedure TJvLabelAppear.Stop;
begin
  FreeAndNil(FTimer);
  if not IsRectEmpty(FOriginalRect) then
    OwnerLabel.BoundsRect := FOriginalRect;
  inherited Stop;
end;

//=== { TJvLabelTyping } =====================================================

constructor TJvLabelTyping.Create(ALabel: TJvCustomBehaviorLabel);
begin
  inherited Create(ALabel);
  FInterval := 100;
  FMakeErrors := True;
end;

procedure TJvLabelTyping.DoTimerEvent(Sender: TObject);
var
  Tmp: TCaption;
  I: Integer;
begin
  FTimer.Enabled := False;
  if FTextPos <= Length(OwnerLabel.Caption) then
  begin
    Tmp := Copy(OwnerLabel.Caption, 1, FTextPos - 1);
    I := Random(10);
    if (I = 7) and MakeErrors then
      Tmp := Tmp + Char(Ord(OwnerLabel.Caption[FTextPos]) - Random(10))
    else
      Tmp := Tmp + OwnerLabel.Caption[FTextPos];
    if (MakeErrors) and (I <> 7) then
      FTimer.Interval := Interval
    else
      FTimer.Interval := Interval * 2;
    OwnerLabel.EffectText := Tmp;
    OwnerLabel.Repaint;
    Inc(FTextPos);
    FTimer.Enabled := True;
  end
  else
    Active := False;
end;

procedure TJvLabelTyping.SetInterval(const Value: Cardinal);
begin
  if FInterval <> Value then
  begin
    Suspend;
    FInterval := Value;
    Resume;
  end;
end;

procedure TJvLabelTyping.SetMakeErrors(const Value: Boolean);
begin
  if FMakeErrors <> Value then
  begin
    Suspend;
    FMakeErrors := Value;
    Resume;
  end;
end;

procedure TJvLabelTyping.Start;
begin
  inherited Start;
  if OwnerLabel.ComponentState * [csLoading, csDestroying] <> [] then
    Exit;
  if FTimer = nil then
  begin
    FTimer := TTimer.Create(nil);
    FTimer.Enabled := False;
    FTimer.OnTimer := DoTimerEvent;
  end;
  FTimer.Interval := Interval;
  Randomize;
  OwnerLabel.EffectText := '';
  OwnerLabel.UseEffectText := True;
  FTextPos := 1;
  FTimer.Enabled := True;
end;

procedure TJvLabelTyping.Stop;
begin
  FreeAndNil(FTimer);
  OwnerLabel.UseEffectText := False;
  inherited Stop;
end;

//=== { TJvLabelSpecial } ====================================================

constructor TJvLabelSpecial.Create(ALabel: TJvCustomBehaviorLabel);
begin
  inherited Create(ALabel);
  FInterval := 20;
end;

procedure TJvLabelSpecial.DoTimerEvent(Sender: TObject);
begin
  FTimer.Enabled := False;
  if FTextPos < Length(OwnerLabel.Caption) then
  begin
    if FCharValue > Ord(OwnerLabel.Caption[FTextPos]) then
    begin
      Inc(FTextPos);
      FCharValue := 32;
    end;
    OwnerLabel.EffectText := Copy(OwnerLabel.Caption, 1, FTextPos) + Char(FCharValue);
    OwnerLabel.Repaint;
    Inc(FCharValue);
    FTimer.Enabled := True;
  end
  else
    Active := False;
end;

procedure TJvLabelSpecial.SetInterval(const Value: Cardinal);
begin
  if FInterval <> Value then
  begin
    Suspend;
    FInterval := Value;
    Resume;
  end;
end;

procedure TJvLabelSpecial.Start;
begin
  inherited Start;
  if OwnerLabel.ComponentState * [csLoading, csDestroying] <> [] then
    Exit;
  if FTimer = nil then
  begin
    FTimer := TTimer.Create(nil);
    FTimer.Enabled := False;
    FTimer.OnTimer := DoTimerEvent;
  end;
  FTextPos := 1;
  FCharValue := 32;
  OwnerLabel.EffectText := '';
  OwnerLabel.UseEffectText := True;
  FTimer.Interval := Interval;
  FTimer.Enabled := True;
end;

procedure TJvLabelSpecial.Stop;
begin
  FreeAndNil(FTimer);
  OwnerLabel.UseEffectText := False;
  inherited Stop;
end;

//=== { TJvLabelCodeBreaker } ================================================

constructor TJvLabelCodeBreaker.Create(ALabel: TJvCustomBehaviorLabel);
begin
  inherited Create(ALabel);
  FInterval := 10;
end;

procedure TJvLabelCodeBreaker.DoTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  if (FCurrentPos > Length(FScratchPad)) or (FCurrentPos > Length(DecodedText)) then
  begin
    OwnerLabel.EffectText := DecodedText;
    OwnerLabel.Repaint;
    Active := False;
    OwnerLabel.UseEffectText := False;
    Exit;
  end
  else
  if FScratchPad[FCurrentPos] <> DecodedText[FCurrentPos] then
  begin  
    FScratchPad[FCurrentPos] := WideChar(32 + Random(Ord(DecodedText[FCurrentPos]) + 10)); 
    //    OwnerLabel.EffectText := Copy(OwnerLabel.Caption, 1, FCurrentPos - 1) +
    //      FScratchPad[FCurrentPos] + Copy(OwnerLabel.Caption, FCurrentPos + 1, MaxInt);
        // (p3) this is the same without the copying...
    OwnerLabel.EffectText := FScratchPad;
    OwnerLabel.Repaint;
  end
  else
    Inc(FCurrentPos);
  // (p3) this seems unnecessary since we have an Interval property
//  Sleep(FInterval);
  FTimer.Enabled := True;
end;

procedure TJvLabelCodeBreaker.SetInterval(const Value: Integer);
begin
  if FInterval <> Value then
  begin
    Suspend;
    FInterval := Value;
    Resume;
  end;
end;

procedure TJvLabelCodeBreaker.Start;
begin
  inherited Start;
  FCurrentPos := 1;
  if (Interval > 0) and (OwnerLabel.Caption <> '') and (DecodedText <> '') then
  begin
    FScratchPad := OwnerLabel.Caption;
    FTimer := TTimer.Create(nil);
    FTimer.Enabled := False;
    FTimer.OnTimer := DoTimer;
    FTimer.Interval := Interval;
    FTimer.Enabled := True;
    OwnerLabel.UseEffectText := True;
  end
  else
    Active := False;
end;

procedure TJvLabelCodeBreaker.Stop;
begin
  FreeAndNil(FTimer);
  OwnerLabel.Caption := OwnerLabel.EffectText;
  OwnerLabel.UseEffectText := False;
  inherited Stop;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

 // (ahuser) registration is done in the constructor the first time it is called

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  FinalizeUnit(sUnitName);

end.

