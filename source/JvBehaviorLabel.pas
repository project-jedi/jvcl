{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBehaviorLabel.PAS, released on 2003-03-24.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):
Sébastien Buysse [sbuysse@buypin.com] - original author of the merged components
Michael Beck [mbeck@bigfoot.com].

Last Modified: 2003-03-24

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

{$I JVCL.INC}
unit JvBehaviorLabel;

interface
uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, StdCtrls, ExtCtrls, JVCLVer;

type
  TJvCustomBehaviorLabel = class; // forward
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

    FTmpActive, FActive, FTemporary: boolean;
    procedure SetActive(const Value: boolean);
  protected
    // Call Suspend to store the current state of the Active property and
    // set Active to false. If the behavior was already inactive, Suspend does nothing
    procedure Suspend;
    // Call Resume to set the Active property to the state it was in before calling Suspend.
    // Resume sets Active to true if it was true when Suspend was called.
    // If Active was false before calling Suspend, Resume does nothing
    procedure Resume;
    // OwnerResize is called when the OwnerLabel is resized. Override this
    // method to do special processing when the OwnerLabel changes it's size or position.
    // OwnerResize does nothing in this class
    procedure OwnerResize; virtual;
    // Start is automatically called when Active is set to true
    // Override this method to take special action when the behavior is "started".
    // Start does nothing in this class
    procedure Start; virtual;
    // Stop is automatically called when Active is set to true
    // Override this method to take special action when the behavior is "stopped".
    // Stop does nothing in this class
    procedure Stop; virtual;
    // The label that the behavior is acting upon
    property OwnerLabel: TJvCustomBehaviorLabel read FLabel;
  public
    constructor Create(ALabel: TJvCustomBehaviorLabel); virtual;
    destructor Destroy; override;
  published
    // Set Active to true to enable the behavior and set it to false to disable it.
    // Active calls Start and Stop as appropriate
    property Active: boolean read FActive write SetActive default false;
  end;

  // TJvLabelBlink implements a blinking behavior
  TJvLabelBlink = class(TJvLabelBehavior)
  private
    FDelay: Cardinal;
    FInterval: Cardinal;
    FTimer: TTimer;
    FToggled: boolean;
    FOldCaption: TCaption;
    procedure SetDelay(const Value: Cardinal);
    procedure SetInterval(const Value: Cardinal);
    procedure DoTimerEvent(Sender: Tobject);
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
    FPixels: integer;
    procedure SetInterval(const Value: Cardinal);
    procedure SetPixels(const Value: integer);
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
    property Pixels: integer read FPixels write SetPixels default 6;
  end;

  // TJvLabelScroll implements a scrolling behavior, a behavior where the text is scrolled horizontally
  // This is sometimes also referred to as a "marquee"
  TJvLabelScroll = class(TJvLabelBehavior)
  private
    FInterval: Cardinal;
    FDirection: TJvLabelScrollDirection;
    FOriginalText: string;
    FTimer: TTimer;
    FPad: boolean;
    procedure SetDirection(const Value: TJvLabelScrollDirection);
    procedure SetInterval(const Value: Cardinal);
    procedure DoTimerEvent(Sender: TObject);
    procedure DoPadding(Value: boolean);
  protected
    procedure Start; override;
    procedure Stop; override;
  public
    constructor Create(ALabel: TJvCustomBehaviorLabel); override;
  published
    property Active;
    // Set Padding to true to simulate the Caption being scrolled "around the Edge" of the
    // label. This propert yis implemented such that the text is right-padded with spaces
    property Padding: boolean read FPad write FPad default false;
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
    FFirst: boolean;
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
    FMakeErrors: boolean;
    FInterval: Cardinal;
    FTextPos: integer;
    FTimer: TTimer;
    FOriginalText: TCaption;
    procedure SetInterval(const Value: Cardinal);
    procedure SetMakeErrors(const Value: boolean);
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
    property MakeErrors: boolean read FMakeErrors write SetMakeErrors default True;
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
    FTextPos, FCharValue: integer;
    FTimer: TTimer;
    FOriginalText: string;
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
  { TJvLabelCodeBreaker "decodes" the text in the label to the
    text in DecodedText. Interval sets the number of milliseconds between
    "decode attempts", i.e character changes }

  TJvLabelCodeBreaker = class(TJvLabelBehavior)
  private
    FScratchPad, FOriginal, FDecodedText: string;
    FInterval, FCurrentPos: integer;
    FTimer: TTimer;
    procedure SetInterval(const Value: integer);
    procedure DoTimer(Sender: TObject);
  protected
    procedure Start; override;
    procedure Stop; override;
  public
    constructor Create(ALabel: TJvCustomBehaviorLabel); override;
  published
    property DecodedText: string read FDecodedText write FDecodedText;
    property Interval: integer read FInterval write SetInterval default 10;
  end;

  TJvLabelBehaviorOptionsClass = class of TJvLabelBehavior;

  TJvCustomBehaviorLabel = class(TCustomLabel)
  private
    FBehavior: TJvLabelBehaviorName;
    FOptions: TJvLabelBehavior;
    FOnCtl3DChanged: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FAboutJVCL: TJVCLAboutInfo;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    function GetOptions: TJvLabelBehavior;
    procedure UpdateDesigner;
    procedure SetBehavior(const Value: TJvLabelBehaviorName);
    procedure SetOptions(const Value: TJvLabelBehavior);
    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
  protected
    procedure Resize; override;
    procedure DoStart;dynamic;
    procedure DoStop;dynamic;
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
  protected
    property Behavior: TJvLabelBehaviorName read FBehavior write SetBehavior;
    property Caption;
    property BehaviorOptions: TJvLabelBehavior read GetOptions write SetOptions;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnStart:TNotifyEvent read FOnStart write FOnStart;
    property OnStop:TNotifyEvent read FOnStop write FOnStop;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvBehaviorLabel = class(TJvCustomBehaviorLabel)
  published
    property Behavior;
    property BehaviorOptions;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnCtl3DChanged;
    property OnParentColorChange;
    property OnStart;
    property OnStop;

    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentBiDiMode;
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
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

  end;

resourcestring
  SNone = '(none)';

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
  Forms;

var
  FBehaviorOptions: TStringlist = nil;

function GetLabelBehaviorOptionsClass(const Name: TJvLabelBehaviorName): TJvLabelBehaviorOptionsClass;
var
  i: integer;
begin
  Result := TJvLabelBehavior;

  if (FBehaviorOptions <> nil) then
  begin
    i := FBehaviorOptions.IndexOf(Name);
    if i >= 0 then
      Result := TJvLabelBehaviorOptionsClass(FBehaviorOptions.Objects[i])
  end;
end;

function GetLabelBehaviorName(BehaviorOptionsClass: TJvLabelBehaviorOptionsClass): string;
var
  i: integer;
begin
  Result := '';
  if (FBehaviorOptions <> nil) then
  begin
    i := FBehaviorOptions.IndexOfObject(TObject(BehaviorOptionsClass));
    if i >= 0 then
      Result := FBehaviorOptions[i];
  end;
end;

procedure GetRegisteredLabelBehaviorOptions(Strings: TStrings);
begin
  if Strings <> nil then
    Strings.Assign(FBehaviorOptions);
end;

procedure RegisterLabelBehaviorOptions(const Name: TJvLabelBehaviorName; BehaviorOptionsClass:
  TJvLabelBehaviorOptionsClass);
begin
  if FBehaviorOptions = nil then
  begin
    FBehaviorOptions := TStringlist.Create;
    FBehaviorOptions.Sorted := true;
  end;
  if FBehaviorOptions.IndexOf(Name) >= 0 then Exit;
  //    raise Exception.CreateFmt('Options %s already registered!',[Name]); // can't raise here: we are probably in an initialization section
  FBehaviorOptions.AddObject(Name, TObject(BehaviorOptionsClass));
end;

{ TJvLabelBehavior }

constructor TJvLabelBehavior.Create(ALabel: TJvCustomBehaviorLabel);
begin
  if ALabel = nil then
    raise Exception.Create('Cannot call TJvLabelBehavior.Create with ALabel = nil!');
  inherited Create;
  FLabel := ALabel;
  FActive := false;
end;

destructor TJvLabelBehavior.Destroy;
begin
  FTemporary := true;
  Stop;
  inherited;
end;

procedure TJvLabelBehavior.OwnerResize;
begin
  //
end;

procedure TJvLabelBehavior.Resume;
begin
  Active := FTmpActive;
  FTemporary := false;
end;

procedure TJvLabelBehavior.SetActive(const Value: boolean);
begin
  if FActive <> Value then
  begin
    if FActive then Stop;
    FActive := Value;
    if FActive then Start;
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
  FTemporary := true;
  Active := false;
end;

{ TJvCustomBehaviorLabel }

procedure TJvCustomBehaviorLabel.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvCustomBehaviorLabel.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

constructor TJvCustomBehaviorLabel.Create(AComponent: TComponent);
begin
  inherited;
  FBehavior := SNone;
end;

destructor TJvCustomBehaviorLabel.Destroy;
begin
  FreeAndNil(FOptions);
  inherited;
end;

procedure TJvCustomBehaviorLabel.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(self);
end;

procedure TJvCustomBehaviorLabel.DoStop;
begin
  if Assigned(FOnStop) then
    FOnStop(self);
end;

function TJvCustomBehaviorLabel.GetOptions: TJvLabelBehavior;
begin
  if FOptions = nil then
  begin
    // (p3) this doesn't update Options in the OI at DT (unless you collapse/expand the property)
    FOptions := GetLabelBehaviorOptionsClass(FBehavior).Create(self);
    UpdateDesigner;
  end;
  Result := FOptions;
end;

procedure TJvCustomBehaviorLabel.MouseEnter(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvCustomBehaviorLabel.MouseLeave(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvCustomBehaviorLabel.Resize;
begin
  inherited;
  BehaviorOptions.OwnerResize;
end;

procedure TJvCustomBehaviorLabel.SetBehavior(const Value: TJvLabelBehaviorName);
var
  S: TStringlist;
begin
  if FBehavior <> Value then
  begin
    S := TStringlist.Create;
    try
      GetRegisteredLabelBehaviorOptions(S);
      if S.IndexOf(Value) < 0 then Exit;
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
  else if (FOptions = nil) or (FOptions.ClassType <> Value.ClassType) then
    Behavior := GetLabelBehaviorName(TJvLabelBehaviorOptionsClass(Value.ClassType));
  UpdateDesigner;
end;

procedure TJvCustomBehaviorLabel.UpdateDesigner;
var
  F: TCustomForm;
begin
  if csDesigning in ComponentState then
  begin
    F := GetParentForm(self);
    if (F <> nil) and (F.Designer <> nil) then
      F.Designer.Modified;
  end;
end;

{ TJvLabelBlink }

constructor TJvLabelBlink.Create(ALabel: TJvCustomBehaviorLabel);
begin
  inherited;
  FDelay := 100;
  FInterval := 400;
end;

procedure TJvLabelBlink.DoTimerEvent(Sender: Tobject);
begin
  FTimer.Enabled := false;
  FTimer.Interval := FInterval;
  if FToggled then
    OwnerLabel.Caption := FOldCaption
  else
    OwnerLabel.Caption := '';
  FToggled := not FToggled;
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
  inherited;
  if (csLoading in OwnerLabel.ComponentState) then Exit;
  if FTimer = nil then
  begin
    FTimer := TTimer.Create(nil);
    FTimer.Enabled := false;
    FTimer.OnTimer := DoTimerEvent;
  end;
  FTimer.Interval := FDelay;
  FOldCaption := OwnerLabel.Caption;
  FToggled := false;
  if FDelay = 0 then FDelay := 1;
  FTimer.Enabled := true; // not (csDesigning in Ownerlabel.ComponentState);
end;

procedure TJvLabelBlink.Stop;
begin
  if FTimer <> nil then
  begin
    FreeAndNil(FTimer);
    if FToggled then
      OwnerLabel.Caption := FOldCaption;
  end;
  inherited;
end;

{ TJvLabelBounce }

constructor TJvLabelBounce.Create(ALabel: TJvCustomBehaviorLabel);
begin
  inherited;
  FInterval := 20;
  FPixels := 6;
end;

procedure TJvLabelBounce.DoTimerEvent(Sender: TObject);
begin
  FTimer.Enabled := false;
  if Pixels = 0 then
    Pixels := Random(8);
  with OwnerLabel do
    case FDirection of
      0:
        begin
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
        end;
      1:
        begin
          if (Top + Height + Pixels >= FParent.ClientHeight)
            or (Left + Width + Pixels >= FParent.ClientWidth) then
          begin
            FDirection := Random(4);
            //            Pixels := Random(8);
          end
          else
          begin
            Top := Top + Pixels;
            Left := Left + Pixels;
          end;
        end;
      2:
        begin
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
        end;
      3:
        begin
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
    end;
  FTimer.Enabled := true;
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

procedure TJvLabelBounce.SetPixels(const Value: integer);
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
  if (csLoading in OwnerLabel.ComponentState) then Exit;
  FParent := OwnerLabel.Parent;
  if FParent = nil then
    raise Exception.Create('OwnerLabel.Parent is nil in TJvLabelBounce.Start!');
  inherited;
  FOriginalRect := OwnerLabel.BoundsRect;
  Randomize;
  if FTimer = nil then
  begin
    FTimer := TTimer.Create(nil);
    FTimer.Enabled := false;
    FTimer.OnTimer := DoTimerEvent;
  end;
  FTimer.Interval := Interval;
  FTimer.Enabled := true;
end;

procedure TJvLabelBounce.Stop;
begin
  FreeAndNil(FTimer);
  if not IsRectEmpty(FOriginalRect) then
    OwnerLabel.BoundsRect := FOriginalRect;
  inherited;
end;

{ TJvLabelScroll }

constructor TJvLabelScroll.Create(ALabel: TJvCustomBehaviorLabel);
begin
  inherited;
  FInterval := 50;
  FDirection := sdLeftToRight;
end;

procedure TJvLabelScroll.DoTimerEvent(Sender: TObject);
var
  tmp: string;
begin
  FTimer.Enabled := false;
  if Length(OwnerLabel.Caption) > 0 then
  begin
    tmp := OwnerLabel.Caption;
    if FDirection = sdLeftToRight then
      tmp := tmp[Length(tmp)] + Copy(tmp, 1, Length(tmp) - 1)
    else
      tmp := Copy(tmp, 2, Length(tmp) - 1) + tmp[1];
    OwnerLabel.Caption := tmp;
  end;
  FTimer.Enabled := true;
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

procedure TJvLabelScroll.DoPadding(Value: boolean);
var
  tmp: string;
begin
  tmp := FOriginalText;
  if Value and not (csDestroying in OwnerLabel.ComponentState) then
    while OwnerLabel.Canvas.TextWidth(tmp) < OwnerLabel.Width do
      tmp := tmp + ' ';
  OwnerLabel.Caption := tmp;
end;

procedure TJvLabelScroll.Start;
begin
  inherited;
  if (csLoading in OwnerLabel.ComponentState) then Exit;
  if FTimer = nil then
  begin
    FTimer := TTimer.Create(nil);
    FTimer.Enabled := false;
    FTimer.OnTimer := DoTimerEvent;
  end;
  FTimer.Interval := Interval;
  FOriginalText := OwnerLabel.Caption;
  DoPadding(Padding);
  FTimer.Enabled := true;
end;

procedure TJvLabelScroll.Stop;
begin
  if FTimer <> nil then
  begin
    FreeAndNil(FTimer);
    OwnerLabel.Caption := FOriginalText;
    //    DoPadding(false);
  end;
  inherited;
end;

{ TJvLabelAppear }

constructor TJvLabelAppear.Create(ALabel: TJvCustomBehaviorLabel);
begin
  inherited;
  FDelay := 100;
  FInterval := 20;
  FPixels := 3;
  FAppearFrom := drFromRight;
end;

procedure TJvLabelAppear.DoTimerEvent(Sender: TObject);
var
  FWidth, FHeight: integer;
  FSuspend: boolean;
begin
  FWidth := FOriginalRect.Right - FOriginalRect.Left;
  FHeight := FOriginalRect.Bottom - FOriginalRect.Top;
  FSuspend := false;
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
    FFirst := false;
  end;

  case FAppearFrom of
    drFromRight:
      begin
        if Abs(OwnerLabel.Left - FOriginalRect.Left) < Pixels then
        begin
          OwnerLabel.Left := FOriginalRect.Left;
          FSuspend := true;
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
      begin
        if Abs(OwnerLabel.Left - FOriginalRect.Left) < Pixels then
        begin
          OwnerLabel.Left := FOriginalRect.Left;
          FSuspend := true;
        end
        else
          OwnerLabel.Left := OwnerLabel.Left + Pixels;
      end;
    drFromTop:
      begin
        if Abs(OwnerLabel.Top - FOriginalRect.Top) < Pixels then
        begin
          OwnerLabel.Top := FOriginalRect.Top;
          FSuspend := true;
        end
        else
          OwnerLabel.Top := OwnerLabel.Top + Pixels;
      end;
    drFromBottom:
      begin
        if Abs(OwnerLabel.Top - FOriginalRect.Top) < Pixels then
        begin
          OwnerLabel.Top := FOriginalRect.Top;
          FSuspend := true;
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
    Active := false
  else
    FTimer.Enabled := true;
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
  if (csLoading in OwnerLabel.ComponentState) then Exit;
  FParent := OwnerLabel.Parent;
  if FParent = nil then
    raise Exception.Create('OwnerLabel.Parent is nil in TJvLabelAppear.Start!');
  inherited;
  if FTimer = nil then
  begin
    FTimer := TTimer.Create(nil);
    FTimer.Enabled := false;
    FTimer.OnTimer := DoTimerEvent;
  end;
  FTimer.Interval := Delay;
  FFirst := true;
  FOriginalRect := OwnerLabel.BoundsRect;
  FTimer.Enabled := true;
end;

procedure TJvLabelAppear.Stop;
begin
  FreeAndNil(FTimer);
  if not IsRectEmpty(FOriginalRect) then
    OwnerLabel.BoundsRect := FOriginalRect;
  inherited;
end;

{ TJvLabelTyping }

constructor TJvLabelTyping.Create(ALabel: TJvCustomBehaviorLabel);
begin
  inherited;
  FInterval := 100;
  FMakeErrors := true;
end;

procedure TJvLabelTyping.DoTimerEvent(Sender: TObject);
var
  tmp: string;
  i: integer;
begin
  FTimer.Enabled := false;
  if FTextPos <= Length(FOriginalText) then
  begin
    tmp := Copy(FOriginalText, 1, FTextPos - 1);
    i := Random(10);
    if (i = 7) and MakeErrors then
      tmp := tmp + Char(Ord(FOriginalText[FTextPos]) - Random(10))
    else
      tmp := tmp + FOriginalText[FTextPos];
    if (MakeErrors) and (i <> 7) then
      FTimer.Interval := Interval
    else
      FTimer.Interval := Interval * 2;
    OwnerLabel.Caption := tmp;
    Inc(FTextPos);
    FTimer.Enabled := true;
  end
  else
    Active := false;
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

procedure TJvLabelTyping.SetMakeErrors(const Value: boolean);
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
  inherited;
  if (csLoading in OwnerLabel.ComponentState) then Exit;
  if FTimer = nil then
  begin
    FTimer := TTimer.Create(nil);
    FTimer.Enabled := false;
    FTimer.OnTimer := DoTimerEvent;
  end;
  FTimer.Interval := Interval;
  Randomize;
  FOriginalText := OwnerLabel.Caption;
  FTextPos := 1;
  FTimer.Enabled := true;
end;

procedure TJvLabelTyping.Stop;
begin
  if FTimer <> nil then
  begin
    FreeAndNil(FTimer);
    OwnerLabel.Caption := FOriginalText;
  end;
  inherited;
end;

{ TJvLabelSpecial }

constructor TJvLabelSpecial.Create(ALabel: TJvCustomBehaviorLabel);
begin
  inherited;
  FInterval := 20;
end;

procedure TJvLabelSpecial.DoTimerEvent(Sender: TObject);
begin
  FTimer.Enabled := false;
  if FTextPos < Length(FOriginalText) then
  begin
    if FCharValue > Ord(FOriginalText[FTextPos]) then
    begin
      Inc(FTextPos);
      FCharValue := 32;
    end;
    OwnerLabel.Caption := Copy(FOriginalText, 1, FTextPos) + char(FCharValue);
    Inc(FCharValue);
    FTimer.Enabled := true;
  end
  else
    Active := false;
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
  inherited;
  if (csLoading in OwnerLabel.ComponentState) then Exit;
  if FTimer = nil then
  begin
    FTimer := TTimer.Create(nil);
    FTimer.Enabled := false;
    FTimer.OnTimer := DoTimerEvent;
  end;
  FTextPos := 1;
  FCharValue := 32;
  FOriginalText := OwnerLabel.Caption;
  FTimer.Interval := Interval;
  FTimer.Enabled := true;
end;

procedure TJvLabelSpecial.Stop;
begin
  if FTimer <> nil then
  begin
    FreeAndNil(FTimer);
    OwnerLabel.Caption := FOriginalText;
  end;
  inherited;
end;

{ TJvLabelCodeBreaker }

constructor TJvLabelCodeBreaker.Create(ALabel: TJvCustomBehaviorLabel);
begin
  inherited;
  FInterval := 10;
end;

procedure TJvLabelCodeBreaker.DoTimer(Sender: TObject);
begin
  FTimer.Enabled := false;
  if (FCurrentPos > Length(FScratchPad)) or (FCurrentPos > Length(DecodedText)) then
  begin
    with OwnerLabel do
    begin
      Caption := DecodedText;
//      Repaint;
    end;
    Active := false;
    Exit;
  end
  else if FScratchPad[FCurrentPos] <> DecodedText[FCurrentPos] then
  begin
    FScratchPad[FCurrentPos] := Char(32 + Random(Ord(DecodedText[FCurrentPos]) + 10));
    with OwnerLabel do
    begin
      Caption := Copy(Caption, 1, FCurrentPos - 1) + FScratchPad[FCurrentPos] + Copy(Caption, FCurrentPos + 1, MaxInt);
//      Repaint;
    end
  end
  else
    Inc(FCurrentPos);
  sleep(FInterval);
  FTimer.Enabled := true;
end;

procedure TJvLabelCodeBreaker.SetInterval(const Value: integer);
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
  inherited;
  FOriginal := OwnerLabel.Caption;
  FCurrentPos := 1;
  if (Interval > 0) and (Length(FOriginal) > 0) and (Length(DecodedText) > 0) then
  begin
    FScratchPad := FOriginal;
    FTimer := TTimer.Create(nil);
    FTimer.Enabled := false;
    FTimer.OnTimer := DoTimer;
    FTimer.Interval := Interval;
    FTimer.Enabled := true;
  end
  else
    Active := false;
end;

procedure TJvLabelCodeBreaker.Stop;
begin
  FreeAndNil(FTimer);
//  OwnerLabel.Caption := FOriginal;
  inherited;
end;

initialization
  RegisterLabelBehaviorOptions('Blinking', TJvLabelBlink);
  RegisterLabelBehaviorOptions('Bouncing', TJvLabelBounce);
  RegisterLabelBehaviorOptions('Scrolling', TJvLabelScroll);
  RegisterLabelBehaviorOptions('Typing', TJvLabelTyping);
  RegisterLabelBehaviorOptions('Appearing', TJvLabelAppear);
  RegisterLabelBehaviorOptions('Special', TJvLabelSpecial);
  RegisterLabelBehaviorOptions('CodeBreaker', TJvLabelCodeBreaker);
finalization
  FreeAndNil(FBehaviorOptions);

end.

