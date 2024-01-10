{
/// FileName:XPMenu.pas

XPMenu for Delphi
Author: Khaled Shagrouni
URL: http://www.shagrouni.com
e-mail: shagrouni@hotmail.com
Version 1.504, Septemper 5, 2001

/// Under Lines Add By Kingron
Modified: Kingron
Data: 2001.09.29
E_Mail:Kingron@163.net
WWW: http://Kingron.myetang.com

I hold the copyright of the modificatory code,If you make any modifications to
the code, please send them to me.Any question the component,Please Mail to me

Note:
   - I Only Test the Component Under Win2K & Win98SE,May be not fit other OS.
/// End Add

XPMenu is a Delphi component to mimic Office XP menu and toolbar style.
Copyright (C) 2001 Khaled Shagrouni.

This component is FREEWARE with source code. I still hold the copyright.
If you have any ideas for improvement or bug reports, don't hesitate to e-mail me.

History:
========

/// Under Lines Add By Kingron
2001.10.4
   - Fix Some Bugs,Ex:FlatMenu's Memory Hole~~~!
2001.10.1
   - Optimize the Draw Speed For Win9x
   - Fix The MenuItem Bitmap Transparent bug under Win9x
   - Fix Other Bugs Under Win9x
   - Fix Top MenuItem Bitmap Draw Bug.
   - Use Double buffer to Draw the Bar(Text Only),Improve the Draw Speed.
2001.09.30
   - Support Event Process.
     OnDrawItem,OnMeasureItem,OnDrawBar,OnMeasureBar
   - Adding a Example For Delphi 5.0.
   - Fix a bug: Count Bar Height Error when user define the System Menu appearance
   - Fix the Default MenuItem Display.
   - Fix some Bugs Under Win9x.
2001.09.29
   - Left Bar Support Stretch(Bitmap Only).
   - Fix some Bugs
   - Support 3D Style
2001.09.28
   - Adding Left Bar,Support Bitmap and Text.
   - Left Bar(Text Only) Support Gradient Color.

Known Bugs:
   - Under Win9x,the FlatMenu property has some problam.
     if use the component under Win9x,Please don't set the FlatMenu to False.
   - Under Win9x,if you both use the ImageList and MenuItem.Bitmap,the Bar Height calc err.
     You can use the Event to Special Process to avoid it!
   - If SubMenu Level more than 3,Draw Speed Very Slow~~~~~~~
     Because the Component recursion search all MenuItem of the Form!
     I hope I can improve the arithmetic to fix this soon, help from others appreciated.
   - if User Change The System Menu appearance,the Bar Calc Err.
     Please Process SYSTEM SETTING CHANGE Message,Set the property UseSystemColors:=False then
     Set the Property UseSystemColors := True
   - May be not compatibly with other program,Ex: ScreenSave,Meet it infrequent.
   - Under Win9x,if not use the Gradient Property, the IconBack has some problem.
   - Big Bug:Resource Not Free~~~~,Checking~~~~~~
     OK: Don't use FlatMenu,the property Cause the Memory hole when the program running~~~~
         But when the program exit,the Memory get back.
/// End Add

Sept 5, 2001, V1.504
   - Removing some problematic code lines in the procedure: ToolBarDrawButton.
     This code causes unwanted effect on desktop when activating the component
     at run time with form contains a ToolBarButton with MenuItem.
Sept 4, 2001, V1.503
   - Bug fixed.
Sept 3, 2001, V1.502
   - Bugs fixed.
Sept 1, 2001, V1.501
   - Some minor changes and bugs fixed.
July 29, 2001, V1.501 (Beta)
   - Adding AutoDetect property.
   - Compatibility issues with Delphi4.
July 25, 2001, V1.5
   - Support for TToolbar.
   - Getting closer to XP style appearance.
   - New options.
june 23, 2001
   - Compatibility issues with Delphi4.
   - Changing the way of menus itration.
   - Making the blue select rectangle little thinner.

june 21, 2001
  Bug fixes:
   - Items correctly sized even if no image list assigned.
   - Shaded colors for top menu items if fixed for some menu bar colors.
  (Actually the bugs was due to two statements deleted by me stupidly/accidentally)

June 19, 2001
  This component is based on code which I have posted at Delphi3000.com
  (http://www.delphi3000/articles/article_2246.asp) and Borland Code-Central
  (http://codecentral.borland.com/codecentral/ccweb.exe/listing?id=16120).

Installation

A. Unzip the files: XPMENU.PAS and XPMENU.DCR Into the same directory.
B. From Delphi menu, Select File| New: Package.
C. Press Add, and browse to add the unit XPMENU.PAS.
D. Press Install.
E. The component is now installed in a new 'XP' page.
F. Save the package.

If you have a previous version installed:
Replace the old files (xpmenu.pas and xpmenu.dcr) with the new one,
open the package and recompile.
If you encounter any problems remove all the compiled units .dcu, .bpl, .dcp
(try to locate them also in 'C:\Program Files\Borland\DelphiX\Projects\Bpl' and
'C:\Program Files\Borland\DelphiX\lib'), then install pre-compiled units again.

--------------------------------------------------------------------------------

Notes on proprties:

Active property:
 To activate/deactivate xpMenu, also, set this property to True when new items
 added at run time.

AutoDetect property:
 Set this property to True to force xpMenu to include new added items
 automatically.

UseSystemColors property:
 The global windows color scheme will be used, setting this property to true
 will override other color related properties.

OverrideOwnerDraw property:
 By default, xpMenu will not affect menu items that has owner draw handler
 assigned (any code in OnDrawItem event). To override any custom draw set this property to true.

Gradient property:
 IconBackcolor will be used as a gradient color for the entire menu,
 Color property wil be ignored.

FlatMenu property:
 To turn menu's border to flat (drop-down and pop-up menu). Any way, a flat
 effect will not appear until a menu item is selected, also unwanted effect
 come across if there is submenu item selected. I hope I can fix this soon,
 help from others appreciated.

Form property:
 The default is the host form, if you want to target a different
 form other than the one hosting the component; set Form property to that form.

--------------------------------------------------------------------------------

ImageLists:
  For toolbars only ImageList assigned to Images property is used; xpMenu
  automatically generate dim and grayed images for non-hot and disabled items.

Buttons with tbsDivider style:
 xpMenu cannot draw toolbar buttons with tbsDivider style, Windows override any
 owner draw for this style (I am using Win 98). To work around this, set the
 button style to tbsSeparator and set its Tag property to none zero value.

Creation order:
 Make sure that the creation order of TXPMenu comes after any menu or toolbar
 component. To change the creation order, choose Edit | Creation Order from
 Delphi menu to open the Creation Order dialog box.

--------------------------------------------------------------------------------

Known issues:

 - xpMenu supports menus only in Delphi 4.
 - xpMenu doesn't detect menus and toolbars inside Frame, the work-around for
   this is to add xpMenu component in the Frame it self.

--------------------------------------------------------------------------------

Tips:

How to create menu toolbar:
 (Extracted from Delphi Help - TToolButton.MenuItem)

 To create an "IE4-style" (Office-style) toolbar that corresponds to
 an existing menu:
  1 Drop a ToolBar on the form and add a ToolButton for each top-level menu
    item you wish to create.
  2 Set the MenuItem property of each ToolButton to correspond to the top level
    menu items.
  3 Set the Grouped property of each ToolButton to True.
  4 Clear the MainMenu property of the Form (if it is assigned)

Images in toolbars and menus:
 To make an image transparent, be sure to fill the background with a unique
 color-a color your image is not using. Also, make sure that the color of the
 bottom leftmost pixel shown onscreen has the same background color; xpMenu will
 use this pixel to determine the transparent color.

}
//______________________________________________________________________________

{$IFDEF VER130}
{$DEFINE VER5U}
{$ENDIF}

{$IFDEF VER140}
{$DEFINE VER5U}
{$ENDIF}

unit XPBarMenu;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, ComCtrls, Forms,
  Menus, Messages, Commctrl;

/// Under Lines Add By Kingron
type
  TBarStyle = (bsText, bsBitmap, bsNone);
  TDrawBarEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect; var CanDraw: Boolean) of object;
  TMeasureBarEvent = procedure(Sender: TObject; ACanvas: TCanvas; var ARect, BarRect: TRect) of object;
/// End Add

type
  TXPBarMenu = class(TComponent)
  private
    FActive: boolean;
    FForm: TForm;
    FFont: TFont;
    FColor: TColor;
    FIconBackColor: TColor;
    FMenuBarColor: TColor;
    FCheckedColor: TColor;
    FSeparatorColor: TColor;
    FSelectBorderColor: TColor;
    FSelectColor: TColor;
    FDisabledColor: TColor;
    FSelectFontColor: TColor;
    FIconWidth: integer;
    FDrawSelect: boolean;
    FUseSystemColors: boolean;

    FFColor, FFIconBackColor, FFSelectColor, FFSelectBorderColor,
      FFSelectFontColor, FCheckedAreaColor, FCheckedAreaSelectColor,
      FFCheckedColor, FFMenuBarColor, FFDisabledColor, FFSeparatorColor,
      FMenuBorderColor, FMenuShadowColor: TColor;

    Is16Bit: boolean;
    FOverrideOwnerDraw: boolean;
    FGradient: boolean;
    FFlatMenu: boolean;
    FAutoDetect: boolean;

    /// Under Lines Add By Kingron
    FItemHeight: integer;
    FFrame3D: boolean;
    FOnDrawBar: TDrawBarEvent;
    FOnMeasureBar: TMeasureBarEvent;
    FBarStretch: boolean;
    FBarCaption: string;
    FBarWidth: integer;
    FBarColorStart: TColor;
    FBarColorEnd: TColor;
    FBarBitmap: TBitmap;
    FBarFont: TFont;
    FBarStyle: TBarStyle;
    FOnDrawItem: TMenuDrawItemEvent;
    FOnMeasureItem: TMenuMeasureItemEvent;
    FBarColorStep: integer;
    /// End Add

    procedure SetActive(const Value: boolean);
    procedure SetForm(const Value: TForm);
    procedure SetFont(const Value: TFont);
    procedure SetMenuBarColor(const Value: TColor);
    procedure SetUseSystemColors(const Value: boolean);
    procedure SetOverrideOwnerDraw(const Value: boolean);

    /// Under Lines Add By Kingron
    function CanDrawBar: boolean;
    procedure SetBarFont(const Value: TFont);
    procedure SetBarBitmap(const Value: TBitmap);
    procedure SetBarColorStep(const Value: integer);
    function _IsOSoK:boolean;
    /// End Add

  protected
    /// Under Lines Add By Kingron
    procedure DrawBar(Sender: TObject; ACanvas: TCanvas; ARect: TRect);
    /// End Add

    procedure InitMenueItems(Form: TScrollingWinControl; Enable: boolean);
    procedure DrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      Selected: Boolean);
    procedure MenueDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      Selected: Boolean);
{$IFDEF VER5U}
    procedure ToolBarDrawButton(Sender: TToolBar;
      Button: TToolButton; State: TCustomDrawState; var DefaultDraw: Boolean);
{$ENDIF}
    procedure ActivateMenuItem(MenuItem: TMenuItem);
    procedure SetGlobalColor(ACanvas: TCanvas);
    procedure DrawTopMenuItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      IsRightToLeft: boolean);
    procedure DrawCheckedItem(FMenuItem: TMenuItem; Selected,
      HasImgLstBitmap: boolean; ACanvas: TCanvas; CheckedRect: TRect);
    procedure DrawTheText(txt, ShortCuttext: string; ACanvas: TCanvas;
      TextRect: TRect; Selected, Enabled, Default, TopMenu,
      IsRightToLeft: boolean; TextFormat: integer);
    procedure DrawIcon(Sender: TObject; ACanvas: TCanvas; B: TBitmap;
      IconRect: Trect; Hot, Selected, Enabled, Checked, FTopMenu,
      IsRightToLeft: boolean);
    procedure DrawArrow(ACanvas: TCanvas; X, Y: integer);
    procedure MeasureItem(Sender: TObject; ACanvas: TCanvas;
      var Width, Height: Integer);

    function GetImageExtent(MenuItem: TMenuItem): TPoint;
    function TopMenuFontColor(ACanvas: TCanvas; Color: TColor): TColor;
    procedure DrawGradient(ACanvas: TCanvas; ARect: TRect;
      IsRightToLeft: boolean);

    procedure DrawWindowBorder(hWnd: HWND; IsRightToLeft: boolean);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Form: TForm read FForm write SetForm;
  published
    /// Under Lines Add By Kingron
    property BarStretch: boolean read FBarStretch write FBarStretch default true;
    property Frame3D: boolean read FFrame3D write FFrame3D default false;
    property BarWidth: integer read FBarWidth write FBarWidth default 22;
    property BarCaption: string read FBarCaption write FBarCaption;
    property BarBitmap: TBitmap read FBarBitmap write SetBarBitmap;
    property BarColorStart: TColor read FBarColorStart write FBarColorStart default clBlue;
    property BarColorEnd: TColor read FBarColorEnd write FBarColorEnd default clBlack;
    property BarColorStep: integer read FBarColorStep write SetBarColorStep default 100;
    property BarStyle: TBarStyle read FBarStyle write FBarStyle default bsText;
    property BarFont: TFont read FBarFont write SetBarFont;
    property ItemHeight: integer read FItemHeight write FItemHeight default 0;
    property OnDrawItem: TMenuDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnMeasureItem: TMenuMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
    property OnDrawBar: TDrawBarEvent read FOnDrawBar write FOnDrawBar;
    property OnMeasureBar: TMeasureBarEvent read FOnMeasureBar write FOnMeasureBar;
    /// End Add
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write FColor default clMenu;
    property IconBackColor: TColor read FIconBackColor write FIconBackColor default clMenu;
    property MenuBarColor: TColor read FMenuBarColor write SetMenuBarColor default clMenu;
    property SelectColor: TColor read FSelectColor write FSelectColor default clHighlight;
    property SelectBorderColor: TColor read FSelectBorderColor write FSelectBorderColor default clActiveBorder;
    property SelectFontColor: TColor read FSelectFontColor write FSelectFontColor default clHighlightText;
    property DisabledColor: TColor read FDisabledColor write FDisabledColor default clGray;
    property SeparatorColor: TColor read FSeparatorColor write FSeparatorColor default clGray;
    property CheckedColor: TColor read FCheckedColor write FCheckedColor;
    property IconWidth: integer read FIconWidth write FIconWidth;
    property DrawSelect: boolean read FDrawSelect write FDrawSelect;
    property UseSystemColors: boolean read FUseSystemColors  write SetUseSystemColors;
    property OverrideOwnerDraw: boolean read FOverrideOwnerDraw  write SetOverrideOwnerDraw;

    property Gradient: boolean read FGradient write FGradient default true;
    property FlatMenu: boolean read FFlatMenu write FFlatMenu;
    property AutoDetect: boolean read FAutoDetect write FAutoDetect;
    property Active: boolean read FActive write SetActive;
  end;

function GetShadeColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
function NewColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
procedure DimBitmap(ABitmap: TBitmap; Value: integer);
function GrayColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
procedure GrayBitmap(ABitmap: TBitmap; Value: integer);
procedure DrawBitmapShadow(B: TBitmap; ACanvas: TCanvas; X, Y: integer;
  ShadowColor: TColor);

procedure GetSystemMenuFont(Font: TFont);
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Standard', [TXPBarMenu]); ///Registry Control Pages Modified By Kingron
end;

{ TXPBarMenue }

constructor TXPBarMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  GetSystemMenuFont(FFont);
  FForm := Owner as TForm;

  FUseSystemColors := true;

  FColor := clMenu;
  FIconBackColor := clMenu;
  FSelectColor := clHighlight;
  FSelectBorderColor := clActiveBorder;
  FMenuBarColor := clMenu;
  FDisabledColor := clGray;
  FSeparatorColor := clGray;
  FCheckedColor := clHighlight;
  FSelectFontColor := clHighlightText;

  FIconWidth := 24;
  FDrawSelect := true;

  ///Under Line Add By Kingron
  FFlatMenu := True;
  FActive := True;
  FAutoDetect := True;
  FGradient := True;
  FFrame3D := False;
  FBarStretch := True;
  FBarCaption := FForm.Caption; /// Bar Default Caption
  FItemHeight := 0; /// Default Item Height
  FBarWidth := 22; /// Bar Width
  FBarColorStart := clBlue; /// The First Color
  FBarColorEnd := clBlack; /// The Second Color
  FBarColorStep := 100;
  FBarFont := TFont.Create; /// Bar Text Font
  FBarFont.Assign(FFont); /// Bar Font Init
  FBarFont.Color := clWhite;
  FBarBitmap := TBitmap.Create; /// Bar Bitmap
  FBarStyle := bsText; /// Bar Style
  /// End Add

  if FActive then
  begin
    InitMenueItems(FForm, true);
  end;

end;

destructor TXPBarMenu.Destroy;
begin
  InitMenueItems(FForm, False);
  FFont.Free;

  /// Under Lines Add By Kingron
  FBarFont.Free;
  FreeAndNil(FBarBitmap);
  /// End Add

  inherited;
end;

procedure TXPBarMenu.ActivateMenuItem(MenuItem: TMenuItem);

  procedure Activate(MenuItem: TMenuItem);
  begin
    if addr(MenuItem.OnDrawItem) <> addr(TXPBarMenu.DrawItem) then
    begin
      if (not assigned(MenuItem.OnDrawItem)) or (FOverrideOwnerDraw) then
        MenuItem.OnDrawItem := DrawItem;
      if (not assigned(MenuItem.OnMeasureItem)) or (FOverrideOwnerDraw) then
        MenuItem.OnMeasureItem := MeasureItem;
    end
  end;

var
  i, j              : integer;
begin

  Activate(MenuItem);
  for i := 0 to MenuItem.Parent.Count - 1 do
  begin
    Activate(MenuItem.Parent.Items[i]);
    for j := 0 to MenuItem.Parent.Items[i].Count - 1 do
      ActivateMenuItem(MenuItem.Parent.Items[i].Items[j]);
  end;

end;

procedure TXPBarMenu.InitMenueItems(Form: TScrollingWinControl; Enable: boolean);

  procedure Activate(MenuItem: TMenuItem);
  begin
    if Enable then
    begin
      if (not assigned(MenuItem.OnDrawItem)) or (FOverrideOwnerDraw) then
        MenuItem.OnDrawItem := DrawItem;
      if (not assigned(MenuItem.OnMeasureItem)) or (FOverrideOwnerDraw) then
        MenuItem.OnMeasureItem := MeasureItem;
    end
    else
    begin
      if addr(MenuItem.OnDrawItem) = addr(TXPBarMenu.DrawItem) then
        MenuItem.OnDrawItem := nil;
      if addr(MenuItem.OnMeasureItem) = addr(TXPBarMenu.MeasureItem) then
        MenuItem.OnMeasureItem := nil;
    end;
  end;

 procedure ItrateMenu(MenuItem: TMenuItem);
  var
    i: integer;
  begin
    Activate(MenuItem);
    for i := 0 to MenuItem.Count - 1 do
      ItrateMenu(MenuItem.Items[i]);
  end;


var
  i, x              : integer;
begin
  for i := 0 to Form.ComponentCount - 1 do
  begin
    if Form.Components[i] is TMainMenu then
    begin
      for x := 0 to TMainMenu(Form.Components[i]).Items.Count - 1 do
      begin
        TMainMenu(Form.Components[i]).OwnerDraw := Enable;
        Activate(TMainMenu(Form.Components[i]).Items[x]);
        ItrateMenu(TMainMenu(Form.Components[i]).Items[x]);
      end;
    end;
    if Form.Components[i] is TPopupMenu then
    begin
      for x := 0 to TPopupMenu(Form.Components[i]).Items.Count - 1 do
      begin
        TPopupMenu(FForm.Components[i]).OwnerDraw := Enable;
        Activate(TMainMenu(Form.Components[i]).Items[x]);
        ItrateMenu(TMainMenu(Form.Components[i]).Items[x]);
      end;
    end;

{$IFDEF VER5U}
    if Form.Components[i] is TToolBar then
      if not (csDesigning in ComponentState) then
      begin
        if not TToolBar(Form.Components[i]).Flat then
          TToolBar(Form.Components[i]).Flat := true;

        if Enable then
        begin
          for x := 0 to TToolBar(FForm.Components[i]).ButtonCount - 1 do
            if (not assigned(TToolBar(Form.Components[i]).OnCustomDrawButton))
              or (FOverrideOwnerDraw) then
            begin
              TToolBar(FForm.Components[i]).OnCustomDrawButton :=
                ToolBarDrawButton;

            end;
        end
        else
        begin
          if addr(TToolBar(Form.Components[i]).OnCustomDrawButton) =
            addr(TXPBarMenu.ToolBarDrawButton) then
            TToolBar(Form.Components[i]).OnCustomDrawButton := nil;

        end;
      end;
{$ENDIF}

  end;
end;

procedure TXPBarMenu.DrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
  Selected: Boolean);
begin
  if FActive then
  begin
    MenueDrawItem(Sender, ACanvas, ARect, Selected);
    /// Under Line Add By Kingron, Add OnDrawItem Event Process
    if Assigned(FOnDrawItem) then
      FOnDrawItem(Sender, ACanvas, ARect, Selected);
    /// End Add
  end;
end;

function TXPBarMenu.GetImageExtent(MenuItem: TMenuItem): TPoint;
var
  HasImgLstBitmap   : boolean;
  B                 : TBitmap;
  FTopMenu          : boolean;
begin
  FTopMenu := false;
  B := TBitmap.Create;
  B.Width := 0;
  B.Height := 0;
  Result.x := 0;
  Result.Y := 0;
  HasImgLstBitmap := false;

  if FForm.Menu <> nil then
    if MenuItem.GetParentComponent.Name = FForm.Menu.Name then
    begin
      FTopMenu := true;
      if FForm.Menu.Images <> nil then
        if MenuItem.ImageIndex <> -1 then
          HasImgLstBitmap := true;

    end;

  if (MenuItem.Parent.GetParentMenu.Images <> nil)
{$IFDEF VER5U}
  or (MenuItem.Parent.SubMenuImages <> nil)
{$ENDIF}
  then
  begin
    if MenuItem.ImageIndex <> -1 then
      HasImgLstBitmap := true
    else
      HasImgLstBitmap := false;
  end;

  if HasImgLstBitmap then
  begin
{$IFDEF VER5U}
    if MenuItem.Parent.SubMenuImages <> nil then
      MenuItem.Parent.SubMenuImages.GetBitmap(MenuItem.ImageIndex, B)
    else
{$ENDIF}
      MenuItem.Parent.GetParentMenu.Images.GetBitmap(MenuItem.ImageIndex, B)
  end
  else
    if MenuItem.Bitmap.Width > 0 then
      B.Assign(MenuItem.Bitmap);

  Result.x := B.Width;
  Result.Y := B.Height;

  if not FTopMenu and not HasImgLstBitmap then
    if Result.x < FIconWidth then
      Result.x := FIconWidth;

  B.Free;
end;

procedure TXPBarMenu.MeasureItem(Sender: TObject; ACanvas: TCanvas;
  var Width, Height: Integer);
var
  s                 : string;
  W, H              : integer;
  P                 : TPoint;
  IsLine            : boolean;

  /// Under Lines Add By Kingron
  FMenu             : TMenu;
  FMenuItem         : TMenuItem;
  i                 : integer;
  FTopMenu          : boolean;
  /// End Add
begin
  if FActive then
  begin
    FMenuItem := TMenuItem(Sender);
    S := FMenuItem.Caption;
      //------
    if S = '-' then IsLine := true else IsLine := false;
    /// Under Lines Comment By Kingron
///    if IsLine then
    /// End Comment

      //------
    if IsLine then
      S := '';

    if Trim(ShortCutToText(FMenuItem.ShortCut)) <> '' then
      S := S + ShortCutToText(FMenuItem.ShortCut) + 'WWW';

    ACanvas.Font.Assign(FFont);
    W := ACanvas.TextWidth(s);
    if pos('&', s) > 0 then
      W := W - ACanvas.TextWidth('&');

    P := GetImageExtent(FMenuItem);

    W := W + P.x + 10;

    if Width < W then
      Width := W;

    if IsLine then
      Height := 4
    else
    begin
      H := ACanvas.TextHeight(s) + Round(ACanvas.TextHeight(s) * 0.75);
      if P.y + 4 > H then
        H := P.y + 4;

      if Height < H then
        Height := H;
    end;
  ///Under Lines Add By Kingron
  //// 这个地方有待于改进算法，看看能不能不使用循环？否则速度太慢~~~~~~
    if CanDrawBar then /// Should Draw the Bar
    begin
      FTopMenu := False;
      FMenu := FMenuItem.Parent.GetParentMenu;

      if FMenu is TMainMenu then /// Search For Top Level Item?
      begin
        for i := 0 to FMenuItem.GetParentMenu.Items.Count - 1 do
          if FMenuItem.GetParentMenu.Items[i] = FMenuItem then /// Yes!
          begin
            FTopMenu := True;
            break;
          end;
      end;
      if not FTopMenu then /// Should Not be the TOP Level Item!
        Inc(Width, FBarWidth + 2); /// Add Width For the Bar

      if FItemHeight <> 0 then /// User Define Item Height!
        Height := FItemHeight;
    end;

    if Assigned(FOnMeasureItem) then
      FOnMeasureItem(Sender, ACanvas, Width, Height);

  /// End Add

  end;
end;

procedure TXPBarMenu.MenueDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
  Selected: Boolean);
var
  txt               : string;
  B                 : TBitmap;
  IconRect, TextRect, CheckedRect: TRect;
  i, X1, X2         : integer;
  TextFormat        : integer;
  HasImgLstBitmap   : boolean;
  FMenuItem         : TMenuItem;
  FMenu             : TMenu;
  FTopMenu          : boolean;
  ISLine            : boolean;
  ImgListHandle     : HImageList; {Commctrl.pas}
  ImgIndex          : integer;
  hWndM             : HWND;
  hDcM              : HDC;
  /// Under Add By Kingron
  FBarHeight        : integer;
  BarRect           : TRect;
  /// End Add

  /// Under Function Add By Kingron

  function GetItemHeight(Sender: TObject): integer;
  var
    Width, Height   : Integer;
  begin
    Height := 0;
    MeasureItem(Sender, ACanvas, Width, Height);
    Result := Height;
  end;
  /// End Function Add

begin
  FTopMenu := false;
  FMenuItem := TMenuItem(Sender);

  SetGlobalColor(ACanvas);

  if FMenuItem.Caption = '-' then IsLine := true else IsLine := false;

  FMenu := FMenuItem.Parent.GetParentMenu;

  if FMenu is TMainMenu then
    for i := 0 to FMenuItem.GetParentMenu.Items.Count - 1 do
      if FMenuItem.GetParentMenu.Items[i] = FMenuItem then
      begin
        FTopMenu := True;
        break;
      end;

  /// Under Lines Add By Kingron
  if not FTopMenu and CanDrawBar then
  begin
    FBarHeight := 0; /// Count For the Bar height
    for i := 0 to FMenuItem.Parent.Count - 1 do
      if FMenuItem.Parent.Items[i].Visible then
        if FItemHeight <> 0 then /// if User Define the Item Height?
          Inc(FBarHeight, FItemHeight) /// Yes,Should Add the Define ItemHeight
        else
          if FMenuItem.Parent.Items[i].IsLine then /// Is -------?
            Inc(FBarHeight, 4) /// The Line's Default Height!
          else
            Inc(FBarHeight, GetItemHeight(FMenuItem.Parent.Items[i])); /// Add Default ItemHeight;

    Dec(ARect.Right, FBarWidth); /// Adjust RECT for the Bar!
    /// if You wan't Left a room for bar between,please modify: FBarWidth to FBarWidth - 1
    OffsetRect(ARect, FBarWidth, 0);

    BarRect := Rect(1, 1, FBarWidth, FBarHeight);
    if Assigned(FOnMeasureBar) then
      FOnMeasureBar(Sender, ACanvas, ARect, BarRect);

    DrawBar(Sender, ACanvas, BarRect); /// Draw the Bar
  end;
  /// End Add

  ACanvas.Font.Assign(FFont);
  if FMenu.IsRightToLeft then
    ACanvas.Font.Charset := ARABIC_CHARSET;

  Inc(ARect.Bottom, 1);
  TextRect := ARect;
  txt := ' ' + FMenuItem.Caption;

  B := TBitmap.Create;

  HasImgLstBitmap := false;

  if FMenuItem.Bitmap.Width > 0 then
    B.Assign(TBitmap(FMenuItem.Bitmap));

  if (FMenuItem.Parent.GetParentMenu.Images <> nil)
{$IFDEF VER5U}
  or (FMenuItem.Parent.SubMenuImages <> nil)
{$ENDIF}
  then
  begin
    if FMenuItem.ImageIndex <> -1 then
      HasImgLstBitmap := true
    else
      HasImgLstBitmap := false;
  end;

  if FMenu.IsRightToLeft then
  begin
    X1 := ARect.Right - FIconWidth;
    X2 := ARect.Right;
  end
  else
  begin
    X1 := ARect.Left;
    X2 := ARect.Left + FIconWidth;
  end;
  IconRect := Rect(X1, ARect.Top , X2, ARect.Bottom );

  if HasImgLstBitmap then
  begin
    CheckedRect := IconRect;
    Inc(CheckedRect.Left, 1);
    Inc(CheckedRect.Top, 2);
    Dec(CheckedRect.Right, 3);
    Dec(CheckedRect.Bottom, 2);
  end
  else
  begin
    CheckedRect.Left := IconRect.Left +
      (IConRect.Right - IconRect.Left - 10) div 2;
    CheckedRect.Top := IconRect.Top +
      (IConRect.Bottom - IconRect.Top - 10) div 2;
    CheckedRect.Right := CheckedRect.Left + 10;
    CheckedRect.Bottom := CheckedRect.Top + 10;
  end;

  if FMenu.IsRightToLeft then
  begin
    X1 := ARect.Left;
    X2 := ARect.Right - FIconWidth;
    if B.Width > FIconWidth then
      X2 := ARect.Right - B.Width - 4;
  end
  else
  begin
    X1 := ARect.Left + FIconWidth;
    if B.Width > X1 then
      X1 := B.Width + 4;
    X2 := ARect.Right;
  end;

  TextRect := Rect(X1, ARect.Top, X2, ARect.Bottom);

  if FTopMenu then
  begin
    if not HasImgLstBitmap then
    begin
      TextRect := ARect;
    end
    else
    begin
      if FMenu.IsRightToLeft then
        TextRect.Right := TextRect.Right + 5
      else
        TextRect.Left := TextRect.Left - 5;
    end
  end;
  /// Under Lines Add By Kingron,Fix Top MenuItem Bitmap Draw
  if FMenuItem.Bitmap <> nil then
    if FTopMenu then /// Top MenuItem
    begin
      Inc(TextRect.Left, B.Width); /// Adjust TextRect For Top MenuItem Bitmap
      IconRect.Top := TextRect.Top + (TextRect.Bottom - TextRect.Top - B.Height) div 4;
      if (TextRect.Bottom - TextRect.Top) < 20 then
        Dec(IconRect.Top, 3); /// Adjust For Low Height
    end
    else
    begin /// MenuItem
      if not HasImgLstBitmap then
      begin
        IconRect.Top := TextRect.Top + (TextRect.Bottom - TextRect.Top - FIconWidth div 2) div 4;
        if (TextRect.Bottom - TextRect.Top) >= FIconWidth then
          Dec(IconRect.Top, 3) /// Adjust For Low Height
      end
      else
      begin
        IconRect.Top := TextRect.Top + (TextRect.Bottom - TextRect.Top - FMenuItem.Parent.GetParentMenu.Images.Height div 2) div 4;
        if (TextRect.Bottom - TextRect.Top) <= FMenuItem.Parent.GetParentMenu.Images.Height then
          Dec(IconRect.Top, FMenuItem.Parent.GetParentMenu.Images.Height div 2) /// Adjust For Low Height
        else
          Dec(IconRect.Top, 4);
      end;
    end;
  /// Under Lines Fix The MenuItem IconRect Calc Err Bug under Win9x
  if not _IsOSoK and not FTopMenu and (HasImgLstBitmap or (FMenuItem.Bitmap.Width <20)) then
    Dec(IconRect.Top ,3);
  /// End Add

  if FTopMenu then
  begin
    ACanvas.brush.color := FFMenuBarColor;
    ACanvas.Pen.Color := FFMenuBarColor;

    ACanvas.FillRect(ARect);
  end
  else
  begin
    if (Is16Bit and FGradient) then
    begin
      inc(ARect.Right, 2); //needed for RightToLeft
      DrawGradient(ACanvas, ARect, FMenu.IsRightToLeft);
      Dec(ARect.Right, 2);
    end
    else
    begin
      ACanvas.brush.color := FFColor;
      if (not FMenuItem.Enabled) and (Selected) then
      else
        ACanvas.FillRect(ARect);

      ACanvas.brush.color := FFIconBackColor;
      if (not FMenuItem.Enabled) and (Selected) then
      else
/// Under Line Modify by Kingron,To Fix the Gradient Bug under Win9x
/// Old : ACanvas.FillRect(IconRect);
        ACanvas.FillRect(Rect(IconRect.Left ,IconRect.Top -2,IconRect.Right,IconRect.Bottom +2));
    end;
//------------
  end;

  if FMenuItem.Enabled then
    ACanvas.Font.Color := FFont.Color
  else
    ACanvas.Font.Color := FDisabledColor;

  if Selected and FDrawSelect then
  begin
    ACanvas.brush.Style := bsSolid;
    if FTopMenu then
    begin
      DrawTopMenuItem(FMenuItem, ACanvas, ARect, FMenu.IsRightToLeft);
    end
    else
      //------
      if FMenuItem.Enabled then
      begin
        /// Under Lines Modify By Kingron,Frame 3D,IF Statement~~~~~~~
        if FFrame3D then
          DrawEdge(ACanvas.Handle, ARect, BDR_SUNKENOUTER, BF_RECT) /// Add By Kingron
        else begin /// Begin ... End == Old Code
          Inc(ARect.Top, 1);
          Dec(ARect.Bottom, 1);
          if FFlatMenu then
            Dec(ARect.Right, 1);
          ACanvas.brush.color := FFSelectColor;
          ACanvas.FillRect(ARect);
          ACanvas.Pen.color := FFSelectBorderColor;
          ACanvas.Brush.Style := bsClear;
          ACanvas.RoundRect(Arect.Left, Arect.top, Arect.Right,
            Arect.Bottom, 0, 0);
          Dec(ARect.Top, 1);
          Inc(ARect.Bottom, 1);
          if FFlatMenu then
            Inc(ARect.Right, 1);
        end;
      end;
      //-----

  end;

  DrawCheckedItem(FMenuItem, Selected, HasImgLstBitmap, ACanvas, CheckedRect);

//-----

  if HasImgLstBitmap then
  begin
{$IFDEF VER5U}
    if FMenuItem.Parent.SubMenuImages <> nil then
    begin
      ImgListHandle := FMenuItem.Parent.SubMenuImages.Handle;
      ImgIndex := FMenuItem.ImageIndex;

      B.Width := FMenuItem.Parent.SubMenuImages.Width;
      B.Height := FMenuItem.Parent.SubMenuImages.Height;
      B.Canvas.Brush.Color := FFIconBackColor;
      B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
      ImageList_DrawEx(ImgListHandle, ImgIndex,
        B.Canvas.Handle, 0, 0, 0, 0, clNone, clNone, ILD_Transparent);

    end
    else
{$ENDIF}
    begin
      ImgListHandle := FMenuItem.Parent.GetParentMenu.Images.Handle;
      ImgIndex := FMenuItem.ImageIndex;

      B.Width := FMenuItem.Parent.GetParentMenu.Images.Width;
      B.Height := FMenuItem.Parent.GetParentMenu.Images.Height;
      B.Canvas.Brush.Color := FFIconBackColor;
      B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
      ImageList_DrawEx(ImgListHandle, ImgIndex,
        B.Canvas.Handle, 0, 0, 0, 0, clNone, clNone, ILD_Transparent);

    end;
  end

  else
    if FMenuItem.Bitmap.Width > 0 then
      B.Assign(TBitmap(FMenuItem.Bitmap));

  DrawIcon(FMenuItem, ACanvas, B, IconRect,
    Selected, False, FMenuItem.Enabled, FMenuItem.Checked,
    FTopMenu, FMenu.IsRightToLeft);

//--------
  if not IsLine then
  begin

    if FMenu.IsRightToLeft then
    begin
      TextFormat := DT_RIGHT + DT_RTLREADING;
      Dec(TextRect.Right, 5);
    end
    else
    begin
      TextFormat := 0;
      Inc(TextRect.Left, 5);
    end;

    DrawTheText(txt, ShortCutToText(FMenuItem.ShortCut),
      ACanvas, TextRect,
      Selected, FMenuItem.Enabled, FMenuItem.Default,
      FTopMenu, FMenu.IsRightToLeft, TextFormat);

//-----------
  end
  else
  begin
    if FMenu.IsRightToLeft then
    begin
      X1 := TextRect.Left;
      X2 := TextRect.Right - 7;
    end
    else
    begin
      /// Under Line Modified By Kingron
      /// Old: X1 := TextRect.Left + 7;
      X1 := TextRect.Left;
      X2 := TextRect.Right;
    end;

    ACanvas.Pen.Color := FFSeparatorColor;

    /// Under Lines Add By Kingron ,Adjust the Line Width!
    if FGradient then /// Adjust Width For Bar
      X1 := ARect.Left;
    /// End Add

    ACanvas.MoveTo(X1, TextRect.Top + Round((TextRect.Bottom - TextRect.Top) / 2));
    ACanvas.LineTo(X2, TextRect.Top + Round((TextRect.Bottom - TextRect.Top) / 2))
  end;

  B.free;

//------

  if not (csDesigning in ComponentState) then
  begin
    if (FFlatMenu) and (not FTopMenu) then
    begin
      hDcM := ACanvas.Handle;
      hWndM := WindowFromDC(hDcM);
      if hWndM <> FForm.Handle then
      begin
        DrawWindowBorder(hWndM, FMenu.IsRightToLeft);
      end;
    end;
  end;

//-----
  ActivateMenuItem(FMenuItem); // to check for new sub items
end;

{$IFDEF VER5U}

procedure TXPBarMenu.ToolBarDrawButton(Sender: TToolBar;
  Button: TToolButton; State: TCustomDrawState; var DefaultDraw: Boolean);

var
  ACanvas           : TCanvas;

  ARect, HPreviousRect   : TRect;
  B                 : TBitmap;
  HasBitmap         : boolean;
  BitmapWidth       : integer;
  TextFormat        : integer;
  XButton           : TToolButton;
  HasBorder         : boolean;
  HasBkg            : boolean;
  IsTransparent     : boolean;
  FBSelectColor     : TColor;

  procedure DrawBorder;
  var
    BRect, WRect    : TRect;

    procedure DrawRect;
    begin
      ACanvas.Pen.color := FFSelectBorderColor;
      ACanvas.MoveTo(WRect.Left, WRect.Top);
      ACanvas.LineTo(WRect.Right, WRect.Top);
      ACanvas.LineTo(WRect.Right, WRect.Bottom);
      ACanvas.LineTo(WRect.Left, WRect.Bottom);
      ACanvas.LineTo(WRect.Left, WRect.Top);
    end;

  begin
    BRect := HPreviousRect;
    Dec(BRect.Bottom, 1);
    Inc(BRect.Top, 1);
    Dec(BRect.Right, 1);

    WRect := BRect;
    if Button.Style = tbsDropDown then
    begin
      Dec(WRect.Right, 13);
      DrawRect;

      WRect := BRect;
      Inc(WRect.Left, WRect.Right - WRect.Left - 13);
      DrawRect;
    end
    else
    begin

      DrawRect;
    end;
  end;

begin

  B := nil;

  HasBitmap := (TToolBar(Button.Parent).Images <> nil) and
    (Button.ImageIndex <> -1) and
    (Button.ImageIndex <= TToolBar(Button.Parent).Images.Count - 1);

  IsTransparent := TToolBar(Button.Parent).Transparent;

  ACanvas := Sender.Canvas;

  SetGlobalColor(ACanvas);

  if (Is16Bit) and (not UseSystemColors) then
    FBSelectColor := NewColor(ACanvas, FSelectColor, 68)
  else
    FBSelectColor := FFSelectColor;

  HPreviousRect := Button.BoundsRect;

  ARect := HPreviousRect;

  {Causing problem when activiting the component at run time
  if FUseSystemColors then
  begin
    if (Button.MenuItem <> nil) then
    begin
      if (TToolBar(Button.Parent).Font.Name <> FFont.Name) or
         (TToolBar(Button.Parent).Font.Size <> FFont.Size) then
      begin

        TToolBar(Button.Parent).Font.Assign(FFont);
        Button.AutoSize := false;
        Button.AutoSize := true;

      end;
    end
  end;
  }
  if Is16Bit then
    ACanvas.brush.color := NewColor(ACanvas, clBtnFace, 16)
  else
    ACanvas.brush.color := clBtnFace;

  if not IsTransparent then
    ACanvas.FillRect(ARect);

  HasBorder := false;
  HasBkg := false;

  if (cdsHot in State) then
  begin
    if (cdsChecked in State) or (Button.Down) or (cdsSelected in State) then
      ACanvas.Brush.Color := FCheckedAreaSelectColor
    else
      ACanvas.brush.color := FBSelectColor;
    HasBorder := true;
    HasBkg := true;
  end;

  if (cdsChecked in State) and not (cdsHot in State) then
  begin
    ACanvas.Brush.Color := FCheckedAreaColor;
    HasBorder := true;
    HasBkg := true;
  end;

  if (cdsIndeterminate in State) and not (cdsHot in State) then
  begin
    ACanvas.Brush.Color := FBSelectColor;
    HasBkg := true;
  end;

  if (Button.MenuItem <> nil) and (State = []) then
  begin
    ACanvas.brush.color := FFMenuBarColor;
    if not IsTransparent then
      HasBkg := true;
  end;

  Inc(ARect.Top, 1);

  if HasBkg then
    ACanvas.FillRect(ARect);

  if HasBorder then
    DrawBorder;

  if (Button.MenuItem <> nil)
    and (cdsSelected in State) then
  begin
    DrawTopMenuItem(Button, ACanvas, ARect, false);
    DefaultDraw := false;
  end;

  ARect := HPreviousRect;
  DefaultDraw := false;

  if Button.Style = tbsDropDown then
  begin
    ACanvas.Pen.Color := clBlack;
    DrawArrow(ACanvas, (ARect.Right - 14) + ((14 - 5) div 2),
      ARect.Top + ((ARect.Bottom - ARect.Top - 3) div 2) + 1);
  end;

  BitmapWidth := 0;
  if HasBitmap then
  begin

    try
      B := TBitmap.Create;

      B.Width := TToolBar(Button.Parent).Images.Width;
      B.Height := TToolBar(Button.Parent).Images.Height;
      B.Canvas.Brush.Color := ACanvas.Brush.Color;
      B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
      ImageList_DrawEx(TToolBar(Button.Parent).Images.Handle, Button.ImageIndex,
        B.Canvas.Handle, 0, 0, 0, 0, clNone, clNone, ILD_Transparent);

      BitmapWidth := b.Width;

      if Button.Style = tbsDropDown then
        Dec(ARect.Right, 12);

      if TToolBar(Button.Parent).List then
      begin

        if Button.BiDiMode = bdRightToLeft then
        begin
          Dec(ARect.Right, 3);
          ARect.Left := ARect.Right - BitmapWidth;

        end
        else
        begin
          Inc(ARect.Left, 3);
          ARect.Right := ARect.Left + BitmapWidth
        end

      end
      else
        ARect.Left := Round(ARect.Left + (ARect.Right - ARect.Left - B.Width) / 2);

      inc(ARect.Top, 2);
      ARect.Bottom := ARect.Top + B.Height + 6;

      DrawIcon(Button, ACanvas, B, ARect, (cdsHot in State),
        (cdsSelected in State), Button.Enabled, (cdsChecked in State), false,
        false);
    finally
      B.Free;
    end;
    ARect := HPreviousRect;
    DefaultDraw := false;
  end;
//-----------

  if TToolBar(Button.Parent).ShowCaptions then
  begin

    if Button.Style = tbsDropDown then
      Dec(ARect.Right, 12);

    if not TToolBar(Button.Parent).List then
    begin
      TextFormat := DT_Center;
      ARect.Top := ARect.Bottom - ACanvas.TextHeight(Button.Caption) - 3;
    end
    else
    begin
      TextFormat := DT_VCENTER;
      if Button.BiDiMode = bdRightToLeft then
      begin
        TextFormat := TextFormat + DT_Right;
        Dec(ARect.Right, BitmapWidth + 7);
      end
      else
      begin
        Inc(ARect.Left, BitmapWidth + 6);
      end

    end;

    if (Button.MenuItem <> nil) then
    begin
      TextFormat := DT_Center;

    end;

    if Button.BiDiMode = bdRightToLeft then
      TextFormat := TextFormat + DT_RTLREADING;

    DrawTheText(Button.Caption, '',
      ACanvas, ARect,
      (cdsSelected in State), Button.Enabled, false,
      (Button.MenuItem <> nil),
      (Button.BidiMode = bdRightToLeft), TextFormat);

    ARect := HPreviousRect;
    DefaultDraw := false;
  end;

  if Button.Index > 0 then
  begin
    XButton := TToolBar(Button.Parent).Buttons[Button.Index - 1];
    if (XButton.Style = tbsDivider) or (XButton.Style = tbsSeparator) then
    begin
      ARect := XButton.BoundsRect;
      if Is16Bit then
        ACanvas.brush.color := NewColor(ACanvas, clBtnFace, 16)
      else
        ACanvas.brush.color := clBtnFace;

      if not IsTransparent then
        ACanvas.FillRect(ARect);
     // if (XButton.Style = tbsDivider) then  // Can't get it.
      if XButton.Tag > 0 then
      begin
        Inc(ARect.Top, 2);
        Dec(ARect.Bottom, 1);

        ACanvas.Pen.color := FFDisabledColor;
        ARect.Left := ARect.Left + (ARect.Right - ARect.Left) div 2;
        ACanvas.MoveTo(ARect.Left, ARect.Top);
        ACanvas.LineTo(ARect.Left, ARect.Bottom);

      end;
      ARect := Button.BoundsRect;
      DefaultDraw := false;
    end;

  end;

  if Button.MenuItem <> nil then
    ActivateMenuItem(Button.MenuItem);
end;
{$ENDIF}

procedure TXPBarMenu.SetGlobalColor(ACanvas: TCanvas);
begin
//-----

  if GetDeviceCaps(ACanvas.Handle, BITSPIXEL) < 16 then
    Is16Bit := false
  else
    Is16Bit := true;

  FFColor := FColor;
  FFIconBackColor := FIconBackColor;

  FFSelectColor := FSelectColor;

  if Is16Bit then
  begin
    FCheckedAreaColor := NewColor(ACanvas, FSelectColor, 75);
    FCheckedAreaSelectColor := NewColor(ACanvas, FSelectColor, 50);

    FMenuBorderColor := GetShadeColor(ACanvas, clBtnFace, 90);
    FMenuShadowColor := GetShadeColor(ACanvas, clBtnFace, 76);
  end
  else
  begin
    FFSelectColor := FSelectColor;
    FCheckedAreaColor := clWhite;
    FCheckedAreaSelectColor := clSilver;
    FMenuBorderColor := clBtnShadow;
    FMenuShadowColor := clBtnShadow;
  end;

  FFSelectBorderColor := FSelectBorderColor;
  FFSelectFontColor := FSelectFontColor;
  FFMenuBarColor := FMenuBarColor;
  FFDisabledColor := FDisabledColor;
  FFCheckedColor := FCheckedColor;
  FFSeparatorColor := FSeparatorColor;

  if FUseSystemColors then
  begin
    GetSystemMenuFont(FFont);
    FFSelectFontColor := FFont.Color;
    if not Is16Bit then
    begin
      FFColor := clWhite;
      FFIconBackColor := clBtnFace;
      FFSelectColor := clWhite;
      FFSelectBorderColor := clHighlight;
      FFMenuBarColor := FFIconBackColor;
      FFDisabledColor := clBtnShadow;
      FFCheckedColor := clHighlight;
      FFSeparatorColor := clBtnShadow;
      FCheckedAreaColor := clWhite;
      FCheckedAreaSelectColor := clWhite;

    end
    else
    begin
      FFColor := NewColor(ACanvas, clBtnFace, 86);
      FFIconBackColor := NewColor(ACanvas, clBtnFace, 16);
      FFSelectColor := NewColor(ACanvas, clHighlight, 68);
      FFSelectBorderColor := clHighlight;
      FFMenuBarColor := clMenu;

      FFDisabledColor := NewColor(ACanvas, clBtnShadow, 10);
      FFSeparatorColor := NewColor(ACanvas, clBtnShadow, 25);
      FFCheckedColor := clHighlight;
      FCheckedAreaColor := NewColor(ACanvas, clHighlight, 75);
      FCheckedAreaSelectColor := NewColor(ACanvas, clHighlight, 50);

    end;
  end;

end;

procedure TXPBarMenu.DrawTopMenuItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; IsRightToLeft: boolean);
var
  X1, X2            : integer;
  DefColor, HoldColor: TColor;
begin
  X1 := ARect.Left;
  X2 := ARect.Right;

  ACanvas.brush.Style := bsSolid;
  ACanvas.brush.color := FFIconBackColor;

  ACanvas.FillRect(ARect);
  ACanvas.Pen.Color := FMenuBorderColor;

  if (not IsRightToLeft) and (Is16Bit) and (Sender is TMenuItem) then
  begin
    ACanvas.MoveTo(X1, ARect.Bottom - 1);
    ACanvas.LineTo(X1, ARect.Top);
    ACanvas.LineTo(X2 - 8, ARect.Top);
    ACanvas.LineTo(X2 - 8, ARect.Bottom);

    DefColor := FFMenuBarColor;

    HoldColor := GetShadeColor(ACanvas, DefColor, 10);
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := HoldColor;
    ACanvas.Pen.Color := HoldColor;

    ACanvas.FillRect(Rect(X2 - 7, ARect.Top, X2, ARect.Bottom));

    HoldColor := GetShadeColor(ACanvas, DefColor, 30);
    ACanvas.Brush.Color := HoldColor;
    ACanvas.Pen.Color := HoldColor;
    ACanvas.FillRect(Rect(X2 - 7, ARect.Top + 3, X2 - 2, ARect.Bottom));

    HoldColor := GetShadeColor(ACanvas, DefColor, 40 + 20);
    ACanvas.Brush.Color := HoldColor;
    ACanvas.Pen.Color := HoldColor;
    ACanvas.FillRect(Rect(X2 - 7, ARect.Top + 5, X2 - 3, ARect.Bottom));

    HoldColor := GetShadeColor(ACanvas, DefColor, 60 + 40);
    ACanvas.Brush.Color := HoldColor;
    ACanvas.Pen.Color := HoldColor;
    ACanvas.FillRect(Rect(X2 - 7, ARect.Top + 6, X2 - 5, ARect.Bottom));

    //---

    ACanvas.Pen.Color := DefColor;
    ACanvas.MoveTo(X2 - 5, ARect.Top + 1);
    ACanvas.LineTo(X2 - 1, ARect.Top + 1);
    ACanvas.LineTo(X2 - 1, ARect.Top + 6);

    ACanvas.MoveTo(X2 - 3, ARect.Top + 2);
    ACanvas.LineTo(X2 - 2, ARect.Top + 2);
    ACanvas.LineTo(X2 - 2, ARect.Top + 3);
    ACanvas.LineTo(X2 - 3, ARect.Top + 3);

    ACanvas.Pen.Color := GetShadeColor(ACanvas, DefColor, 10);
    ACanvas.MoveTo(X2 - 6, ARect.Top + 3);
    ACanvas.LineTo(X2 - 3, ARect.Top + 3);
    ACanvas.LineTo(X2 - 3, ARect.Top + 6);
    ACanvas.LineTo(X2 - 4, ARect.Top + 6);
    ACanvas.LineTo(X2 - 4, ARect.Top + 3);

    ACanvas.Pen.Color := GetShadeColor(ACanvas, DefColor, 30);
    ACanvas.MoveTo(X2 - 5, ARect.Top + 5);
    ACanvas.LineTo(X2 - 4, ARect.Top + 5);
    ACanvas.LineTo(X2 - 4, ARect.Top + 9);

    ACanvas.Pen.Color := GetShadeColor(ACanvas, DefColor, 40);
    ACanvas.MoveTo(X2 - 6, ARect.Top + 5);
    ACanvas.LineTo(X2 - 6, ARect.Top + 7);

  end
  else
  begin
    ACanvas.Pen.Color := FMenuBorderColor;
    ACanvas.Brush.Color := FMenuShadowColor;

    ACanvas.MoveTo(X1, ARect.Bottom - 1);
    ACanvas.LineTo(X1, ARect.Top);
    ACanvas.LineTo(X2 - 3, ARect.Top);
    ACanvas.LineTo(X2 - 3, ARect.Bottom);

    ACanvas.Pen.Color := ACanvas.Brush.Color;
    ACanvas.FillRect(Rect(X2 - 2, ARect.Top + 2, X2, ARect.Bottom));
  end;

end;

procedure TXPBarMenu.DrawCheckedItem(FMenuItem: TMenuItem; Selected,
  HasImgLstBitmap: boolean; ACanvas: TCanvas; CheckedRect: TRect);
var
  X1, X2            : integer;
begin
  if FMenuItem.RadioItem then
  begin
    if FMenuItem.Checked then
    begin

      ACanvas.Pen.color := FFSelectBorderColor;
      if selected then
        ACanvas.Brush.Color := FCheckedAreaSelectColor
      else
        ACanvas.Brush.Color := FCheckedAreaColor;
      ACanvas.Brush.Style := bsSolid;
      if HasImgLstBitmap then
      begin
        ACanvas.RoundRect(CheckedRect.Left, CheckedRect.Top,
          CheckedRect.Right, CheckedRect.Bottom,
          6, 6);
      end
      else
      begin
        ACanvas.Ellipse(CheckedRect.Left, CheckedRect.Top,
          CheckedRect.Right, CheckedRect.Bottom);
      end;
    end;
  end
  else
  begin
    if (FMenuItem.Checked) then
      if (not HasImgLstBitmap) then
      begin
        ACanvas.Pen.color := FFCheckedColor;
        if selected then
          ACanvas.Brush.Color := FCheckedAreaSelectColor
        else
          ACanvas.Brush.Color := FCheckedAreaColor; ;
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Rectangle(CheckedRect.Left, CheckedRect.Top,
          CheckedRect.Right, CheckedRect.Bottom);
        ACanvas.Pen.color := clBlack;
        x1 := CheckedRect.Left + 1;
        x2 := CheckedRect.Top + 5;
        ACanvas.MoveTo(x1, x2);

        x1 := CheckedRect.Left + 4;
        x2 := CheckedRect.Bottom - 2;
        ACanvas.LineTo(x1, x2);
           //--
        x1 := CheckedRect.Left + 2;
        x2 := CheckedRect.Top + 5;
        ACanvas.MoveTo(x1, x2);

        x1 := CheckedRect.Left + 4;
        x2 := CheckedRect.Bottom - 3;
        ACanvas.LineTo(x1, x2);
           //--
        x1 := CheckedRect.Left + 2;
        x2 := CheckedRect.Top + 4;
        ACanvas.MoveTo(x1, x2);

        x1 := CheckedRect.Left + 5;
        x2 := CheckedRect.Bottom - 3;
        ACanvas.LineTo(x1, x2);
           //-----------------

        x1 := CheckedRect.Left + 4;
        x2 := CheckedRect.Bottom - 3;
        ACanvas.MoveTo(x1, x2);

        x1 := CheckedRect.Right + 2;
        x2 := CheckedRect.Top - 1;
        ACanvas.LineTo(x1, x2);
           //--
        x1 := CheckedRect.Left + 4;
        x2 := CheckedRect.Bottom - 2;
        ACanvas.MoveTo(x1, x2);

        x1 := CheckedRect.Right - 2;
        x2 := CheckedRect.Top + 3;
        ACanvas.LineTo(x1, x2);

      end
      else
      begin
        ACanvas.Pen.color := FFSelectBorderColor;
        if selected then
          ACanvas.Brush.Color := FCheckedAreaSelectColor
        else
          ACanvas.Brush.Color := FCheckedAreaColor;
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Rectangle(CheckedRect.Left, CheckedRect.Top,
          CheckedRect.Right, CheckedRect.Bottom);
      end;
  end;

end;

procedure TXPBarMenu.DrawTheText(txt, ShortCuttext: string; ACanvas: TCanvas; TextRect: TRect;
  Selected, Enabled, Default, TopMenu, IsRightToLeft: boolean; TextFormat: integer);
var
  DefColor          : TColor;
begin
  DefColor := FFont.Color;

  ACanvas.Font.Assign(FFont);

  if Enabled then
    DefColor := FFont.Color;

  if Selected then
    DefColor := FFSelectFontColor;

  if not Enabled then
  begin
    DefColor := FFDisabledColor;
    //if Selected then
    //  if Is16Bit then
    //    DefColor := NewColor(ACanvas, FFDisabledColor, 10);
  end;

  if (TopMenu and Selected) then
    DefColor := TopMenuFontColor(ACanvas, FFIconBackColor);

  ACanvas.Font.color := DefColor; // will not affect Buttons

  TextRect.Top := TextRect.Top +
    ((TextRect.Bottom - TextRect.Top) - ACanvas.TextHeight('W')) div 2;

  SetBkMode(ACanvas.Handle, TRANSPARENT);

  if Default and Enabled then
  begin

    Inc(TextRect.Left, 1);
    ACanvas.Font.Style := [fsBold] + Font.Style; /// This Line Add By Kingron
    ACanvas.Font.color := GetShadeColor(ACanvas,
      ACanvas.Pixels[TextRect.Left, TextRect.Top], 30);
    DrawtextEx(ACanvas.Handle,
      PChar(txt),
      Length(txt),
      TextRect, TextFormat, nil);
    Dec(TextRect.Left, 1);

    Inc(TextRect.Top, 2);
    Inc(TextRect.Left, 1);
    Inc(TextRect.Right, 1);

    ACanvas.Font.color := GetShadeColor(ACanvas,
      ACanvas.Pixels[TextRect.Left, TextRect.Top], 30);
    DrawtextEx(ACanvas.Handle,
      PChar(txt),
      Length(txt),
      TextRect, TextFormat, nil);

    Dec(TextRect.Top, 1);
    Dec(TextRect.Left, 1);
    Dec(TextRect.Right, 1);

    ACanvas.Font.color := GetShadeColor(ACanvas,
      ACanvas.Pixels[TextRect.Left, TextRect.Top], 40);
    DrawtextEx(ACanvas.Handle,
      PChar(txt),
      Length(txt),
      TextRect, TextFormat, nil);

    Inc(TextRect.Left, 1);
    Inc(TextRect.Right, 1);

    ACanvas.Font.color := GetShadeColor(ACanvas,
      ACanvas.Pixels[TextRect.Left, TextRect.Top], 60);
    DrawtextEx(ACanvas.Handle,
      PChar(txt),
      Length(txt),
      TextRect, TextFormat, nil);

    Dec(TextRect.Left, 1);
    Dec(TextRect.Right, 1);
    Dec(TextRect.Top, 1);

    ACanvas.Font.color := DefColor;
  end;

  DrawtextEx(ACanvas.Handle, PChar(txt), Length(txt), TextRect, TextFormat, nil);

  txt := ShortCutText + ' ';

  if not Is16Bit then
    ACanvas.Font.color := DefColor
  else
    ACanvas.Font.color := GetShadeColor(ACanvas, DefColor, -40);

  if IsRightToLeft then
  begin
    Inc(TextRect.Left, 10);
    TextFormat := DT_LEFT
  end
  else
  begin
    Dec(TextRect.Right, 10);
    TextFormat := DT_RIGHT;
  end;

  DrawtextEx(ACanvas.Handle,
    PChar(txt),
    Length(txt),
    TextRect, TextFormat, nil);

end;

procedure TXPBarMenu.DrawIcon(Sender: TObject; ACanvas: TCanvas; B: TBitmap;
  IconRect: Trect; Hot, Selected, Enabled, Checked, FTopMenu,
  IsRightToLeft: boolean);
var
  DefColor          : TColor;
  X1, X2            : integer;
begin
  if B <> nil then
  begin
    X1 := IconRect.Left;
    X2 := IconRect.Top + 2;
    if Sender is TMenuItem then
    begin
      inc(X2, 2);
      if FIconWidth >= B.Width then
        X1 := X1 + ((FIconWidth - B.Width) div 2) - 1
      else
      begin
        if IsRightToLeft then
          X1 := IconRect.Right - b.Width - 2
        else
          X1 := IconRect.Left + 2;
      end;
    end;

    if (Hot) and (not FTopMenu) and (Enabled) and (not Checked) then
      if not Selected then
      begin
        dec(X1, 1);
        dec(X2, 1);
      end;

{$IFDEF WIN32} /// This Line Add By Kingron
/// Under Lines Cause some Problam in Win9x,Bitmap Transparent Bug
    if (not Hot) and (Enabled) and (not Checked) and _IsOSoK then
      if Is16Bit then
        DimBitmap(B, 30);

/// Under Two Line Cause some problam in Win9x
    if not Enabled and _IsOSoK then
        GrayBitmap(B, 70);
{$ENDIF} /// This Line Add By Kingron
    if (Hot) and (Enabled) and (not Checked) then
    begin
      if (Is16Bit) and (not UseSystemColors) and (Sender is TToolButton) then
        DefColor := NewColor(ACanvas, FSelectColor, 68)
      else
        DefColor := FFSelectColor;

      DefColor := GetShadeColor(ACanvas, DefColor, 50);
      DrawBitmapShadow(B, ACanvas, X1 + 2, X2 + 2, DefColor);
    end;

    B.Transparent := True;
    ACanvas.Draw(X1, X2, B);

  end;

end;

procedure TXPBarMenu.DrawArrow(ACanvas: TCanvas; X, Y: integer);
begin
  ACanvas.MoveTo(X, Y);
  ACanvas.LineTo(X + 4, Y);

  ACanvas.MoveTo(X + 1, Y + 1);
  ACanvas.LineTo(X + 4, Y);

  ACanvas.MoveTo(X + 2, Y + 2);
  ACanvas.LineTo(X + 3, Y);

end;

function TXPBarMenu.TopMenuFontColor(ACanvas: TCanvas; Color: TColor): TColor;
var
  r, g, b, avg      : integer;
begin

  Color := ColorToRGB(Color);
  r := Color and $000000FF;
  g := (Color and $0000FF00) shr 8;
  b := (Color and $00FF0000) shr 16;

  Avg := (r + b) div 2;

  if (Avg > 150) or (g > 200) then
    Result := FFont.Color
  else
    Result := NewColor(ACanvas, Color, 90);
   // Result := FColor;
end;

procedure TXPBarMenu.SetActive(const Value: boolean);
begin

  FActive := Value;

  if FActive then
  begin
    InitMenueItems(FForm, false);
    InitMenueItems(FForm, true);
  end
  else
    InitMenueItems(FForm, false);

  if FForm <> nil then
    Windows.DrawMenuBar(FForm.Handle);
end;

procedure TXPBarMenu.SetForm(const Value: TForm);
var
  Hold              : boolean;
begin
  if Value <> FForm then
  begin
    Hold := Active;
    Active := false;
    FForm := Value;
    if Hold then
      Active := True;
  end;
end;

procedure TXPBarMenu.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  DrawMenuBar(FForm.Handle);
end;

procedure TXPBarMenu.SetMenuBarColor(const Value: TColor);
begin
  FMenuBarColor := Value;
  Windows.DrawMenuBar(FForm.Handle);
end;

procedure TXPBarMenu.SetOverrideOwnerDraw(const Value: boolean);
begin
  FOverrideOwnerDraw := Value;
  if FActive then
    Active := True;
end;

procedure TXPBarMenu.SetUseSystemColors(const Value: boolean);
begin
  FUseSystemColors := Value;
  Windows.DrawMenuBar(FForm.Handle);
end;

procedure GetSystemMenuFont(Font: TFont);
var
  FNonCLientMetrics : TNonCLientMetrics;
  FFont:TFont;
begin
  FFont:=TFont.Create;
  FNonCLientMetrics.cbSize := Sizeof(TNonCLientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @FNonCLientMetrics, 0) then
  begin
    FFont.Handle := CreateFontIndirect(FNonCLientMetrics.lfMenuFont);
    FFont.Color := clMenuText;
    if FFont.Name = 'MS Sans Serif' then
      FFont.Name := 'Tahoma';
  end;
  Font.Assign(FFont);
  FFont.Free;
end;

procedure TXPBarMenu.DrawGradient(ACanvas: TCanvas; ARect: TRect;
  IsRightToLeft: boolean);
var
  i                 : integer;
  v                 : integer;
  BRect             : TRect;
  B                 : TBitmap;
begin
  /// The modify by Kingron,Use Double Buffer to improce the Draw Speed
  B:=TBitmap.Create;
  V := 0;
  B.Height :=ARect.Bottom - ARect.Top;
  B.Width := ARect.Right - ARect.Left;
  BRect := Rect(0,0,B.Width -1 ,B.Height -1);
  if IsRightToLeft then
  begin
    BRect.Left := BRect.Right -1 ;
    for i := ARect.Right downto ARect.Left do
    begin
      if (BRect.Left < ARect.Right)
        and (BRect.Left > ARect.Right - FIconWidth + 5) then
        inc(v, 3)
      else
        inc(v, 1);

      if v > 96 then v := 96;
      B.Canvas.Brush.Color := NewColor(B.Canvas, FFIconBackColor, v);
      B.Canvas.FillRect(BRect);

      Dec(BRect.Left);
      BRect.Right := BRect.Left - 1;
    end;
    ACanvas.CopyRect(ARect,B.Canvas,Rect(0,0,B.Width -1 ,B.Height -1));
  end
  else
  begin
    BRect.Right := BRect.Left +1;
    for i := ARect.Left to ARect.Right do
    begin
      if (BRect.Left > ARect.Left)
        and (BRect.Left < ARect.Left + FIconWidth + 5) then
        inc(v, 3)
      else
        inc(v, 1);

      if v > 96 then v := 96;
      B.Canvas.Brush.Color := NewColor(B.Canvas, FFIconBackColor, v);
      B.Canvas.FillRect(BRect);

      Inc(BRect.Left);
      BRect.Right := BRect.Left + 1;
    end;
    ACanvas.CopyRect(ARect,B.Canvas,Rect(0,0,B.Width -1 ,B.Height -1));
  end;
  B.Free;

end;

procedure TXPBarMenu.DrawWindowBorder(hWnd: HWND; IsRightToLeft: boolean);
var
  WRect, CRect      : TRect;
  dCanvas           : TCanvas;
begin

  if hWnd <= 0 then exit;
  dCanvas := nil;
  try
    dCanvas := TCanvas.Create;
    dCanvas.Handle := GetDc(0);

    GetClientRect(hWnd, CRect);
    GetWindowRect(hWnd, WRect);

    ExcludeClipRect(dCanvas.Handle, CRect.Left, CRect.Top, CRect.Right,
      CRect.Bottom);

    dCanvas.Brush.Style := bsClear;

    Dec(WRect.Right, 2);
    Dec(WRect.Bottom, 2);

    dCanvas.Pen.Color := FMenuBorderColor;
    dCanvas.Rectangle(WRect.Left, WRect.Top, WRect.Right, WRect.Bottom);

    if IsRightToLeft then
    begin
      dCanvas.Pen.Color := FFColor;
      dCanvas.Rectangle(WRect.Left + 1, WRect.Top + 1, WRect.Right - 2,
        WRect.Top + 3);

      dCanvas.MoveTo(WRect.Left + 2, WRect.Top + 2);
      dCanvas.LineTo(WRect.Left + 2, WRect.Bottom - 2);

      dCanvas.Pen.Color := FFIconBackColor;
      dCanvas.MoveTo(WRect.Right - 2, WRect.Top + 2);
      dCanvas.LineTo(WRect.Right - 2, WRect.Bottom - 2);

      dCanvas.MoveTo(WRect.Right - 2, WRect.Top + 2);
      dCanvas.LineTo(WRect.Right - 1 - FIconWidth, WRect.Top + 2);
    end
    else
    begin
      if not FGradient then
      begin
        dCanvas.Pen.Color := FFColor;
        dCanvas.Rectangle(WRect.Left + 1, WRect.Top + 1, WRect.Right - 2,
          WRect.Top + 3);

        dCanvas.Pen.Color := FFIconBackColor;
        dCanvas.MoveTo(WRect.Left + 1, WRect.Top + 2);
        dCanvas.LineTo(WRect.Left + 2 + FIconWidth, WRect.Top + 2);
      end;

      dCanvas.Pen.Color := FFIconBackColor;
      dCanvas.MoveTo(WRect.Left + 1, WRect.Top + 1);
      dCanvas.LineTo(WRect.Left + 1, WRect.Bottom - 2);

    end;

    Inc(WRect.Right, 2);
    Inc(WRect.Bottom, 2);

    dCanvas.Pen.Color := FMenuShadowColor;
    dCanvas.Rectangle(WRect.Left + 2, WRect.Bottom, WRect.Right, WRect.Bottom - 2);
    dCanvas.Rectangle(WRect.Right - 2, WRect.Bottom, WRect.Right, WRect.Top + 2);

    dCanvas.Pen.Color := FFIconBackColor;
    dCanvas.Rectangle(WRect.Left, WRect.Bottom - 2, WRect.Left + 2, WRect.Bottom);
    dCanvas.Rectangle(WRect.Right - 2, WRect.Top, WRect.Right, WRect.Top + 2);
  finally
    IntersectClipRect(dCanvas.Handle, WRect.Left, WRect.Top, WRect.Right, WRect.Bottom);
    /// Under Line Add By Kingron ,to Fix Memory Hole Bug!!!!!
    ReleaseDC(0,dCanvas.Handle);
    dCanvas.Free;
  end;

end;

procedure TXPBarMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if not FAutoDetect then exit;
  if (Operation = opInsert) and
    ((AComponent is TMenuItem) or (AComponent is TToolButton)) then
  begin
    if (csDesigning in ComponentState) then
      Active := true
    else
     //if ComponentState = [] then
      Active := true;
  end;

end;

function GetShadeColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
var
  r, g, b           : integer;

begin
  clr := ColorToRGB(clr);
  r := Clr and $000000FF;
  g := (Clr and $0000FF00) shr 8;
  b := (Clr and $00FF0000) shr 16;

  r := (r - value);
  if r < 0 then r := 0;
  if r > 255 then r := 255;

  g := (g - value) + 2;
  if g < 0 then g := 0;
  if g > 255 then g := 255;

  b := (b - value);
  if b < 0 then b := 0;
  if b > 255 then b := 255;

  Result := Windows.GetNearestColor(ACanvas.Handle, RGB(r, g, b));
end;

function NewColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
var
  r, g, b           : integer;

begin
  if Value > 100 then Value := 100;
  clr := ColorToRGB(clr);
  r := Clr and $000000FF;
  g := (Clr and $0000FF00) shr 8;
  b := (Clr and $00FF0000) shr 16;

  r := r + Round((255 - r) * (value / 100));
  g := g + Round((255 - g) * (value / 100));
  b := b + Round((255 - b) * (value / 100));

  Result := Windows.GetNearestColor(ACanvas.Handle, RGB(r, g, b));
end;

function GrayColor(ACanvas: TCanvas; clr: TColor; Value: integer): TColor;
var
  r, g, b, avg      : integer;

begin
  if Value > 100 then Value := 100;
  clr := ColorToRGB(clr);
  r := Clr and $000000FF;
  g := (Clr and $0000FF00) shr 8;
  b := (Clr and $00FF0000) shr 16;

  Avg := (r + g + b) div 3;
  Avg := Avg + Value;

  if Avg > 240 then Avg := 240;

  Result := Windows.GetNearestColor(ACanvas.Handle, RGB(Avg, avg, avg));
end;

procedure GrayBitmap(ABitmap: TBitmap; Value: integer);
var
  x, y              : integer;
  LastColor1, LastColor2, Color: TColor;
begin
  LastColor1 := 0;
  LastColor2 := 0;

  for y := 0 to ABitmap.Height do
    for x := 0 to ABitmap.Width do
    begin
      Color := ABitmap.Canvas.Pixels[x, y];
      if Color = LastColor1 then
        ABitmap.Canvas.Pixels[x, y] := LastColor2
      else
      begin
        LastColor2 := GrayColor(ABitmap.Canvas, Color, Value);
        ABitmap.Canvas.Pixels[x, y] := LastColor2;
        LastColor1 := Color;
      end;
    end;
end;

procedure DimBitmap(ABitmap: TBitmap; Value: integer);
var
  x, y              : integer;
  LastColor1, LastColor2, Color: TColor;
begin
  if Value > 100 then Value := 100;
  LastColor1 := -1;
  LastColor2 := -1;

  for y := 0 to ABitmap.Height - 1 do
    for x := 0 to ABitmap.Width - 1 do
    begin
      Color := ABitmap.Canvas.Pixels[x, y];
      if Color = LastColor1 then
        ABitmap.Canvas.Pixels[x, y] := LastColor2
      else
      begin
        LastColor2 := NewColor(ABitmap.Canvas, Color, Value);
        ABitmap.Canvas.Pixels[x, y] := LastColor2;
        LastColor1 := Color;
      end;
    end;
end;

procedure DrawBitmapShadow(B: TBitmap; ACanvas: TCanvas; X, Y: integer;
  ShadowColor: TColor);
var
  BX, BY            : integer;
  TransparentColor  : TColor;
begin
  TransparentColor := B.Canvas.Pixels[0, B.Height - 1];
  for BY := 0 to B.Height - 1 do
    for BX := 0 to B.Width - 1 do
    begin
      if B.Canvas.Pixels[BX, BY] <> TransparentColor then
        ACanvas.Pixels[X + BX, Y + BY] := ShadowColor;

    end;
end;

/// The Procedure DrawBar Add By Kingron

procedure TXPBarMenu.DrawBar(Sender: TObject; ACanvas: TCanvas; ARect: TRect);
var
  NeedDrawBar       : boolean;

  procedure DrawBarText; /// Draw Text
  var
    i               : word;
    DR, DG, DB      : integer;
    R, G, B         : integer;
    dy, y           : real;
    lf              : TLogFont;
    tf              : TFont;
    Bmp             : TBitmap;
  begin
    /// Draw Dither Back Color
    /// FBarColorStep used for Win9x, To Adjust the Draw Speed
    Bmp := TBitmap.Create;
    Bmp.Height :=ARect.Bottom-ARect.Top;
    Bmp.Width := ARect.Right - ARect.Left;

    R := GetRValue(ColorToRGB(FBarColorEnd));
    G := GetGValue(ColorToRGB(FBarColorEnd));
    B := GetBValue(ColorToRGB(FBarColorEnd));
    DR := (R - GetRValue(ColorToRGB(FBarColorStart))) div FBarColorStep;
    DG := (G - GetGValue(ColorToRGB(FBarColorStart))) div FBarColorStep;
    DB := (B - GetBValue(ColorToRGB(FBarColorStart))) div FBarColorStep;
    dy := (ARect.Bottom - ARect.Top) / FBarColorStep;
    y := 0;
    for i := FBarColorStep downto 0 do
    begin
      Bmp.Canvas.brush.color := RGB(i * DR + R, i * DG + G, i * DB + B);
      Bmp.Canvas.fillrect(rect(0, round(y), ARect.Right - ARect.Left, round(y + dy)));
      y := y + dy;
    end;
    /// Draw Caption
    with Bmp.Canvas do
    begin
      Brush.Style := bsClear;
      Font.Assign(FBarFont);
      tf := TFont.Create;
      tf.Assign(Font);
      GetObject(tf.Handle, sizeof(lf), @lf);
      lf.lfEscapement := 900;
      tf.Handle := CreateFontIndirect(lf);
      Font.Assign(tf);
      tf.Free;
      TextOut(ARect.Left + 2, ARect.Bottom - 5, FBarCaption);
    end;
    ACanvas.CopyRect(ARect,Bmp.Canvas,Rect(0,0,Bmp.Width - 1,Bmp.Height - 1));
    Bmp.Free;
  end;

  procedure DrawBarBitmap; /// Draw Bitmap
  var
    X, Y, W, H      : integer;
    Y2              : integer;
  begin
    X := ARect.Left;
    Y := ARect.Top;
    W := ARect.Right - X;
    H := ARect.Bottom - Y;
    Y2 := 0;

    /// Stretch Draw the Bitmap
    if FBarStretch then
      StretchBlt(ACanvas.Handle, X, Y - 2, W, H, FBarBitmap.Canvas.Handle, 0, 0, FBarBitmap.Width - 1, FBarBitmap.Height - 1, SRCCOPY)
    else
    begin
      if H > FBarBitmap.Height then
        Inc(Y, H - FBarBitmap.Height)
      else
        Inc(Y2, FBarBitmap.Height - H);
      BitBlt(ACanvas.Handle, X, Y - 2, W, H, FBarBitmap.Canvas.Handle, 0, Y2, SRCCOPY);
    end;
  end;

begin
  NeedDrawBar := CanDrawBar;
  if Assigned(FOnDrawBar) then
    FOnDrawBar(Sender, ACanvas, ARect, NeedDrawBar);
  if not NeedDrawBar then exit; /// Don't Need to Draw
  if FBarStyle = bsText then
    DrawBarText
  else
    DrawBarBitmap;
end;

procedure TXPBarMenu.SetBarFont(const Value: TFont);
begin
  FBarFont.Assign(Value);
end;

procedure TXPBarMenu.SetBarBitmap(const Value: TBitmap);
begin
  FBarBitmap.Assign(Value);
end;

function TXPBarMenu.CanDrawBar: boolean;
begin
  Result := False;
  if (BarStyle = bsText) and (FBarCaption <> '') then
    Result := True;
  if (BarStyle = bsBitmap) and (not FBarBitmap.Empty) then
    Result := True;
  if BarStyle = bsNone then
    Result := False;
end;

procedure TXPBarMenu.SetBarColorStep(const Value: integer);
begin
  FBarColorStep := Value;
  if FBarColorStep <= 0 then
    FBarColorStep := 1;
  if FBarColorStep > 255 then
    FBarColorStep := 255;
end;

/// Os is Win2000 or Higher?
function TXPBarMenu._IsOSoK: boolean;
begin
  Result :=DWORD(LOBYTE(LOWORD(GetVersion)))>=5;
end;

end.

