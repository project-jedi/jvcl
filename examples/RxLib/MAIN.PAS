{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit Main;

interface

{$I jvcl.inc}

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Gauges, ExtCtrls, Menus, JvSpeedbar,
  JvAppEvent, JvSplit, JvMenus, JvComponent, JvTrayIcon, JvBDEProgress,
  JvFormPlacement, JvPicClip, JvAppStorage, JvAppRegistryStorage,
  JvExExtCtrls;

type
  TMainForm = class(TForm)
    MainMenu1: TJvMainMenu;
    FileMenu: TMenuItem;
    WindowMenu: TMenuItem;
    HelpMenu: TMenuItem;
    N4: TMenuItem;
    ExitItem: TMenuItem;
    ViewMenu: TMenuItem;
    AboutItem: TMenuItem;
    View1Item: TMenuItem;
    CustomizeItem: TMenuItem;
    View4Item: TMenuItem;
    View3Item: TMenuItem;
    StatusPanel: TPanel;
    MessagePanel: TPanel;
    Panel1: TPanel;
    ProgressPanel: TPanel;
    DbiProgress: TGauge;
    DBProgress1: TJvDBProgress;
    ToolbarMenu: TJvPopupMenu;
    Customize1: TMenuItem;
    SaveLayoutItem: TMenuItem;
    RestoreLayoutItem: TMenuItem;
    CascadeItem: TMenuItem;
    TileItem: TMenuItem;
    Toolbar: TJvSpeedBar;
    AboutBtn: TjvSpeedItem;
    TileBtn: TjvSpeedItem;
    CascadeBtn: TjvSpeedItem;
    CustomizeBtn: TjvSpeedItem;
    ExitBtn: TjvSpeedItem;
    ArrangeBtn: TjvSpeedItem;
    View1Btn: TjvSpeedItem;
    View2Btn: TjvSpeedItem;
    View3Btn: TjvSpeedItem;
    AppEvents: TJvAppEvents;
    WizardBtn: TjvSpeedItem;
    SpeedbarSection1: TjvSpeedbarSection;
    SpeedbarSection2: TjvSpeedbarSection;
    SpeedbarSection3: TjvSpeedbarSection;
    SpeedbarSection4: TjvSpeedbarSection;
    TrayIcon: TJvTrayIcon;
    PopupMenu: TJvPopupMenu;
    RXDemo1: TMenuItem;
    About1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    WizardItem: TMenuItem;
    Images: TJvPicClip;
    PopupImg: TJvPicClip;
    RXWebSite1: TMenuItem;
    JvAppRegistryStorage: TJvAppRegistryStorage;
    JvFormStorage: TJvFormStorage;
    procedure Exit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure View(Sender: TObject);
    procedure CustomizeItemClick(Sender: TObject);
    procedure SaveLayoutItemClick(Sender: TObject);
    procedure RestoreLayoutItemClick(Sender: TObject);
    procedure WindowItemClick(Sender: TObject);
    procedure AboutItemClick(Sender: TObject);
    procedure FormPlacement1RestorePlacement(Sender: TObject);
    procedure FormPlacement1SavePlacement(Sender: TObject);
    procedure AppActivate(Sender: TObject);
    procedure AppRestore(Sender: TObject);
    procedure AppSettingsChanged(Sender: TObject);
    procedure WizardBtnClick(Sender: TObject);
    procedure AppMinimize(Sender: TObject);
    procedure AppHint(Sender: TObject);
    procedure RXDemo1Click(Sender: TObject);
    procedure ToolbarMenuGetItemParams(Sender: TMenu; Item: TMenuItem;
      State: TMenuOwnerDrawState; AFont: TFont; var Color: TColor;
      var Graphic: TGraphic; var NumGlyphs: Integer);
    procedure MainMenu1GetItemParams(Sender: TMenu; Item: TMenuItem;
      State: TMenuOwnerDrawState; AFont: TFont; var Color: TColor;
      var Graphic: TGraphic; var NumGlyphs: Integer);
    procedure PopupMenuGetItemParams(Sender: TMenu; Item: TMenuItem;
      State: TMenuOwnerDrawState; AFont: TFont; var Color: TColor;
      var Graphic: TGraphic; var NumGlyphs: Integer);
//    procedure PopupMenuDrawMargin(Sender: TMenu; Rect: TRect);
    procedure RXWebSite1Click(Sender: TObject);
  private
    { Private declarations }
    procedure ActiveFormChange(Sender: TObject);
    procedure OpenForm(Sender: TObject; FormClass: TFormClass);
    procedure UpdateToolbar;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses IniFiles, Buttons, Ctrls, DBAware, Tools, About, PageDemo,
  JvxSlider, JvJVCLUtils, JvHints, ShellAPI, JvGradientCaption;

{$R *.DFM}

{$IFDEF WIN32}
{$R BACKGRND.R32}
{$ELSE}
{$R BACKGRND.R16}
{$ENDIF}

procedure TMainForm.UpdateToolbar;
begin
  if ColorToRGB(Toolbar.Color) <> ColorToRGB(clBlack) then
    Toolbar.Wallpaper.Bitmap.Handle := LoadBitmap(hInstance, 'BACKGROUND')
  else Toolbar.Wallpaper := nil;
  Toolbar.SetFontDefault;
end;

procedure TMainForm.Exit(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SetHintStyle(hsRectangle, 2, False, taCenter);
  Screen.OnActiveFormChange := ActiveFormChange;
  RegisterClasses([TControlsForm, TToolsForm, TDBAwareForm]);
  UpdateToolbar;
{$IFDEF WIN32}
  RXDemo1.Default := True;
//  GradientFormCaption(Self, clWindowText);
{$ENDIF}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Screen.OnActiveFormChange := nil;
end;

procedure TMainForm.ActiveFormChange(Sender: TObject);
var
  Btn: TjvSpeedItem;
  Item: TMenuItem;
  FormClass: TClass;
begin
  Btn := nil; Item := nil;
  CascadeItem.Enabled := MDIChildCount > 0;
  TileItem.Enabled := MDIChildCount > 0;
  CascadeBtn.Enabled := MDIChildCount > 0;
  TileBtn.Enabled := MDIChildCount > 0;
  ArrangeBtn.Enabled := MDIChildCount > 0;
  if Screen.ActiveForm <> nil then begin
    FormClass := Screen.ActiveForm.ClassType;
    if FormClass = TControlsForm then begin
      Btn := View1Btn;
      Item := View1Item;
    end
    else if FormClass = TDBAwareForm then begin
      Btn := View2Btn;
      Item := View3Item;
    end
    else if FormClass = TToolsForm then begin
      Btn := View3Btn;
      Item := View4Item;
    end;
  end;
  View1Item.Checked := False;
  View3Item.Checked := False;
  View4Item.Checked := False;
  if Item <> nil then Item.Checked := True;
  if Btn <> nil then Btn.Down := True
  else begin
    View1Btn.Down := False;
    View2Btn.Down := False;
    View3Btn.Down := False;
  end;
end;

procedure TMainForm.OpenForm(Sender: TObject; FormClass: TFormClass);
begin
  if Sender is TSpeedButton then (Sender as TSpeedButton).Down := True;
  Screen.Cursor := crHourGlass;
  try
    FindShowForm(FormClass, '');
    ActiveFormChange(nil);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.View(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1: OpenForm(Sender, TControlsForm);
    2: OpenForm(Sender, TDBAwareForm);
    3: OpenForm(Sender, TToolsForm);
  end;
end;

procedure TMainForm.WizardBtnClick(Sender: TObject);
begin
  ShowDialog(TClientAssistant);
end;

procedure TMainForm.CustomizeItemClick(Sender: TObject);
begin
  Toolbar.Customize(0);
end;

procedure TMainForm.SaveLayoutItemClick(Sender: TObject);
var
  IniFile: TIniFile;
begin
//!!!
  IniFile := TIniFile.Create(GetDefaultIniName);
  try
    Toolbar.SaveLayout;
  finally
    IniFile.Free;
  end;
end;

procedure TMainForm.RestoreLayoutItemClick(Sender: TObject);
var
  IniFile: TIniFile;
begin
//!!!
  IniFile := TIniFile.Create(GetDefaultIniName);
  try
    Toolbar.RestoreLayout;
  finally
    IniFile.Free;
  end;
end;

procedure TMainForm.WindowItemClick(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
    1: Cascade;
    2: Tile;
    3: ArrangeIcons;
  end;
end;

procedure TMainForm.AboutItemClick(Sender: TObject);
begin
  TrayIcon.Animated := False;
  try
    ShowDialog(TAboutForm);
  finally
    TrayIcon.Animated := True;
  end;
end;

procedure TMainForm.FormPlacement1RestorePlacement(Sender: TObject);
begin
//!!!  RestoreMDIChildren(Self, FormPlacement1.IniFile);
{$IFNDEF WIN32}
  AppTaskbarIcons(True);
  Application.BringToFront;
{$ENDIF}
end;

procedure TMainForm.FormPlacement1SavePlacement(Sender: TObject);
begin
//!!!  SaveMDIChildren(Self, FormPlacement1.IniFile);
end;

procedure TMainForm.AppSettingsChanged(Sender: TObject);
begin
  UpdateToolbar;
end;

procedure TMainForm.AppActivate(Sender: TObject);
begin
{$IFNDEF WIN32}
  if Screen.ActiveForm <> nil then Screen.ActiveForm.BringToFront;
{$ENDIF}
end;

procedure TMainForm.AppRestore(Sender: TObject);
begin
{$IFDEF WIN32}
  if NewStyleControls then ShowWindow(Application.Handle, SW_SHOW);
{$ENDIF}
end;

procedure TMainForm.AppMinimize(Sender: TObject);
begin
{$IFDEF WIN32}
  if NewStyleControls then ShowWindow(Application.Handle, SW_HIDE);
{$ENDIF}
end;

procedure TMainForm.AppHint(Sender: TObject);
begin
  MessagePanel.Caption := Application.Hint;
end;

procedure TMainForm.RXDemo1Click(Sender: TObject);
begin
  Application.Restore;
  Application.BringToFront;
end;

procedure TMainForm.RXWebSite1Click(Sender: TObject);
begin
  RxWebSite;
end;

procedure TMainForm.ToolbarMenuGetItemParams(Sender: TMenu; Item: TMenuItem; State: TMenuOwnerDrawState; AFont: TFont;
  var Color: TColor; var Graphic: TGraphic; var NumGlyphs: Integer);
begin
  case Item.Tag of
    1: { Customize } Graphic := CustomizeBtn.Glyph;
    2: { Save } Graphic := Images.GraphicCell[0];
    3: { Restore } Graphic := Images.GraphicCell[1];
  end;
end;

procedure TMainForm.MainMenu1GetItemParams(Sender: TMenu; Item: TMenuItem;
  State: TMenuOwnerDrawState; AFont: TFont; var Color: TColor;
  var Graphic: TGraphic; var NumGlyphs: Integer);
begin
  if Item = View1Item then Graphic := View1Btn.Glyph
  else if Item = View3Item then Graphic := View2Btn.Glyph
  else if Item = View4Item then Graphic := View3Btn.Glyph
  else if Item = WizardItem then Graphic := WizardBtn.Glyph
  else if Item = CustomizeItem then Graphic := CustomizeBtn.Glyph
  else if Item = ExitItem then Graphic := ExitBtn.Glyph
  else if Item = CascadeItem then Graphic := CascadeBtn.Glyph
  else if Item = TileItem then Graphic := TileBtn.Glyph
  else if Item = AboutItem then Graphic := AboutBtn.Glyph;
end;

procedure TMainForm.PopupMenuGetItemParams(Sender: TMenu; Item: TMenuItem;
  State: TMenuOwnerDrawState; AFont: TFont; var Color: TColor;
  var Graphic: TGraphic; var NumGlyphs: Integer);
begin
  case Item.Tag of
    1..4: Graphic := PopupImg.GraphicCell[Item.Tag - 1];
  end;
end;

{procedure TMainForm.PopupMenuDrawMargin(Sender: TMenu; Rect: TRect);
const
  Txt = 'RXLib';
begin
  with PopupMenu.Canvas.Font do begin
    Name := 'Courier New';
    Style := [fsBold];
    Size := 20;
    Color := clWhite;
    Handle := CreateRotatedFont(PopupMenu.Canvas.Font, 90);
  end;
  PopupMenu.DefaultDrawMargin(Rect, clLime, RGB(GetRValue(clLime) div 4,
    GetGValue(clLime) div 4, GetBValue(clLime) div 4));
  SetBkMode(PopupMenu.Canvas.Handle, TRANSPARENT);
  ExtTextOut(PopupMenu.Canvas.Handle, Rect.Left, Rect.Bottom - 5, ETO_CLIPPED,
    @Rect, Txt, Length(Txt), nil);
end;}

end.

