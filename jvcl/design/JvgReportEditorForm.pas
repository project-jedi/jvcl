{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgReportEditorForm.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Burov Dmitry, translation of russian text.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgReportEditorForm;

interface

uses
  Windows, Mask, Spin, Menus, ExtCtrls, StdCtrls, Buttons, ComCtrls, Controls,
  Dialogs, Forms, Classes, SysUtils, Graphics, ImgList, Printers,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, PropertyCategories,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvgReport, JvgCaption, JvgBevel, JvgPage, JvgLabel, JvgRuler,
  JvgListBox, JvgReportParamEditorForm, JvComponent, JvExControls;

type
  TJvgRepProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  TJvgReportCompEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJvgReportEditor = class(TJvComponent)
    FReport: TJvgReport;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure Preview;
    procedure Edit;
  published
    property Report: TJvgReport read FReport write FReport;
  end;

  TJvgReportEditorForm = class(TJvForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PM_Control: TPopupMenu;
    N_Linktofile: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    PC: TJvgPageControl;
    Panel1: TPanel;
    Bevel4: TBevel;
    P_Sides: TPanel;
    Panel2: TPanel;
    glBevel1: TJvgBevel;
    Bevel2: TBevel;
    Bevel1: TBevel;
    B_Label: TSpeedButton;
    sb_Open: TSpeedButton;
    sb_Save: TSpeedButton;
    sb_Preview: TSpeedButton;
    sb_Book: TSpeedButton;
    sb_Album: TSpeedButton;
    Bevel3: TBevel;
    sb_OLE: TSpeedButton;
    sb_SnapToGrid: TSpeedButton;
    b_Bevel: TSpeedButton;
    sb_Print: TSpeedButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    P_Font: TPanel;
    ColorDialog1: TColorDialog;
    N3: TMenuItem;
    N_DeleteObject: TMenuItem;
    P_HRuler: TPanel;
    P_Main: TPanel;
    ScrollBox_: TScrollBox;
    ShapeSize: TShape;
    P_VRuler: TPanel;
    TabSheet3: TTabSheet;
    ImageList1: TImageList;
    HRuler: TJvgRuler;
    VRuler: TJvgRuler;
    glBevel4: TJvgBevel;
    sb_FixAllMoving: TSpeedButton;
    sb_FixMoving: TSpeedButton;
    glLabel3: TLabel;
    N_OLESize: TMenuItem;
    N_Clip: TMenuItem;
    N_Center: TMenuItem;
    N_Scale: TMenuItem;
    N_Stretch: TMenuItem;
    N_AutoSize: TMenuItem;
    P_SBar: TPanel;
    Panel5: TPanel;
    glLabel4: TLabel;
    glLabel5: TLabel;
    glLabel7: TLabel;
    se_Width: TSpinEdit;
    se_Top: TSpinEdit;
    se_Left: TSpinEdit;
    glLabel6: TLabel;
    se_Height: TSpinEdit;
    cb_Components: TComboBox;
    glLabel8: TLabel;
    N4: TMenuItem;
    Bevel5: TBevel;
    SB_Left: TSpeedButton;
    SB_Bottom: TSpeedButton;
    SB_Right: TSpeedButton;
    SB_Top: TSpeedButton;
    sb_AlignL: TSpeedButton;
    sb_AlignC: TSpeedButton;
    sb_AlignR: TSpeedButton;
    sb_AlignW: TSpeedButton;
    RxSpeedButton8: TSpeedButton;
    RxSpeedButton9: TSpeedButton;
    sb_BevelBold: TSpeedButton;
    glBevel2: TJvgBevel;
    Panel3: TPanel;
    RxSpinEdit1: TSpinEdit;
    Panel6: TPanel;
    sbFontColor: TSpeedButton;
    glBevel3: TJvgBevel;
    Panel7: TPanel;
    Edit1: TMemo;
    FE_OLE: TEdit;
    SpeedButton1: TSpeedButton;
    OpenOLEFile: TOpenDialog;
    sb_FontUnderline: TSpeedButton;
    sb_FontItalic: TSpeedButton;
    sb_FontBold: TSpeedButton;
    TabSheet4: TTabSheet;
    glLabel1: TLabel;
    lb_Params: TJvgListBox;
    Panel4: TPanel;
    SpeedButton2: TSpeedButton;
    sbBackColor: TSpeedButton;
    SpeedButton3: TSpeedButton;
    CheckBox1: TCheckBox;
    FontComboBox1: TComboBox;
    ColorComboBox1: TComboBox;
    procedure OpenClick(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure ScrollBox_MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure SB_LeftClick(Sender: TObject);
    procedure FontComboBox1Change(Sender: TObject);
    procedure RxSpinEdit1Change(Sender: TObject);
    procedure ColorComboBox1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sb_BookClick(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure sb_FontBoldClick(Sender: TObject);
    procedure sb_AlignLClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure sbFontColorClick(Sender: TObject);
    procedure N_DeleteObjectClick(Sender: TObject);
    procedure ScrollBox_MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ScrollBox_MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sb_FixMovingClick(Sender: TObject);
    procedure N_AutoSizeClick(Sender: TObject);
    procedure sb_SnapToGridClick(Sender: TObject);
    procedure se_SizeChange(Sender: TObject);
    procedure cb_ComponentsChange(Sender: TObject);
    procedure N4Click(Sender: TObject);
    procedure sb_BevelBoldClick(Sender: TObject);
    procedure se_TopChange(Sender: TObject);
    procedure se_WidthChange(Sender: TObject);
    procedure se_HeightChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FE_OLEChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure sb_PrintClick(Sender: TObject);
    procedure sb_PreviewClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure cb_ComponentsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CheckBox1Click(Sender: TObject);
  private
    FReportParamEditor: TJvgReportParamEditor;
    FMouseDown: Boolean;
    FCanUndo: Boolean;
    FSkipSizeUpdate: Boolean;
    FUndoPosShift: TPoint;
    FSelectedControlLastPos: TPoint;
    FControlPos: TPoint;
    FSelPt: TPoint;
    FGrid: TPoint;
    FStep: TPoint;
    FScrollBox: TJvgReportScrollBox;
    FSelectedControl: TJvgReportItem;
    FSelection: Boolean;
    FSelectionRect: TRect;
    procedure RemakeComponentsList;
    procedure Read(FileName: string; ParentWnd: TWinControl);
    procedure Save(FileName: string);
    procedure OnMouseDown_(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnMouseUp_(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnMouseMove_(Sender: TObject; Shift: TShiftState;
       X, Y: Integer);
    procedure OnResize_(Sender: TObject);
    procedure OnDrawScrollBox(Sender: TObject);
    procedure UpdatePageSize;
    procedure ResizeReportControls(l, t, w, h: Integer;
      fUseParamsAsShifts: Boolean);
    procedure ShowComponentPos(Control: TControl);
    procedure AssignEventsToAllComponents;
    procedure UpdateToolBar(Control: TJvgReportItem);
  public
    Component: TJvgReport;
    procedure Preview(JvgReport: TJvgReport);
    procedure Edit(JvgReport: TJvgReport);
  end;

  TJvgPublicControl = class(TControl)
  public
    property Caption;
  end;

  TJvgPublicControlClass = class of TJvgPublicControl;

//var
//  glRepEditor: TJvgReportEditorForm;
//  Form2: TComponent;

implementation

uses
  Math,
  JvgTypes, JvgUtils, JvgAlignFunction, JvDsgnConsts, JvgAlignForm;

{$R *.dfm}

const
  IGNORE_VALUE = 65536;

//=== common proc ============================================================

procedure ShowReportEditor(JvgReport: TComponent; AEdit: Boolean);
var
  Dialog: TJvgReportEditorForm;
  Report: TJvgReport;
begin
  Dialog := TJvgReportEditorForm.Create(Application);
  try
    if JvgReport is TJvgReport then
      Report := TJvgReport(JvgReport)
    else
      Report := TJvgReportEditor(JvgReport).Report;
    if Report <> nil then
      if AEdit then
        Dialog.Edit(Report)
      else
        Dialog.Preview(Report);
  finally
    Dialog.Free;
  end;
end;

//=== TJvgComponentListProperty ==============================================

function TJvgRepProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TJvgRepProperty.GetValue: string;
begin
  Result := Format('(%s)', [GetPropType^.Name]);
end;

procedure TJvgRepProperty.Edit;
begin
  ShowReportEditor(TJvgReport(GetComponent(0)), True);
end;

//=== TJvgReportCompEditor ===================================================

procedure TJvgReportCompEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      ShowReportEditor(Component, True);
    1:
      ShowReportEditor(Component, False);
  end;
end;

function TJvgReportCompEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsEditReport;
    1:
      Result := RsPreviewReportEllipsis;
  end;
end;

function TJvgReportCompEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

//=== TJvgReportEditor =======================================================

procedure TJvgReportEditor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = Report) and (Operation = opRemove) then
    Report := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TJvgReportEditor.Preview;
begin
  ShowReportEditor(Self, False);
end;

procedure TJvgReportEditor.Edit;
begin
  ShowReportEditor(Self, True);
end;

//== TJvgReportEditorForm ====================================================

procedure TJvgReportEditorForm.Preview(JvgReport: TJvgReport);
begin
  PC.Visible := False;
  FScrollBox.Enabled := False;
  Component := JvgReport;
  ShowModal;
end;

procedure TJvgReportEditorForm.Edit(JvgReport: TJvgReport);
begin
  PC.Visible := True;
  FScrollBox.Enabled := True;
  Component := JvgReport;
  ShowModal;
end;

procedure TJvgReportEditorForm.OnMouseDown_(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Pt: TPoint;
  I: Integer;
begin
  if TControl(Sender).Cursor <> crDefault then
    Exit;
  ActiveControl := nil;
  FCanUndo := False;
  if B_Label.Down or B_Bevel.Down or sb_OLE.Down or (ssCtrl in Shift) then
  begin
    ScrollBox_MouseDown(Sender, Button, Shift, X + TControl(Sender).Left,
      Y + TControl(Sender).Top);
    Exit;
  end;
  ScrollBox_MouseDown(Sender, Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FMouseDown := True;
    TControl(Sender).Tag := 1;
    FControlPos.X := X;
    FControlPos.Y := Y;
  end
  else
  if Button = mbRight then
  begin
    OnMouseDown_(Sender, mbLeft, Shift, X, Y);
    Pt.X := X;
    Pt.Y := Y;
    Pt := FSelectedControl.ClientToScreen(Pt);
    N_Linktofile.Enabled := FSelectedControl.ContainOLE;
    N_OLESize.Enabled := FSelectedControl.ContainOLE;
    case FSelectedControl.OLESizeMode of
      0:
        N_Clip.Checked := True;
      1:
        N_Center.Checked := True;
      2:
        N_Scale.Checked := True;
      3:
        N_Stretch.Checked := True;
      4:
        N_AutoSize.Checked := True;
    end;

    PM_Control.Popup(Pt.X, Pt.Y);
    OnMouseUp_(Sender, Button, Shift, X, Y);
  end;

  if Assigned(FSelectedControl) then
    if ssShift in Shift then
    begin
      TJvgReportItem(Sender).Selected := not TJvgReportItem(Sender).Selected;
      FMouseDown := False;
      if FSelectedControl = Sender then
        Exit;
    end
    else
    begin
      if TJvgReportItem(Sender).Selected then
        Exit;
      with FScrollBox do
        for I := 0 to ControlCount - 1 do
          if (Controls[I] is TJvgReportItem) and
            TJvgReportItem(Controls[I]).Selected then
            TJvgReportItem(Controls[I]).Selected := False;
      FSelectedControl.Selected := False;
      FSelectedControl.Invalidate;
    end;

  FSelectedControl := TJvgReportItem(Sender);
  FSelectedControl.Selected := True;

  UpdateToolBar(FSelectedControl);
  FSelectedControlLastPos.X := FSelectedControl.Left;
  FSelectedControlLastPos.Y := FSelectedControl.Top;
end;

procedure TJvgReportEditorForm.OnMouseUp_(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ScrollBox_MouseUp(Sender, Button, Shift, X, Y);
  FMouseDown := False;
  if not Assigned(FSelectedControl) then
    Exit;
  FUndoPosShift.X := FSelectedControlLastPos.X - FSelectedControl.Left;
  FUndoPosShift.Y := FSelectedControlLastPos.Y - FSelectedControl.Top;
  FCanUndo := True;
  //if not Assigned(FSelectedControl) then Exit;
  //if TControl(Sender).Tag = 0 then Exit;
{  DrawFocusRect( TControl(Sender).Parent.Handle, FocusRect );
  R := Rect( 0, 0, Screen.Width, Screen.Height );
  ClipCursor(@R);
  TControl(Sender).Left := TControl(Sender).Left + X - FControlPos.X;
  TControl(Sender).Top := TControl(Sender).Top + Y - FControlPos.Y;
  if TControl(Sender).Tag = 2 then//...was moved
    TJvgReportItem(Sender).RepaintBorder;}
  TControl(Sender).Tag := 0;
end;

procedure TJvgReportEditorForm.OnMouseMove_(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  if FSelection then
    ScrollBox_MouseMove(Sender, Shift, X, Y);
  if sb_FixAllMoving.Down then
    Exit;
  //  if TControl(Sender).Tag = 0 then Exit;
  if not FMouseDown then
    Exit;
  with FScrollBox do
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TJvgReportItem then
        with TJvgReportItem(Controls[I]) do
          if Selected and (Fixed = 0) then
          begin
            Left := ((Left + X - FControlPos.X) div FStep.X) * FStep.X;
            Top := ((Top + Y - FControlPos.Y) div FStep.Y) * FStep.Y;
          end;
  FSkipSizeUpdate := True;
  ShowComponentPos(FSelectedControl);
  //TControl(Sender).Left := TControl(Sender).Left + X - FControlPos.X;
  //TControl(Sender).Top := TControl(Sender).Top + Y - FControlPos.Y;
  {
  TControl(Sender).Tag := 2;//...on moving
  DC := GetDC( TControl(Sender).Parent.Handle );
  DrawFocusRect( DC, FocusRect );
  FocusRect := Bounds( TControl(Sender).Left+X-FControlPos.X, TControl(Sender).Top+Y-FControlPos.Y,  FSelectedControl.Width, FSelectedControl.Height );
  DrawFocusRect( DC, FocusRect );
  ReleaseDC( TControl(Sender).Parent.Handle, DC );
  }
end;

procedure TJvgReportEditorForm.OnResize_(Sender: TObject);
begin
  FSkipSizeUpdate := True;
  if Sender = FSelectedControl then
    ShowComponentPos(TControl(Sender));
end;

procedure TJvgReportEditorForm.Read(FileName: string; ParentWnd: TWinControl);
begin
  FScrollBox.HorzScrollBar.Position := 0;
  FScrollBox.VertScrollBar.Position := 0;
  FSelectedControl := nil;
  UpdateToolBar(nil);
  Component.LoadFromFile(FileName);
  Component.CreateReport(ParentWnd, True);
  AssignEventsToAllComponents;
  RemakeComponentsList;
end;

procedure TJvgReportEditorForm.Save(FileName: string);
begin
  FScrollBox.HorzScrollBar.Position := 0;
  FScrollBox.VertScrollBar.Position := 0;
  Component.SaveToFile(FileName);
end;

procedure TJvgReportEditorForm.OpenClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  if OpenDialog1.Execute then
    Read(OpenDialog1.FileName, FScrollBox);
end;

procedure TJvgReportEditorForm.Save1Click(Sender: TObject);
begin
  SaveDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  if SaveDialog1.Execute then
    Save(SaveDialog1.FileName);
end;

procedure TJvgReportEditorForm.ScrollBox_MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Compon: TJvgReportItem;
  R: TRect;
  Pt: TPoint;
begin
  if ssCtrl in Shift then
  begin
    FSelectionRect := Rect(0, 0, 0, 0);
    FSelPt.X := X - FScrollBox.HorzScrollBar.Position;
    FSelPt.Y := Y - FScrollBox.VertScrollBar.Position;
    FSelPt := FScrollBox.ClientToScreen(FSelPt);
    FSelection := True;
  end;
  if B_Label.Down or B_Bevel.Down or sb_OLE.Down then
  begin
    Compon := Component.AddComponent;
    with Compon do
    begin
      Left := X - FScrollBox.HorzScrollBar.Position;
      Top := Y - FScrollBox.VertScrollBar.Position;
      if B_Label.Down then
      begin
        Text := 'Label'; // do not localize
        SideLeft := 0;
        SideTop := 0;
        SideRight := 0;
        SideBottom := 0;
      end;
      OnMouseDown := OnMouseDown_;
      OnMouseUp := OnMouseUp_;
      OnMouseMove := OnMouseMove_;
      OnResize := OnResize_;
      ContainOLE := sb_OLE.Down;
      B_Label.Down := False;
      B_Bevel.Down := False;
      sb_OLE.Down := False;
      RemakeComponentsList;
    end;
  end
  else
  begin
    R := FScrollBox.ClientRect;
    Pt.X := 0;
    Pt.Y := 0;
    Pt := FScrollBox.ClientToScreen(Pt);
    OffsetRect(R, Pt.X, Pt.Y);
    ClipCursor(@R);
  end;
end;

procedure TJvgReportEditorForm.ScrollBox_MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  DC: HDC;
  Pt: TPoint;
begin
  if not FSelection then
    Exit;
  DC := GetDC(0);
  DrawFocusRect(DC, FSelectionRect);
  Pt.X := X - FScrollBox.HorzScrollBar.Position;
  Pt.Y := Y - FScrollBox.VertScrollBar.Position;
  Pt := FScrollBox.ClientToScreen(Pt);
  FSelectionRect := Bounds(Min(FSelPt.X, Pt.X), Min(FSelPt.Y, Pt.Y),
    Abs(FSelPt.X - Pt.X), Abs(FSelPt.Y - Pt.Y));
  DrawFocusRect(DC, FSelectionRect);
  ReleaseDC(0, DC);
end;

procedure TJvgReportEditorForm.ScrollBox_MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  DC: HDC;
  R: TRect;
  I: Integer;
begin
  if FSelection then
  begin
    DC := GetDC(0);
    DrawFocusRect(DC, FSelectionRect);
    ReleaseDC(0, DC);
    FSelection := False;
    //...select all in rect
    with Component.ParentWnd do
      for I := 0 to ControlCount - 1 do
        if Controls[I] is TJvgReportItem then
          with TJvgReportItem(Controls[I]) do
          begin
            R := ClientRect;
            OffsetRect(R, ClientOrigin.X, ClientOrigin.Y);
            Selected := IntersectRect(R, R, FSelectionRect);
          end;
  end;
  R := Rect(0, 0, Screen.Width, Screen.Height);
  ClipCursor(@R);
end;

procedure TJvgReportEditorForm.FormCreate(Sender: TObject);
var
  R: TRect;
begin
  R := Rect(0, 0, Screen.Width, Screen.Height);
  ClipCursor(@R);
  FScrollBox := TJvgReportScrollBox.Create(Self);
  FScrollBox.Align := alClient;
  //  FScrollBox.Color := clWhite;
  ShapeSize.Parent := FScrollBox;
  FScrollBox.OnDraw := OnDrawScrollBox;
  FScrollBox.Parent := P_Main;
  FScrollBox.Tag := 1; //...1 - draw report controls with dot bevel
  FScrollBox.OnMouseDown := ScrollBox_MouseDown;
  //  FScrollBox.VertScrollBar.Tracking := True;
  //  FScrollBox.HorzScrollBar.Tracking := True;
  ShapeSize.Parent := FScrollBox;
  ShapeSize.Left := 0;
  ShapeSize.Top := 0;
  //  LaccReport := TJvgReport.Create;
  FGrid.X := 8;
  FGrid.Y := 8;
  FStep.X := 1;
  FStep.Y := 1;
  {  HRuler := TJvgRuler.Create(Self);
    HRuler.Orientation := goHorizontal;
    HRuler.Parent := P_HRuler;
    HRuler.Top := 0;
    HRuler.Height := P_HRuler.Height;

    VRuler := TJvgRuler.Create(Self);
    VRuler.Orientation := goVertical;
    VRuler.Parent := P_VRuler;
    VRuler.Top := 0;
    VRuler.Width := P_VRuler.Width;}
end;

procedure TJvgReportEditorForm.Edit1Change(Sender: TObject);
begin
  Memo1.Text := TMemo(Sender).Text;
  if Assigned(FSelectedControl) then
    FSelectedControl.Text := TMemo(Sender).Text;
end;

procedure TJvgReportEditorForm.Memo1Change(Sender: TObject);
begin
  Edit1.Text := TMemo(Sender).Text;
  if Assigned(FSelectedControl) then
    FSelectedControl.Text := TMemo(Sender).Text;
end;

procedure TJvgReportEditorForm.SB_LeftClick(Sender: TObject);
begin
  if Assigned(FSelectedControl) then
    with FSelectedControl do
      case TControl(Sender).Tag of
        1:
          SideLeft := 1 - SideLeft;
        2:
          SideRight := 1 - SideRight;
        3:
          SideTop := 1 - SideTop;
        4:
          SideBottom := 1 - SideBottom;
        5:
          begin
            SideLeft := 0;
            SideTop := 0;
            SideRight := 0;
            SideBottom := 0;
            SB_Left.Down := False;
            SB_Top.Down := False;
            SB_Right.Down := False;
            SB_Bottom.Down := False;
          end;
      else
        begin
          SideLeft := 1;
          SideTop := 1;
          SideRight := 1;
          SideBottom := 1;
          SB_Left.Down := True;
          SB_Top.Down := True;
          SB_Right.Down := True;
          SB_Bottom.Down := True;
        end;
      end;
end;

procedure TJvgReportEditorForm.FontComboBox1Change(Sender: TObject);
begin
  // (rom) following two lines disabled
  //if not Assigned(FSelectedControl) then
  //  Exit;
  //-----!  FSelectedControl.FName := TFontComboBox(Sender).FontName;
end;

procedure TJvgReportEditorForm.RxSpinEdit1Change(Sender: TObject);
begin
  if Assigned(FSelectedControl) then
    FSelectedControl.FSize := Trunc(TSpinEdit(Sender).Value);
end;

procedure TJvgReportEditorForm.ColorComboBox1Change(Sender: TObject);
begin
  // (rom) following two lines disabled
  //if not Assigned(FSelectedControl) then
  //  Exit;
  //-----!  FSelectedControl.FColor := TColorComboBox(Sender).ColorValue;
end;

procedure TJvgReportEditorForm.FormShow(Sender: TObject);
begin
  Component.OwnerWnd := Self;
  Component.ParentWnd := FScrollBox;
  if Component.FReportList.Count > 0 then
    Component.CreateReport(FScrollBox, True);
  //  Randomize;
  UpdatePageSize;
  AssignEventsToAllComponents;
  RemakeComponentsList;
  //  ShapeSize.Width := Printer.PageWidth; //f_PrintReport.CBReport1.Width;
  //  ShapeSize.Height := Printer.PageHeight; //f_PrintReport.CBReport1.Height;
end;

procedure TJvgReportEditorForm.sb_BookClick(Sender: TObject);
begin
  {  if TControl(Sender).Tag = 1 then
      f_PrintReport.CBReport1.Orientation := f_PrintReport.PrintWin1.Orientation
      else  f_PrintReport.CBReport1.Orientation := f_PrintReport.PrintWin2.Orientation;}
  if TControl(Sender).Tag = 1 then
    Printer.Orientation := poPortrait
  else
    Printer.Orientation := poLandscape;
  UpdatePageSize;
end;

procedure TJvgReportEditorForm.N1Click(Sender: TObject);
begin
  FScrollBox.RemoveControl(FSelectedControl);
  FScrollBox.InsertControl(FSelectedControl);
end;

procedure TJvgReportEditorForm.sb_FontBoldClick(Sender: TObject);
begin
  if Assigned(FSelectedControl) then
    with FSelectedControl do
      case TControl(Sender).Tag of
        1:
          FStyle := FStyle xor 1;
        2:
          FStyle := FStyle xor 2;
        3:
          FStyle := FStyle xor 4;
      end;
end;

procedure TJvgReportEditorForm.sb_AlignLClick(Sender: TObject);
begin
  if Assigned(FSelectedControl) then
    FSelectedControl.Alignment := TControl(Sender).Tag;
end;

procedure TJvgReportEditorForm.sbFontColorClick(Sender: TObject);
var
  I: Integer;
begin
  if not Assigned(FSelectedControl) then
    Exit;
  with ColorDialog1 do
  begin
    case TControl(Sender).Tag of
      0:
        Color := FSelectedControl.FColor;
      1:
        Color := FSelectedControl.BkColor;
    else
      Color := FSelectedControl.BvColor;
    end;

    if Execute then
      for I := 0 to FScrollBox.ControlCount - 1 do
        if FScrollBox.Controls[I] is TJvgReportItem then
          with TJvgReportItem(FScrollBox.Controls[I]) do
            if TJvgReportItem(FScrollBox.Controls[I]).Selected then
              case TControl(Sender).Tag of
                0:
                  TJvgReportItem(FScrollBox.Controls[I]).FColor := Color;
                1:
                  TJvgReportItem(FScrollBox.Controls[I]).BkColor := Color;
              else
                TJvgReportItem(FScrollBox.Controls[I]).BvColor := Color;
              end;
  end;
end;

procedure TJvgReportEditorForm.N_DeleteObjectClick(Sender: TObject);
begin
  if Assigned(FSelectedControl) then
  begin
    if Windows.MessageBox(0, PChar(RsDeleteObject), PChar(RsConfirm), MB_OKCANCEL) <> IDOK then
      Exit;
    if FSelectedControl.ContainOLE then
      FScrollBox.RemoveControl(FSelectedControl.OLEContainer);
    FScrollBox.RemoveControl(FSelectedControl);
    FSelectedControl.Free;
    FSelectedControl := nil;
    RemakeComponentsList;
  end;
end;

procedure TJvgReportEditorForm.OnDrawScrollBox(Sender: TObject);
begin
  VRuler.Top := ShapeSize.Top;
  HRuler.Left := ShapeSize.Left + P_VRuler.Width;
end;

procedure TJvgReportEditorForm.FormDestroy(Sender: TObject);
begin
  //...
end;

procedure TJvgReportEditorForm.RemakeComponentsList;
var
  I: Integer;
begin
  cb_Components.Items.Clear;
  for I := 0 to FScrollBox.ControlCount - 1 do
    if FScrollBox.Controls[I] is TJvgReportItem then
      cb_Components.Items.Add(TJvgReportItem(FScrollBox.Controls[I]).CompName);
  cb_Components.Text := '';
  lb_Params.Items.Clear;
  for I := 0 to Component.ParamNames.Count - 1 do
    lb_Params.Items.Add(Component.ParamNames[I]);
end;

procedure TJvgReportEditorForm.UpdatePageSize;
const
  Sizes: array [Boolean, 1..2] of Integer =
    ((21, 29), (29, 21));
begin
  ShapeSize.Width := Round(Sizes[Printer.Orientation = poLandscape][1] *
    GetDeviceCaps(Canvas.Handle, LOGPIXELSX) * 1.541 * 2.54 / 10);
  ShapeSize.Height := Round(Sizes[Printer.Orientation = poLandscape][2] *
    GetDeviceCaps(Canvas.Handle, LOGPIXELSY) * 1.541 * 2.54 / 10);
  HRuler.Width := ShapeSize.Width + 10;
  VRuler.Height := ShapeSize.Height + 10;
end;

procedure TJvgReportEditorForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  l, t, w, h: Integer;
begin
  w := 0;
  h := 0;
  if (Shift = [ssCtrl]) and (Chr(Key) = 'Z') and FCanUndo then
  begin
    FCanUndo := False;
    ResizeReportControls(FUndoPosShift.X, FUndoPosShift.Y, 0, 0, True);
  end;

  if Assigned(ActiveControl) then
    Exit;
  l := 0;
  t := 0;
  case Key of
    VK_UP:
      if Shift = [ssShift] then
        h := -1
      else
      if Shift = [ssCtrl] then
        t := -1;
    VK_DOWN:
      if Shift = [ssShift] then
        h := 1
      else
      if Shift = [ssCtrl] then
        t := 1;
    VK_LEFT:
      if Shift = [ssShift] then
        w := -1
      else
      if Shift = [ssCtrl] then
        l := -1;
    VK_RIGHT:
      if Shift = [ssShift] then
        w := 1
      else
      if Shift = [ssCtrl] then
        l := 1;
  else
    Exit;
  end;
  ResizeReportControls(l, t, w, h, True);

end;

procedure TJvgReportEditorForm.sb_FixMovingClick(Sender: TObject);
var
  I: Integer;
begin
  with FScrollBox do
    for I := 0 to ControlCount - 1 do
      if (Controls[I] is TJvgReportItem) and
        TJvgReportItem(Controls[I]).Selected then
        TJvgReportItem(Controls[I]).Fixed := Ord(sb_FixMoving.Down);
end;

procedure TJvgReportEditorForm.N_AutoSizeClick(Sender: TObject);
begin
  FSelectedControl.OLESizeMode := TMenuItem(Sender).Tag;
end;

procedure TJvgReportEditorForm.sb_SnapToGridClick(Sender: TObject);
begin
  if sb_SnapToGrid.Down then
  begin
    FStep.X := FGrid.X;
    FStep.Y := FGrid.Y;
  end
  else
  begin
    FStep.X := 1;
    FStep.Y := 1;
  end;
end;

procedure TJvgReportEditorForm.se_SizeChange(Sender: TObject);
begin
  if (not FMouseDown) and (not FSkipSizeUpdate) then
    ResizeReportControls(se_Left.Value, IGNORE_VALUE,
      IGNORE_VALUE, IGNORE_VALUE, False {fUseParamsAsShifts});
  ShowComponentPos(FSelectedControl);
  FSkipSizeUpdate := False;
end;

procedure TJvgReportEditorForm.se_TopChange(Sender: TObject);
begin
  if (not FMouseDown) and (not FSkipSizeUpdate) then
    ResizeReportControls(IGNORE_VALUE, se_Top.Value,
      IGNORE_VALUE, IGNORE_VALUE, False {fUseParamsAsShifts});
  ShowComponentPos(FSelectedControl);
  FSkipSizeUpdate := False;
end;

procedure TJvgReportEditorForm.se_WidthChange(Sender: TObject);
begin
  if (not FMouseDown) and (not FSkipSizeUpdate) then
    ResizeReportControls(IGNORE_VALUE, IGNORE_VALUE,
      se_Width.Value, IGNORE_VALUE, False {fUseParamsAsShifts});
  ShowComponentPos(FSelectedControl);
  FSkipSizeUpdate := False;
end;

procedure TJvgReportEditorForm.se_HeightChange(Sender: TObject);
begin
  if (not FMouseDown) and (not FSkipSizeUpdate) then
    ResizeReportControls(IGNORE_VALUE, IGNORE_VALUE,
      IGNORE_VALUE, se_Height.Value, False {fUseParamsAsShifts});
  ShowComponentPos(FSelectedControl);
  FSkipSizeUpdate := False;
end;

procedure TJvgReportEditorForm.ResizeReportControls(l, t, w, h: Integer;
  fUseParamsAsShifts: Boolean);
var
  I: Integer;
begin
  with FScrollBox do
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TJvgReportItem then
        with TJvgReportItem(Controls[I]) do
          if Selected and (Fixed = 0) then
            if fUseParamsAsShifts then
            begin
              if l < IGNORE_VALUE then
                Left := Left + l;
              if t < IGNORE_VALUE then
                Top := Top + t;
              if w < IGNORE_VALUE then
                Width := Width + w;
              if h < IGNORE_VALUE then
                Height := Height + h;
            end
            else
            begin
              if l < IGNORE_VALUE then
                Left := l;
              if t < IGNORE_VALUE then
                Top := t;
              if w < IGNORE_VALUE then
                Width := w;
              if h < IGNORE_VALUE then
                Height := h;
            end;
end;

procedure TJvgReportEditorForm.cb_ComponentsChange(Sender: TObject);
var
  I: Integer;
begin
  if Assigned(FSelectedControl) then
    if FSelectedControl.CompName = cb_Components.Text then
      Exit;
  with FScrollBox do
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TJvgReportItem then
        if TJvgReportItem(Controls[I]).CompName = cb_Components.Text then
        begin
          OnMouseDown_(Controls[I], mbLeft, [], 0, 0);
          OnMouseUp_(Controls[I], mbLeft, [], 0, 0);
          Exit;
        end;
end;

procedure TJvgReportEditorForm.ShowComponentPos(Control: TControl);
begin
  if Component <> nil then
  begin
    se_Left.Value := Control.Left;
    se_Top.Value := Control.Top;
    se_Width.Value := Control.Width;
    se_Height.Value := Control.Height;
  end;
end;

procedure TJvgReportEditorForm.AssignEventsToAllComponents;
var
  I: Integer;
begin
  with Component.ParentWnd do
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TJvgReportItem then
        with TJvgReportItem(Controls[I]) do
        begin
          OnMouseDown := OnMouseDown_;
          OnMouseUp := OnMouseUp_;
          OnMouseMove := OnMouseMove_;
          OnResize := OnResize_;
        end;
end;

function CanAlignControl(Control: TControl): Boolean;
begin
  Result := (Control is TJvgReportItem) and
    TJvgReportItem(Control).Selected and
    (TJvgReportItem(Control).Fixed = 0);
end;

procedure TJvgReportEditorForm.N4Click(Sender: TObject);
begin
  with TAlignForm.Create(Self) do
    try
      if ShowModal = mrOk then
        AlignControlsInWindow(Component.ParentWnd, CanAlignControl,
          Horz, Vert);
    finally
      Free;
    end;
end;

procedure TJvgReportEditorForm.sb_BevelBoldClick(Sender: TObject);
var
  I: Integer;
begin
  with Component.ParentWnd do
    for I := 0 to ControlCount - 1 do
      if (Controls[I] is TJvgReportItem) and
        TJvgReportItem(Controls[I]).Selected then
        TJvgReportItem(Controls[I]).PenWidth := 1 + Ord(sb_BevelBold.Down);
end;

procedure TJvgReportEditorForm.UpdateToolBar(Control: TJvgReportItem);
begin
  with Control do
  begin
    {
      se_Left.Enabled := Assigned(Control);
      se_Top.Enabled := Assigned(Control);
      se_Width.Enabled := Assigned(Control);
      se_Height.Enabled := Assigned(Control);}
    P_Sides.Enabled := Assigned(Control);
    P_Font.Enabled := Assigned(Control);
    P_SBar.Enabled := Assigned(Control);
    if not Assigned(Control) then
      Exit;
    Edit1.Text := Text;
    Memo1.Text := Text;
    FontComboBox1.Text := FName;
    RxSpinEdit1.Value := FSize;
    ColorComboBox1.Color := FColor;

    SB_Left.Down := SideLeft = 1;
    SB_Top.Down := SideTop = 1;
    SB_Right.Down := SideRight = 1;
    SB_Bottom.Down := SideBottom = 1;
    case Alignment of
      1:
        sb_AlignL.Down := True;
      2:
        sb_AlignR.Down := True;
      3:
        sb_AlignC.Down := True;
      4:
        sb_AlignW.Down := True;
    end;
    sb_FixMoving.Down := Fixed <> 0;
    sb_FontBold.Down := (FStyle and 1) <> 0;
    sb_FontItalic.Down := (FStyle and 2) <> 0;
    sb_FontUnderline.Down := (FStyle and 4) <> 0;
    FE_OLE.Text := OLELinkToFile;
    FE_OLE.Enabled := ContainOLE;
    sb_BevelBold.Down := (PenWidth - 1) <> 0;
    FSkipSizeUpdate := True;
    ShowComponentPos(Control);

    cb_Components.Text := CompName;
  end;
end;

procedure TJvgReportEditorForm.FormClose(Sender: TObject; var Action:
  TCloseAction);
begin
  FReportParamEditor.Free;
  FScrollBox.HorzScrollBar.Position := 0;
  FScrollBox.VertScrollBar.Position := 0;
  Component.Save;
end;

procedure TJvgReportEditorForm.FE_OLEChange(Sender: TObject);
var
  S: string;
begin
  if (not Assigned(FSelectedControl)) or (not FileExists(FE_OLE.Text)) then
    Exit;
  S := FE_OLE.Text;
  if ExtractFilePath(Name) = ExtractFilePath(ParamStr(0)) then
    S := ExtractFileName(Name);
  if FSelectedControl.OLELinkToFile <> S then
    FSelectedControl.OLELinkToFile := S;
end;

procedure TJvgReportEditorForm.SpeedButton1Click(Sender: TObject);
begin
  if OpenOLEFile.Execute then
    FE_OLE.Text := OpenOLEFile.FileName;
end;

procedure TJvgReportEditorForm.sb_PrintClick(Sender: TObject);
begin
  if Assigned(Component) and (Component.ComponentList.Count > 0) then
    Component.Print;
  Component.OwnerWnd := Self;
  Component.ParentWnd := FScrollBox;
end;

procedure TJvgReportEditorForm.sb_PreviewClick(Sender: TObject);
var
  Form: TForm;
  Image: TImage;
  Bmp: TBitmap;
  R: TRect;
  I, W, H: Integer;
begin
  if not Assigned(Component) then
    Exit;
  Form := TForm.Create(nil);
  Form.Caption := RsPagePreview;
  Image := TImage.Create(Form);
  Bmp := TBitmap.Create;
  Image.Parent := Form;
  H := CentimetersToPixels(Form.Canvas.Handle, 29, True);
  W := CentimetersToPixels(Form.Canvas.Handle, 21, False);
  //  Image.Width := W+8;
  //  Image.Height := H+8;
  Image.Left := 0;
  Image.Top := 0;
  Bmp.Width := W + 7;
  Bmp.Height := H + 7;
  try
    //    Bmp.Canvas.Brush.Color := clWhite;
    //    R := Image.ClientRect; Bmp.Canvas.FillRect( R );

    with Component do
      for I := 0 to ComponentList.Count - 1 do
        with TJvgReportItem(ComponentList[I]) do
        begin
          PaintTo(Bmp.Canvas);
          if ContainOle then
            OLEContainer.PaintTo(Bmp.Canvas.Handle, Left, Top);
        end;

    Bmp.Canvas.Brush.Color := clBtnFace;
    R := Bounds(Bmp.Width - 7, 0, 7, Bmp.Height - 7);
    Bmp.Canvas.FillRect(R);
    R := Bounds(0, Bmp.Height - 7, Bmp.Width - 7, 7);
    Bmp.Canvas.FillRect(R);
    Bmp.Canvas.Brush.Color := 0;
    R := Bounds(Bmp.Width - 7, 7, 7, Bmp.Height - 7);
    Bmp.Canvas.FillRect(R);
    R := Bounds(7, Bmp.Height - 7, Bmp.Width - 7, 7);
    Bmp.Canvas.FillRect(R);

    Image.Picture.Bitmap := Bmp;
    Image.Stretch := True;
    Image.Width := W div 2;
    Image.Height := H div 2;
    Form.ClientWidth := Image.Width;
    Form.ClientHeight := Image.Height;

    Bmp.Free;
    Bmp := nil;
    Form.ShowModal;
  finally
    Bmp.Free;
    Form.Free;
  end;
end;

procedure TJvgReportEditorForm.SpeedButton2Click(Sender: TObject);
begin
  if not Assigned(FReportParamEditor) then
    FReportParamEditor := TJvgReportParamEditor.Create(nil);
  FReportParamEditor.ShowModal;
end;

procedure TJvgReportEditorForm.cb_ComponentsKeyDown(Sender: TObject; var Key:
  Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    FSelectedControl.CompName := Trim(cb_Components.Text);
    RemakeComponentsList;
    cb_Components.Text := FSelectedControl.CompName;
  end;
  if Key = VK_ESCAPE then
    cb_Components.Text := FSelectedControl.CompName;
end;

procedure TJvgReportEditorForm.CheckBox1Click(Sender: TObject);
begin
  if Assigned(FSelectedControl) then
    FSelectedControl.Transparent := Ord(TCheckBox(Sender).Checked);
end;

end.
